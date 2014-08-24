module Database.Blacktip () where

import qualified Control.Concurrent        as CC
import qualified Control.Concurrent.MVar   as MV
import qualified Control.Exception         as E
import qualified Data.Binary.Put           as BinP
import qualified Data.ByteString.Char8     as B
import qualified Data.Int                  as DI
import qualified Data.Locator              as DL
import qualified Data.Time.Clock.POSIX     as PSX
import qualified Filesystem                as FS
import qualified Filesystem.Path.CurrentOS as FPC
import qualified Network.Info              as NI
import qualified Safe
import Control.Monad (forever, when)
import Data.Bits
import Data.Word
import Database.Blacktip.Types

-- There are only supposed to be one of these
-- babies running per node (MAC address)!
getInterfaceByName :: InterfaceName -> IO (Maybe NI.NetworkInterface)
getInterfaceByName n = fmap (Safe.headMay
                             . filter ((n ==)
                             . NI.name))
                       NI.getNetworkInterfaces

getMac :: Interface -> IO (Maybe NI.MAC)
getMac iface = case iface of
  NIInterface iface -> return $ Just (NI.mac iface)
  IName       name  -> (fmap . fmap) NI.mac (getInterfaceByName name)

getUnixMillis :: IO Milliseconds
getUnixMillis = fmap (round . (*1000)) PSX.getPOSIXTime

serverState :: IO (MV.MVar ServerState)
serverState = MV.newEmptyMVar

readTimestamp :: FPC.FilePath -> IO (Either ParseError Integer)
readTimestamp path = do
  result <- E.try $ FS.readFile path :: IO (Either IOError B.ByteString)
  case result of
   Left  _   -> return $ Right 0
   Right val -> case possibleParse of
     Nothing  -> return $ Left ParseError
     Just num -> return $ Right num
     where possibleParse = (Safe.readMay . B.unpack) val

writeTimestamp :: MV.MVar ServerState -> FPC.FilePath -> IO CC.ThreadId
writeTimestamp s path = do
  putStrLn "writer started"
  CC.forkIO go
  where go = forever $ do
          ss <- MV.takeMVar s
          FS.writeFile path (B.pack (show (ssTime ss)))
          -- sleep for 1 second
          CC.threadDelay 1000000

arrowOfTimeError :: (Show a, Show b) => a -> b -> c
arrowOfTimeError ts stateTime =
  error ("ERROR ARROW OF TIME BACKWARDS - Had timestamp: "
         ++ show ts
         ++ " and state time: "
         ++ show stateTime)

bumpItYo :: Milliseconds -> ServerState -> ServerState
bumpItYo ts s
  | ts == stateTime = s { ssSequence = (+1) stateSeq }
  | ts >  stateTime = s { ssSequence = 0 }
  | otherwise       = (arrowOfTimeError ts stateTime)
  where stateTime = ssTime s
        stateSeq  = ssSequence s

-- readFile try $ FS.readFile (FPC.decodeString "lol") :: IO (Either IOException ByteString)
-- writeFile config = FPC.writeFile (timestampPath config)

generateUniqueId :: Config -> IO (Either NoInterfaceError UniqueId)
generateUniqueId config = do
  ms <- getUnixMillis
  ss <- serverState
  empty <- MV.isEmptyMVar ss
  _ <- when empty (initWriter (timestampPath config))
  s <- MV.takeMVar ss
  let seq = ssSequence $ bumpItYo ms s
  return $ Right 0
  where mac = (getMac . interface) config

initWriter :: FPC.FilePath -> IO ()
initWriter path = do
  ms <- getUnixMillis
  st <- readTimestamp path
  case st of
    Left ParseError -> error ("Path for timestamp file: "
                              ++ show path
                              ++ " could not be parsed as a timestamp.")
    Right stateTime -> do
      _ <- when ((toInteger ms) < stateTime) (arrowOfTimeError ms stateTime)
      ss <- serverState
      _ <- writeTimestamp ss path 
      return ()

bitRange n lo hi = reverse (map (testBit n) [lo..hi])
-- bits n = bitRange n 0 (bitSize n - 1)
showBits n lo hi = map (\b -> if b then '1' else '0') (bitRange n lo hi)
-- showBits (shift (toInteger ms) 64) 0 128

binnify ms (NI.MAC a b c d e f) sq = withSq
  where withTimestamp = shift (toInteger ms) 64
        withMacA = shift (withTimestamp .|. toInteger a)  56
        withMacB = shift (withMacA      .|. toInteger b)  48
        withMacC = shift (withMacB      .|. toInteger c)  40
        withMacD = shift (withMacC      .|. toInteger d)  32
        withMacE = shift (withMacD      .|. toInteger e)  24
        withMacF = shift (withMacE      .|. toInteger f)  16
        withSq   = withMacF             .|. toInteger sq

putify :: Milliseconds -> NI.MAC -> DI.Int16 -> BinP.Put
putify ms (NI.MAC a b c d e f) sq = do
  let ts = fromIntegral ms :: Word64
  let counter = fromIntegral sq :: Word16
  -- 64 bits of timestamp
  BinP.putWord64be ts
  -- 48 bits of MAC address
  BinP.putWord8 a
  BinP.putWord8 b
  BinP.putWord8 c
  BinP.putWord8 d
  BinP.putWord8 e
  BinP.putWord8 f
  -- 16 bits of counter (sequence)
  BinP.putWord16be counter

-- iface <- getInterfaceByName "wlan0"
-- let muhMac = NI.mac $ fromJust iface
-- ms <- getUnixMillis
-- BinP.runPut (putify ms muhMac 0)
-- import qualified Data.ByteString.Lazy.Char8     as BL
-- BL.readInteger

-- writeTimestamp config =

-- *Data.Blacktip DL> DL.toBase62 1000000000000000000000000000
-- "1IdHllabYuAOlNK4"
-- *Data.Blacktip DL DW> DW.Int128 0 10
-- 10
-- *Data.Blacktip DL DW> toInteger $ DW.Int128 0 10
-- 10
-- *Data.Blacktip DL DW SIM> SIM.macs
-- [3c:15:c2:d9:bf:c2]
-- *Data.Blacktip DL DW SIM> SIM.mac
-- Just 3c:15:c2:d9:bf:c2
-- *Data.Blacktip> fmap (round . (*1000)) getPOSIXTime 
-- 1408837883771
-- import Data.Int
-- *Data.Blacktip> fmap (round . (*1000)) getPOSIXTime :: IO Int64
-- 1408838053322
-- *Data.Blacktip> SIM.nics
-- [("wlan0",3c:15:c2:d9:bf:c2)]

-- ONE INSTANCE PER SERVER!
-- This is a port of Boundary's "flake", an Erlang unique ID service. The format
-- of the IDs is as follows:

--   64 bits - timestamp
--   48 bits - id (i.e. MAC address)
--   16 bits - sequence
