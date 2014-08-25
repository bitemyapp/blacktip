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
import System.IO.Unsafe (unsafePerformIO)

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

-- We don't want multiple of these running around via inlining
{-# NOINLINE serverState #-}
serverState :: MV.MVar ServerState
-- unsafePerformIO so it doesn't create an
-- emptyMVar when I bind against serverState
serverState = unsafePerformIO $ MV.newEmptyMVar

readTimestamp :: FPC.FilePath -> IO Int
readTimestamp path = do
  result <- FS.readFile path
  return $ (read . B.unpack) result

writeTimestamp :: MV.MVar ServerState -> FPC.FilePath -> IO CC.ThreadId
writeTimestamp s path = do
  CC.forkIO go
  where go = forever $ do
          ss <- MV.readMVar s
          FS.writeFile path (B.pack (show (ssTime ss)))
          -- sleep for 1 second
          CC.threadDelay 1000000

arrowOfTimeError :: (Show a, Show b) => a -> b -> c
arrowOfTimeError ts stateTime =
  error ("ERROR ARROW OF TIME BACKWARDS - Had timestamp: "
         ++ show ts
         ++ " and state time: "
         ++ show stateTime)

-- bumpItYo :: Monad m => Milliseconds -> ServerState -> m (ServerState, ServerState)
-- bumpItYo ts s
--   | ts == stateTime = return (s, s { ssSequence = (+1) stateSeq })
--   | ts >  stateTime = return (s, s { ssSequence = 0 })
--   | otherwise       = return (s, arrowOfTimeError ts stateTime)
--   where stateTime = ssTime s
--         stateSeq  = ssSequence s

bumpItYo :: Milliseconds -> ServerState -> ServerState
bumpItYo ts s
  | ts == stateTime = s { ssSequence = (+1) stateSeq }
  | ts >  stateTime = s { ssSequence = 0 }
  | otherwise       = arrowOfTimeError ts stateTime
  where stateTime = ssTime s
        stateSeq  = ssSequence s

-- readFile try $ FS.readFile (FPC.decodeString "lol") :: IO (Either IOException ByteString)
-- writeFile config = FPC.writeFile (timestampPath config)

generateUniqueId :: Config -> IO (Either NoInterfaceError UniqueId)
generateUniqueId config = do
  -- millis <- getUnixMillis
  millis <- readTimestamp path
  empty <- MV.isEmptyMVar serverState
  _ <- when empty (initWriter (timestampPath config))
  -- newState <- MV.modifyMVar ss (bumpItYo ms)
  mState <- MV.takeMVar serverState
  let newState = bumpItYo millis mState
  _ <- MV.putMVar serverState newState
  let sSeq = ssSequence $ newState
  mMac <- (getMac . interface) config
  case mMac of
   Nothing  -> return $ Left NoInterfaceError
   Just mac -> return $ Right (binnify millis mac sSeq)
  where path = timestampPath config

initWriter :: FPC.FilePath -> IO ()
initWriter path = do
  ms <- getUnixMillis
  stateTime <- readTimestamp path
  _  <- when (ms < stateTime) (arrowOfTimeError ms stateTime)
  _  <- MV.putMVar serverState (ServerState ms 0)
  _  <- writeTimestamp serverState path
  return ()

bitRange :: Bits a => a -> Int -> Int -> [Bool]
bitRange n lo hi = reverse (map (testBit n) [lo..hi])
showBits :: Bits a => a -> Int -> Int -> [Char]
showBits n lo hi = map (\b -> if b then '1' else '0') (bitRange n lo hi)

binnify :: Milliseconds -> NI.MAC -> Sequence -> Integer
binnify ms (NI.MAC a b c d e f) sq = withSq
  where withTimestamp = shift (toInteger ms) 64
        withMacA = shift (toInteger a) 56 .|. withTimestamp
        withMacB = shift (toInteger b) 48 .|. withMacA
        withMacC = shift (toInteger c) 40 .|. withMacB
        withMacD = shift (toInteger d) 32 .|. withMacC
        withMacE = shift (toInteger e) 24 .|. withMacD
        withMacF = shift (toInteger f) 16 .|. withMacE
        withSq   = withMacF               .|. toInteger sq

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
