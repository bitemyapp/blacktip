module Database.Blacktip
       ( generateUniqueId
       , getInterfaceByName
       , toBase62
         ) where

import qualified Control.Concurrent        as CC
import qualified Control.Concurrent.MVar   as MV
import qualified Data.ByteString.Char8     as B
import qualified Data.Locator              as DL
import qualified Data.Time.Clock.POSIX     as PSX
import qualified Filesystem                as FS
import qualified Filesystem.Path.CurrentOS as FPC
import qualified Network.Info              as NI
import qualified Safe
import Control.Monad (forever, when)
import Data.Bits
import Database.Blacktip.Types
import System.IO.Unsafe (unsafePerformIO)

-- There are only supposed to be one of these
-- babies running per node (MAC address)!
getInterfaceByName :: InterfaceName -> IO (Maybe NI.NetworkInterface)
getInterfaceByName n = fmap (Safe.headMay
                             . filter ((n ==)
                             . NI.name))
                       NI.getNetworkInterfaces

toBase62 :: Integer -> String
toBase62 = DL.toBase62

getMac :: Interface -> IO (Maybe NI.MAC)
getMac iface = case iface of
  NIInterface interf -> return $ Just (NI.mac interf)
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

bumpItYo :: Milliseconds -> ServerState -> ServerState
bumpItYo ts s
  | ts == stateTime = s { ssSequence = (+1) stateSeq }
  | ts >  stateTime = s { ssTime = ts, ssSequence = 0 }
  | otherwise       = arrowOfTimeError ts stateTime
  where stateTime = ssTime s
        stateSeq  = ssSequence s

-- readFile try $ FS.readFile (FPC.decodeString "lol") :: IO (Either IOException ByteString)
-- writeFile config = FPC.writeFile (timestampPath config)

generateUniqueId :: Config -> IO (Either NoInterfaceError UniqueId)
generateUniqueId config = do
  millis <- getUnixMillis
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

initWriter :: FPC.FilePath -> IO ()
initWriter path = do
  ms <- getUnixMillis
  stateTime <- readTimestamp path
  _  <- when (ms <= stateTime) (arrowOfTimeError ms stateTime)
  _  <- MV.putMVar serverState (ServerState ms 0)
  _  <- writeTimestamp serverState path
  return ()

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
