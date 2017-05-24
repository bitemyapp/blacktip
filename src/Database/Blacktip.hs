module Database.Blacktip
       ( module Database.Blacktip.Types
       , generateUniqueId
       , generateUniqueId'
       , getInterfaceByName
       , getUnixMillis
       , integerToRecord
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
import Control.Exception (mask, try)
import Control.Monad (forever, when)
import Data.Bits
import Data.Bits.Bitwise (fromListBE)
import Data.List.Split (chunksOf)
import Database.Blacktip.Types
import System.Clock
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

-- getUnixMillisMonotonic :: IO Milliseconds
-- getUnixMillisMonotonic = fmap (round . (*1000)) PSX.getPOSIXTime

-- We don't want multiple of these running around via inlining
{-# NOINLINE serverState #-}
serverState :: MV.MVar ServerState
-- unsafePerformIO so it doesn't create an
-- emptyMVar when I bind against serverState
-- Don't complain to me about the unsafePerformIO, I know what I'm doing.
-- http://www.reddit.com/r/haskell/comments/2jbl78/from_60_frames_per_second_to_500_in_haskell/claf6hg?context=3
serverState = unsafePerformIO MV.newEmptyMVar

readTimestamp :: FPC.FilePath -> IO Int
readTimestamp path = do
  result <- try $ FS.readFile path :: IO (Either IOError B.ByteString)
  case result of
   (Right "") -> return 0
   _          -> return $ either (const 0) (read . B.unpack) result

writeTimestamp :: MV.MVar ServerState -> FPC.FilePath -> IO CC.ThreadId
writeTimestamp s path = CC.forkIO go
  where go = forever $ do
          ss <- MV.readMVar s
          mask $ \_ -> FS.writeFile path (B.pack (show (ssTime ss)))
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

generateUniqueId' :: Config -> IO (Either NoInterfaceError
                                  (UniqueId, IdentityRecord))
generateUniqueId' config = do
  millis <- getUnixMillis
  empty <- MV.isEmptyMVar serverState
  _ <- when empty (initWriter (timestampPath config))
  -- newState <- MV.modifyMVar ss (bumpItYo ms)
  mState <- MV.takeMVar serverState
  let newState = bumpItYo millis mState
  _ <- MV.putMVar serverState newState
  let sSeq = ssSequence newState
  mMac <- (getMac . interface) config
  case mMac of
   Nothing  -> return $ Left NoInterfaceError
   Just mac -> return $ Right (binnify millis mac sSeq, IdentityRecord millis mac sSeq)

generateUniqueId :: Config -> IO (Either NoInterfaceError UniqueId)
generateUniqueId config = do
  uid <- generateUniqueId' config
  return $ fmap fst uid

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

-- works with Integer since I am providing range, big-endian
bitRange :: Bits a => a -> Int -> Int -> [Bool]
bitRange n lo hi = reverse $ map (testBit n) [lo..hi]

integerToRecord :: Integer -> IdentityRecord
integerToRecord n = IdentityRecord milliseconds mac recSequence
                    -- extract 128 bits.
  where extractedBits = bitRange n 0 127
        milliBits     = take 64 extractedBits
        macBits       = chunksOf 8 $ take 48 $ drop 64 extractedBits
        sequenceBits  = take 16 $ drop 112 extractedBits
        milliseconds  = fromListBE milliBits :: Milliseconds
        a             = fromListBE (head macBits)
        b             = fromListBE (macBits !! 1)
        c             = fromListBE (macBits !! 2)
        d             = fromListBE (macBits !! 3)
        e             = fromListBE (macBits !! 4)
        f             = fromListBE (macBits !! 5)
        mac           = NI.MAC a b c d e f
        recSequence   = fromListBE sequenceBits :: Sequence
