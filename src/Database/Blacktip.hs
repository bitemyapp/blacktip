module Database.Blacktip () where

import qualified Control.Concurrent.MVar   as MV
import qualified Control.Exception         as E
import qualified Data.Binary.Put           as BinP
import qualified Data.Bits                 as Bits
import qualified Data.ByteString.Char8     as B
import qualified Data.Int                  as DI
import qualified Data.Locator              as DL
import qualified Data.Time.Clock.POSIX     as PSX
import qualified Filesystem                as FS
import qualified Filesystem.Path.CurrentOS as FPC
import qualified Network.Info              as NI
import qualified Safe
import Data.Word
import Database.Blacktip.Types

-- There are only supposed to be one of these
-- babies running per node (MAC address)!
getInterfaceByName :: InterfaceName -> IO (Maybe NI.NetworkInterface)
getInterfaceByName n = fmap (Safe.headMay
                             . filter ((n ==)
                             . NI.name))
                       NI.getNetworkInterfaces

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

writeTimestamp s path = undefined

bumpItYo s ts = undefined

-- readFile try $ FS.readFile (FPC.decodeString "lol") :: IO (Either IOException ByteString)
-- writeFile config = FPC.writeFile (timestampPath config)

generateUniqueId :: Config -> UniqueId
generateUniqueId = blah
  where blah = undefined

bitRange n lo hi = reverse (map (Bits.testBit n) [lo..hi])
-- bits n = bitRange n 0 (Bits.bitSize n - 1)
showBits n lo hi = map (\b -> if b then '1' else '0') (bitRange n lo hi)
-- showBits (shift (toInteger ms) 64) 0 128

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

-- BinP.runPut (putify ms muhMac 0)
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
