module Data.Blacktip () where

import qualified Control.Concurrent.MVar   as MV
import qualified Data.DoubleWord           as DW
import qualified Data.Int                  as DI
import qualified Data.Locator              as DL
import qualified Data.Time.Clock.POSIX     as PSX
import qualified Filesystem                as FS
import qualified Filesystem.Path.CurrentOS as FPC
import qualified Network.Info              as NI
import qualified Safe

-- There are only supposed to be one of these
-- babies running per node (MAC address)!
type InterfaceName = String
getInterfaceByName :: InterfaceName -> IO (Maybe NI.NetworkInterface)
getInterfaceByName n = fmap (Safe.headMay
                             . filter ((n ==)
                             . NI.name))
                       NI.getNetworkInterfaces

data Interface = NIInterface NI.NetworkInterface
               | IName       InterfaceName
                 deriving Show

type Milliseconds = Int
data Config =
  Config { interface         :: Interface
         , timestampPath     :: FPC.FilePath
         , allowableDowntime :: Milliseconds }
  deriving Show

data ServerState =
  ServerState { time     :: UnixMillis
              , sequence :: DI.Int16 }
  deriving Show

newtype UnixMillis = UnixMillis Integer deriving Show

getUnixMillis :: IO UnixMillis
getUnixMillis = fmap (UnixMillis . round . (*1000)) PSX.getPOSIXTime

-- {64 bit timestamp, 48 bit id (MAC address), 16 bit sequence}
type UniqueId = DW.Int128

generateUniqueId :: Config -> UniqueId
generateUniqueId = blah
  where blah = undefined

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
