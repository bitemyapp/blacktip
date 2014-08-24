module Database.Blacktip.Types
       ( Config(..)
       , Interface(..)
       , ParseError(..)
       , ServerState(..)
       , InterfaceName
       , Milliseconds
       , UniqueId
       , defaultConfig
         )
       where

import qualified Data.Int                  as DI
import qualified Data.Word                 as W
import qualified Filesystem.Path.CurrentOS as FPC
import qualified Network.Info              as NI

type InterfaceName = String

data Interface = NIInterface NI.NetworkInterface
               | IName       InterfaceName
                 deriving Show

type Milliseconds = Int

data Config =
  Config { interface         :: Interface
         , timestampPath     :: FPC.FilePath
         , allowableDowntime :: Milliseconds }
  deriving Show

defaultConfig :: Config
defaultConfig =
  Config { interface = IName "eth0"
         , timestampPath = FPC.decodeString "/tmp/blacktip-timestamp-dets"
           -- 720 hours.
         , allowableDowntime = 2592000000 }
                      

data ServerState =
  ServerState { ssTime     :: Milliseconds
              , ssSequence :: DI.Int16 }
  deriving Show

-- {64 bit timestamp, 48 bit id (MAC address), 16 bit sequence}
type UniqueId = Integer

-- if the data in the file doesn't parse
data ParseError = ParseError deriving Show
