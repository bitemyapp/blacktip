module Database.Blacktip.Types
       ( Config(..)
       , Interface(..)
       , ServerState(..)
       , ArrowOfTimeError(..)
       , NoInterfaceError(..)
       , ParseError(..)
       , IdentityRecord(..)
       , InterfaceName
       , Milliseconds
       , Sequence
       , UniqueId
       , defaultConfig
         )
       where

import qualified Data.Int                  as DI
import qualified Filesystem.Path.CurrentOS as FPC
import qualified Network.Info              as NI

type InterfaceName = String

data Interface = NIInterface NI.NetworkInterface
               | IName       InterfaceName
                 deriving Show

type Milliseconds = Int
type Sequence     = DI.Int16

data Config =
  Config { interface         :: Interface
         , timestampPath     :: FPC.FilePath
         , allowableDowntime :: Milliseconds }
  deriving Show

defaultConfig :: Config
defaultConfig =
  Config { interface = IName "eth0"
         , timestampPath = FPC.decodeString "/tmp/blacktip-timestamp"
           -- 720 hours.
         , allowableDowntime = 2592000000 }
                      

data ServerState =
  ServerState { ssTime     :: Milliseconds
              , ssSequence :: Sequence }
  deriving Show

-- {64 bit timestamp, 48 bit id (MAC address), 16 bit sequence}
type UniqueId = Integer

data IdentityRecord =
  IdentityRecord { identityTime :: Milliseconds
                 , identityMac  :: NI.MAC
                 , identitySequence :: Sequence }
  deriving (Eq, Ord, Show)

-- if the data in the file doesn't parse
data ParseError = ParseError deriving Show

-- If our current timestamp is younger than the one in our state
data ArrowOfTimeError = ArrowOfTimeError deriving Show

-- We couldn't find an interface by that name, did you change it
-- from the default IName "eth0" ?
data NoInterfaceError = NoInterfaceError deriving (Eq, Ord, Show)
