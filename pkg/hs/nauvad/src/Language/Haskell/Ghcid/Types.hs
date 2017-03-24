{-# LANGUAGE DeriveDataTypeable #-}

-- | The types types that we use in Ghcid
module Language.Haskell.Ghcid.Types(
    GhciError(..),
    Stream(..),
    Load(..), Severity(..), isMessage,
    NVDMessage(..), GHCMessage(..)
    ) where

import Data.Data
import Data.Text (Text)
import qualified Data.Aeson as A
import Control.Exception.Base (Exception)
import Nauva.Internal.Types

-- | GHCi shut down
data GhciError = UnexpectedExit String String
    deriving (Show,Eq,Ord,Typeable,Data)

-- | Make GhciError an exception
instance Exception GhciError

-- | The stream Ghci is talking over.
data Stream = Stdout | Stderr
    deriving (Show,Eq,Ord,Bounded,Enum,Read,Typeable,Data)

-- | Severity of messages
data Severity = Warning | Error
    deriving (Show,Eq,Ord,Bounded,Enum,Read,Typeable,Data)

-- | Load messages
data Load
    = Loading
        {loadModule :: String
        ,loadFile :: FilePath
        }
    | Message
        {loadSeverity :: Severity
        ,loadFile :: FilePath
        ,loadFilePos :: (Int,Int)
        ,loadMessage :: [String]
        }
    deriving (Show, Eq, Ord)

-- | Is a Load a message with severity?
isMessage :: Load -> Bool
isMessage Message{} = True
isMessage _ = False



-- | Here we have two groups of messages: control and data. Control is
-- all about the connection between NVD and the browser. Data is all
-- about the state and contents of the application.
data NVDMessage
    = NVDMLoading
      -- ^ The application was killed and the server is recompiling it.
      -- During this time the client SHOULD NOT send any messages to the
      -- server (as they will be ignored). The client SHOULD wait for
      -- 'NVDMGood' or 'NVDMFailed'.

    | NVDMGood
    | NVDMFailed [GHCMessage]

    | NVDMLocation Text
    | NVDMLocationRaw A.Value
    | NVDMSpine Spine
    | NVDMSpineRaw A.Value

instance A.ToJSON NVDMessage where
    toJSON NVDMLoading        = A.toJSON [A.toJSON (1 :: Int)]
    toJSON NVDMGood           = A.toJSON [A.toJSON (2 :: Int)]
    toJSON (NVDMFailed msgs)  = A.toJSON [A.toJSON (3 :: Int), A.toJSON (map A.toJSON msgs)]
    toJSON (NVDMLocation loc) = A.toJSON [A.toJSON (4 :: Int), A.toJSON loc]
    toJSON (NVDMLocationRaw v) = A.toJSON [A.toJSON (4 :: Int), v]
    toJSON (NVDMSpine spine)  = A.toJSON [A.toJSON (5 :: Int), A.toJSON spine]
    toJSON (NVDMSpineRaw v)   = A.toJSON [A.toJSON (5 :: Int), v]


data GHCMessage = GHCMessage Severity FilePath (Int, Int) [Text]

instance A.ToJSON GHCMessage where
    toJSON (GHCMessage severity filePath filePos msg) = A.toJSON
        [ A.toJSON severity
        , A.toJSON filePath
        , A.toJSON filePos
        , A.toJSON msg
        ]

instance A.ToJSON Severity where
    toJSON Warning = A.toJSON [A.toJSON (1 :: Int)]
    toJSON Error   = A.toJSON [A.toJSON (2 :: Int)]
