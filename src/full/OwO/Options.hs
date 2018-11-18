{-# LANGUAGE DeriveGeneric #-}

module OwO.Options
  ( CmdOptions(..)
  , PragmaOptions(..)
  , defaultCmdOptions
  , defaultPragmaOptions
  ) where

import           Data.Function
import           Data.List

import           GHC.Generics  (Generic)

data CmdOptions = CmdOptions
  { optInputFile    :: Maybe FilePath
  , optIncludePaths :: [FilePath]
  , optShowVersion  :: Bool
  , optShowHelp     :: Bool
  -- , optPragmaOptions    :: PragmaOptions
  } deriving (Generic, Show)

data PragmaOptions = PragmaOptions
  { optSafe :: Bool
  } deriving (Eq, Generic, Show)

defaultCmdOptions :: CmdOptions
defaultCmdOptions = CmdOptions
  { optInputFile         = Nothing
  , optIncludePaths      = []
  , optShowVersion       = False
  , optShowHelp          = False
  -- , optPragmaOptions     = defaultPragmaOptions
  }

defaultPragmaOptions :: PragmaOptions
defaultPragmaOptions = PragmaOptions
  { optSafe = True
  }
