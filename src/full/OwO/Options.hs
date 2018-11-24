{-# LANGUAGE DeriveGeneric #-}

module OwO.Options
  ( CompilerOptions(..)
  , PragmaOptions(..)
  , defaultPragmaOptions
  ) where

import           Data.Function
import           Data.List

import           GHC.Generics  (Generic)

data CompilerOptions = CompilerOptions
  { optInputFile     :: FilePath
  , optIncludePaths  :: [FilePath]
  , optPragmaOptions :: PragmaOptions
  } deriving (Generic, Show)

data PragmaOptions = PragmaOptions
  { optSafe :: Bool
  } deriving (Eq, Generic, Show)

defaultPragmaOptions :: PragmaOptions
defaultPragmaOptions = PragmaOptions
  { optSafe = True
  }
