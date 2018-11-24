{-# LANGUAGE DeriveGeneric #-}

module OwO.Options
  ( CompilerOptions(..)
  , PragmaOptions(..)
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
  { optNoPositivityCheck     :: Bool
  , optNoTerminationCheck    :: Bool
  , optNoExhaustivenessCheck :: Bool
  } deriving (Eq, Generic, Show)
