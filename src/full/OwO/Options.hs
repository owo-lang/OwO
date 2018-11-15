module OwO.Options where

import Data.Function


data CmdOptions = Options
  { optInputFile        :: Maybe FilePath
  , optIncludePaths     :: [FilePath]
  , optShowVersion      :: Bool
  , optShowHelp         :: Bool
  } deriving Show

