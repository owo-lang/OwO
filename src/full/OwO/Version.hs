{-# LANGUAGE TemplateHaskell #-}

module OwO.Version
  ( version
  , commitInfo
  , versionWithCommitInfo
  ) where

import           Development.GitRev

import           Data.List          (intercalate, map)
import           Data.Maybe         (fromMaybe)
import           Data.Version

import qualified Paths_OwO          as PO

-- | The version of OwO.

version :: String
version = intercalate "." $
  show <$> versionBranch PO.version

versionWithCommitInfo :: String
versionWithCommitInfo = version ++ maybe "" ('-' :) commitInfo

-- | Information about current git commit, generated at compile time
commitInfo :: Maybe String
commitInfo
  | hash == "UNKNOWN" = Nothing
  | otherwise         = Just $ abbrev hash ++ dirty
  where
    hash = $(gitHash)

    -- | Check if any tracked files have uncommitted changes
    dirty | $(gitDirtyTracked) = "-dirty"
          | otherwise          = ""

    -- | Abbreviate a commit hash while keeping it unambiguous
    abbrev = take 11

