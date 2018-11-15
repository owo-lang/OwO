{-# LANGUAGE CPP #-}

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.BuildPaths (exeExtension)
import Distribution.PackageDescription

import System.Directory
import System.Exit
import System.IO
import System.FilePath
import System.FilePath.Find
import System.Process

main = defaultMainWithHooks hooks
hooks = simpleUserHooks { regHook = checkAndAddCliCompletion }

bashCompletion :: FilePath
bashCompletion = "~/.bash_completion"

checkCliCompletion :: PackageDescription -> LocalBuildInfo -> RegisterFlags -> IO ()
checkCliCompletion pkg info flags = do
#ifdef WIN32
  -- We should disable this for Windows.
  putStrLn "No cli compwetion for Windows. Gomenasai OwO."
#else
  -- These codes are for *nix users
  let dirs = absoluteInstallDirs pkg info NoCopyDest
      owo  = buildDir info </> "owo" </> "owo"
  -- Error handling?
  _  <- rawSystem "touch" [bashCompletion]
  (_, h0', _, h1) <-
      createProcess (proc owo ["--bash-completion-script", owo])
        { std_out = CreatePipe
        }
  case h0' of
    Nothing -> putStrLn "Ayayayaya! Failed to install OwO!"
    Just h0 -> do
      completion <- hGetContents h0
      exitCode   <- waitForProcess h1
      case exitCode of
        ExitFailure _ -> putStrLn $
          "Ayayayaya! Failed to genewate bash compwetion!"
        ExitSuccess   -> appendFile bashCompletion completion
#endif

checkAndAddCliCompletion :: PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO ()
checkAndAddCliCompletion pkg info hooks flags = do
  checkCliCompletion pkg info flags
  regHook simpleUserHooks pkg info hooks flags
  -- This actually does something useful
