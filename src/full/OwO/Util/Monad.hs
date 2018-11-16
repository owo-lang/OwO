module OwO.Util.Monad where

import           Control.Monad

ifM :: Monad m => Bool -> m () -> m ()
ifM True  m = m
ifM False _ = return ()

