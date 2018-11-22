module OwO.Util.Applicative where

import           Control.Applicative

ifM :: Applicative m => Bool -> m () -> m ()
ifM True  m = m
ifM False _ = pure ()
