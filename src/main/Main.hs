module Main (main) where

import OwO.Main (runOwO)

import OptionParser (options)

main :: IO ()
main = options >>= runOwO
