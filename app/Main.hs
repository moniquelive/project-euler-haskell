module Main where

import Lib(pCurrent)

main :: IO ()
main = do
  r <- pCurrent
  putStrLn . show $ r
