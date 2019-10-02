module Main where

import Lib
import System.IO
import System.Directory
import Text.Read
import Control.Monad
import Data.List.Split

-- main :: IO ()
-- main = readFile "/proc/1726/cmdline" >>= putStrLn

main :: IO ()
main = getDirectoryContents "/proc/"
  >>= filterM (\x -> case readMaybe x :: Maybe Integer of
                  Just _ -> return True
                  Nothing -> return False)
  >>= mapM (\x ->
              readFile ("/proc/" ++ x ++ "/cmdline") >>= return . head . splitOn " ")
  >>= mapM_ putStrLn
