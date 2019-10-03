module Main where

import Control.Monad
import Data.Functor
import Data.List.Split
import Lib
import System.Directory
import System.IO
import Text.Read

-- main :: IO ()
-- main = readFile "/proc/1726/cmdline" >>= putStrLn
data Proc =
  Proc
    { name :: String
    , pid :: String
    }

data Model =
  Model
    { procs :: [Proc]
    , search :: String -- Maybe String ?
    , position :: Int
    }

-- show_procs :: [String] -> String -> Int -> IO ()
-- show_procs procs search pos =
-- show_procs :: [String] -> IO ()
-- show_procs procs = map put(take 5 procs)
showProc x = putStrLn $ (pid x) ++ " :: " ++ (name x)

path pid = "/proc/" ++ pid ++ "/cmdline"

getProcs :: IO [Proc]
getProcs =
  getDirectoryContents "/proc/" >>=
  filterM (\x ->
                 case readMaybe x :: Maybe Integer of
                   Just _ -> return True
                   Nothing -> return False)
  >>= filterM (\x -> doesFileExist (path x) )
  >>= mapM (\pid ->
              readFile (path pid) >>= (\name -> return (Proc (head . splitOn " " $ name) pid)))
  >>= filterM (\x -> return (name x /= ""))

main :: IO ()
main =
  take 5 <$> getProcs
  >>= mapM_ showProc
