module Main where

import           Control.Monad
import           Data.List.Split
import           Data.Maybe
import           System.Directory
import           SysTools.Tasks
import           Text.Read

data Proc =
  Proc
    { name :: String
    , pid  :: String
    }

data Model =
  Model
    { procs    :: [Proc]
    , search   :: String -- Maybe String ?
    , position :: Int
    }

header = "Filter Processes : \n"
instructions = "\nup/down arrow keys to move, type to filter, enter to select"

render :: [Proc] -> IO ()
render ps = putStrLn $ header <> unlines (showProc <$> ps) <> instructions

filter_procs :: String -> [Proc] -> [Proc]
filter_procs search = filter (isContainedIn search . name)

showProc :: Proc -> String
showProc x =  pid x ++ " :: " ++ name x

path :: String -> FilePath
path pid = "/proc/" ++ pid ++ "/cmdline"

getProcName :: String -> String
getProcName = head . splitOn " "

getProcs :: IO [Proc]
getProcs = do
  dirs <- getDirectoryContents "/proc/"
  valid_dirs <- filterM (doesFileExist . path ) . map show . catMaybes $ (readMaybe <$> dirs :: [Maybe Integer])
  found_procs <- mapM (\pid -> do
                          contents <- readFile . path $ pid
                          return Proc {name = getProcName contents, pid = pid}
                      ) valid_dirs
  -- found_procs <- mapM ((>>= (\x -> return Proc {name = getProcName x, pid = ""})) . readFile . path)  valid_dirs
  return $ filter hasName found_procs
  where
    hasName x = name x /= ""

main :: IO ()
main = do
  ps <- getProcs
  render $ filter_procs "emacs" ps
  -- listen
