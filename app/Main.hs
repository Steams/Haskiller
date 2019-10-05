{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad
import           Data.List.Split
import           Data.Maybe
import           System.Console.ANSI
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
    , slot     :: Int
    }

data ArrowInput = Up | Down
data Input = Exit | Nav ArrowInput

maxShown = 5

header = "Filter Processes : \n"
instructions = "\nup/down arrow keys to move, type to filter, enter to select"


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
  return $ filter hasName found_procs
  where
    hasName x = name x /= ""

-- \ESC[A is code for Up arrow etc
parseInput :: IO Input
parseInput =
  getChar >>= \case
    '\ESC' -> parseInput
    '[' -> parseInput
    'A' -> return $ Nav Up
    'B' -> return $ Nav Down
    _ -> return Exit

updatePos :: ArrowInput -> Int -> Int -> Int
updatePos dir current length =
  case dir of
    Up   -> if current == 1 then current else current - 1
    Down -> if current == length then current else current + 1

updateSlot :: ArrowInput -> Int -> Int -> Int
updateSlot dir current length =
  case dir of
    Up   -> if current == 1 then current else current - 1
    Down -> if current == min maxShown length then current else current + 1

updateModel :: Model -> Int -> Int -> Model
updateModel model pos slot = Model (procs model) (search model) pos slot

render :: Model -> IO ()
render model = do
  clearScreen
  putStrLn $ header <> unlines (showProc <$> ps) <> instructions
  where
    ps = take 10 . drop (position model - 1) $ (procs model)

listen :: Model -> IO ()
listen model =
  parseInput >>= \case
    Nav dir ->
      let size = length $ procs model
          currentPos = position model
          currentSlot = slot model
          newModel =
            updateModel
              model
              (updatePos dir currentPos size)
              (updateSlot dir currentSlot size)
       in render newModel >> listen newModel
    Exit -> return ()

main :: IO ()
main = do
  ps <- getProcs
  putStrLn $ show . length $ ps
  render $ model ps
  listen $ model ps
  where
    model x = Model x "" 1 1
