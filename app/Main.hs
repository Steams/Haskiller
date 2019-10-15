{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad
import           Data.List.Extra
import           Data.Maybe
import           System.Console.ANSI
import           System.Console.ANSI
import           System.Directory
import           System.Process
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
data TextInput = SearchInput Char | Backspace
data Input = Exit | Nav ArrowInput | Search TextInput | Select

safe_head :: [a] -> Maybe a
safe_head = \case
  [] -> Nothing
  x:xs -> Just x

maxShown = 10

header pos shownPs allPs search = "Filter Processes : #" ++ show pos ++  " (" ++ show shownPs ++ "/" ++ show allPs ++ ") : " ++ search ++ "\n"
instructions = "\nup/down arrow keys to move, type to filter, enter to select"


filter_procs :: Model -> [Proc]
filter_procs model = filter (isContainedIn (search model) . name) (procs model)

showProc :: Proc -> String
showProc x =  pid x ++ " :: " ++ name x

path :: String -> FilePath
path pid = "/proc/" ++ pid ++ "/cmdline"

getProcName :: String -> String
getProcName p = case (safe_head . splitOn " " $ p) of
  Just x  -> x
  Nothing -> ""

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
    '\DEL' -> return $ Search Backspace
    '\n' -> return $ Select
    '\ESC' -> parseInput
    '[' -> parseInput
    -- may need to add flag to parseInput so that A and B count as regular letters when not parsting \ESC
    'A' -> return $ Nav Up
    'B' -> return $ Nav Down
    c   -> return $ Search $ SearchInput c
    -- _ -> return Exit

updatePos :: ArrowInput -> Int -> Int -> Int
updatePos dir current size =
  case dir of
    Up   -> if current == 1 then current else current - 1
    Down -> if current == size then current else current + 1

updateSlot :: ArrowInput -> Int -> Int -> Int
updateSlot dir current size =
  case dir of
    Up   -> if current == 1 then current else current - 1
    Down -> if current == min maxShown size then current else current + 1

updateModelPos :: Model -> Int -> Int -> Model
updateModelPos model = Model (procs model) (search model)

updateModelSearch:: Model -> TextInput -> Model
updateModelSearch model x =
  case x of
    SearchInput c -> Model (procs model) (snoc (search model) c) 1 1
    Backspace -> Model (procs model) (reverse . drop 1 . reverse $ search model) 1 1

selected_proc model =
  let
    filtered = filter_procs model
    pos      = position model

    scroll =
      case slot model of
        1        -> drop (pos - 1)
        maxShown -> drop (pos - maxShown)
        _        -> drop (pos - slot model)

    displayed_procs    = take maxShown . scroll $ filtered
    selected           = take 1 . drop (slot model - 1) $ displayed_procs
  in
    case selected of
      [] -> Nothing
      xs -> Just $ head xs

render :: Model -> IO ()
render model =
  let
    filtered = filter_procs model
    pos      = position model

    scroll =
      case slot model of
        1        -> drop (pos - 1)
        maxShown -> drop (pos - maxShown)
        _        -> drop (pos - slot model)

    displayed_procs    = take maxShown . scroll $ filtered

    before_selected    = take (slot model - 1) displayed_procs
    selected           = take 1 . drop (slot model - 1) $ displayed_procs
    after_selected     = take (maxShown - slot model) . drop (slot model) $ displayed_procs

    before_formatted   = unlines $ showProc <$> before_selected
    after_formatted    = unlines $ showProc <$> after_selected
    selected_formatted = case (safe_head selected) of
        Just x  -> "> " ++ showProc  (head selected)
        Nothing -> ""

    all_count       = length (procs model)
    filtered_count  = length filtered

    heading = header pos filtered_count all_count (search model)

   in do
    clearScreen
    setCursorPosition 0 0
    putStrLn  heading
    putStrLn  before_formatted
    setSGR [SetColor Foreground Dull Blue]
    putStrLn  selected_formatted
    setSGR [Reset]
    putStrLn  after_formatted
    putStrLn  instructions

printPS ps = do
  putStrLn $ showProc ps
  getChar

kill_ps pid = runCommand $ "kill " ++ pid

listen :: Model -> IO ()
listen model =
  parseInput >>= \case
    Nav dir ->
      let
        size        = length $ filter_procs model
        currentPos  = position model
        currentSlot = slot model
        new_pos     = updatePos dir currentPos size
        new_slot    = updateSlot dir currentSlot size
        newModel    = updateModelPos model new_pos new_slot
       in
        redraw newModel
    Search (SearchInput c) ->
      let
        newModel    = updateModelSearch model $ SearchInput c
       in
        redraw newModel
    Search Backspace ->
      let
        newModel    = updateModelSearch model Backspace
       in
        redraw newModel
    Select ->
      case selected_proc model of
        Just x  -> kill_ps (pid x)  >> refresh (model {search = ""})
        Nothing -> refresh (model {search = ""})
    Exit -> return ()

redraw model = do
  render $ model
  listen $ model

refresh model = do
  ps <- getProcs
  redraw $ model {procs = ps}

main :: IO ()
main = refresh $ Model [] "" 1 1
