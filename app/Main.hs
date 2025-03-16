module Main where

import Files (buildListOfFiles, isValidPath)
import Report (numberOfSyscalls, reportSyscallSummary)
import ReportBuilder (buildReport)
import System.Environment

data Settings = Settings
  { sourceRoot :: FilePath,
    sourceFilter :: [String]
  }

defaultSourceFilter :: [String]
defaultSourceFilter = [".c", ".h", ".cpp", ".hpp", ".cxx", ".hxx"]

usage :: IO ()
usage = do
  putStrLn "search-syscall <path> - where <path> is file or directory to scan"

readArguments :: IO (Maybe Settings)
readArguments = do
  args <- getArgs
  return $ case args of
    (root : _) -> Just Settings {sourceRoot = root, sourceFilter = defaultSourceFilter}
    [] -> Nothing

runApp :: Settings -> IO ()
runApp settings = do
  putStrLn "Checking root directory..."
  let root = sourceRoot settings
  isValidRoot <- isValidPath root
  if not isValidRoot then error "root directory is incorrect or not exist" else putStrLn "  -> OK"

  putStrLn "Building list of files..."
  files <- buildListOfFiles (sourceRoot settings) (sourceFilter settings)
  putStrLn ("  -> " ++ show (length files) ++ " matche(s) found")

  putStrLn "Searching for syscalls..."
  report <- buildReport files
  putStrLn ("  -> " ++ show (numberOfSyscalls report) ++ " match(es) found")

  putStrLn ""
  putStrLn (reportSyscallSummary report)
  putStrLn ""
  return ()

main :: IO ()
main = do
  arguments <- readArguments
  maybe usage runApp arguments
