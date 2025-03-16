{-# LANGUAGE ScopedTypeVariables #-}

module Files where

import Control.Exception (SomeException, catch)
import Control.Monad (foldM)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.FilePath (takeExtension, (</>))

-- Check if a file or directory exists
isValidPath :: FilePath -> IO Bool
isValidPath file = do
  isFile <- doesFileExist file
  isDir <- doesDirectoryExist file
  return $ isFile || isDir

-- Build list of files with allowed extensions from a root directory
buildListOfFiles :: FilePath -> [String] -> IO [FilePath]
buildListOfFiles rootFileOrDirectory supportedExtensions = do
  rootDirectory <- makeAbsolute rootFileOrDirectory
  visitSubdirectory rootDirectory
  where
    visitSubdirectory :: FilePath -> IO [FilePath]
    visitSubdirectory rootDirectory = do
      entries <-
        map (rootDirectory </>)
          <$> catch
            (listDirectory rootDirectory)
            ( \(e :: SomeException) -> do
                putStrLn ("Error reading directory " ++ rootDirectory ++ ": " ++ show e)
                return []
            )

      (filesToCheck, directoriesToVisit) <- separateFilesFromDirectories entries
      let filesToVisit = filter (\file -> takeExtension file `elem` supportedExtensions) filesToCheck
      filesFromSubdirectories <- mapM visitSubdirectory directoriesToVisit
      return (filesToVisit ++ concat filesFromSubdirectories)

-- Partition files and directories
separateFilesFromDirectories :: [FilePath] -> IO ([FilePath], [FilePath])
separateFilesFromDirectories = foldM separator ([], [])
  where
    separator (files, directories) currentFilePath = do
      isDirectory <- doesDirectoryExist currentFilePath
      return $ if isDirectory then (files, currentFilePath : directories) else (currentFilePath : files, directories)
