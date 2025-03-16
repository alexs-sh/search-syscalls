module ReportBuilder where

import Control.Concurrent (MVar, ThreadId, forkIO, modifyMVar, modifyMVar_, newMVar, readMVar, threadDelay)
import Control.DeepSeq (deepseq)
import Control.Monad (foldM, forM_)
import qualified Data.ByteString as BS
import qualified Data.HashSet as HS
import Data.Word (Word8)
import GHC.Conc (numCapabilities)
import Report (Report (Report), emptyReport, knownSyscalls)

-- Default task size (number of files) for processing
defaultFileBatchSize :: Int
defaultFileBatchSize = 4

-- Checks if a Word8 is an alphanumeric character.
-- It's used for splitting text file(s) into tokens.
isAlphaNumWord8 :: Word8 -> Bool
isAlphaNumWord8 w =
  (w >= 48 && w <= 57) -- numbers
    || (w >= 65 && w <= 90) -- capital letters
    || (w >= 97 && w <= 122) -- letters

-- A shared context for workers during concurrent processing
data SharedContext = SharedContext
  { processingList :: [FilePath], -- List of files to be processed by workers
    finalReport :: Report, -- The final report, which workers incrementally add to
    activeWorkersCount :: Int -- The number of active workers (when == 0, all workers have finished)
  }

-- Create a new SharedContext
newSharedContext :: [FilePath] -> Int -> SharedContext
newSharedContext files numberOfWorkers = SharedContext {processingList = files, finalReport = emptyReport, activeWorkersCount = numberOfWorkers}

-- Fetch the next batch of files to process and update the processing list
fetchNextFileBatch :: SharedContext -> (SharedContext, [FilePath])
fetchNextFileBatch context = let (filesToProcess, remainFiles) = splitAt defaultFileBatchSize $ processingList context in (context {processingList = remainFiles}, filesToProcess)

-- Update the context when a worker is done
workerDone :: SharedContext -> Report -> SharedContext
workerDone (SharedContext files summary workersCount) workerReport =
  SharedContext files (summary <> workerReport) (workersCount - 1)

-- The worker thread that processes files until they are available in a shared
-- context
workerThread :: MVar SharedContext -> IO ()
workerThread context = do
  report <- run emptyReport
  modifyMVar_ context (\ctx -> return (workerDone ctx report))
  where
    run :: Report -> IO Report
    run report = do
      filesToProcess <- modifyMVar context (return . fetchNextFileBatch)
      case filesToProcess of
        [] -> return report
        _ -> do
          nextReport <- foldM buildReportFromFile report filesToProcess
          run nextReport

-- Wait for all workers to finish processing and provide the final report
waitWorkers :: MVar SharedContext -> IO Report
waitWorkers context = do
  lockedContext <- readMVar context
  if activeWorkersCount lockedContext <= 0
    then do
      return $ finalReport lockedContext
    else do
      threadDelay (1000 * 100)
      waitWorkers context

-- Fork a worker thread
forkWorker :: MVar SharedContext -> IO ThreadId
forkWorker context = forkIO $ workerThread context

-- Process a single file and update the report with syscalls found in it
buildReportFromFile :: Report -> FilePath -> IO Report
buildReportFromFile (Report existingReport) file = do
  rawText <- BS.readFile file
  let tokens = BS.splitWith (not . isAlphaNumWord8) rawText
  let syscalls = foldr addSyscallToReport existingReport tokens
  syscalls `deepseq` return ()
  return $ Report syscalls
  where
    addSyscallToReport candidate report
      | candidate `HS.member` knownSyscalls = HS.insert candidate report
      | otherwise = report

-- Run through a list of files and find all system calls in it
buildReport :: [FilePath] -> IO Report
buildReport [] = return emptyReport
buildReport [singleFile] = foldM buildReportFromFile emptyReport [singleFile]
buildReport files = do
  let numberOfWorkers = max (numCapabilities - 1) 1 :: Int
  context <- newMVar $ newSharedContext files numberOfWorkers
  forM_ [1 .. numberOfWorkers] $ \_ -> forkWorker context
  waitWorkers context
