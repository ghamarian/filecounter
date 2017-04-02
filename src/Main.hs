{-# LANGUAGE FlexibleContexts  #-}
module Main where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (forM, liftM, forM_, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell)
import Control.Monad.Reader
import Control.Monad.State.Lazy
{-import Control.Monad.Trans.Writer.Lazy-}

myName :: MonadReader String m => String -> m String
myName step = do
    name <- ask
    return (step ++ ", I am " ++ name)

localExmaple :: Reader String (String, String, String)
localExmaple = do
        a <- myName "First"
        b <- local (++ "dy") (myName "Second")
        c <- myName "Third"
        return (a, b, c)

main :: IO ()
main = do
    list <- countEntriesTrad "."
    print list

listDirectory :: FilePath -> IO [String]
listDirectory = fmap (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."

countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
            let newName = path </> name
            isDir <- doesDirectoryExist newName
            if isDir
              then countEntriesTrad newName
              else return []
  return $ (path, length contents) : concat rest

---------------------------------------------------------------
{-newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }-}
{-tell   :: w -> m ()-}
{-tell w = writer ((),w)-}

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
      let newName = path </> name
      isDir <- liftIO . doesDirectoryExist $ newName
      when isDir $ countEntries newName

newtype AppConfig = AppConfig {
      cfgMaxDepth :: Int
    } deriving (Show)

data AppState = AppState {
      stDeepestReached :: Int
    } deriving (Show)

type App = ReaderT AppConfig (StateT AppState IO)

runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth =
    let config = AppConfig maxDepth
        state = AppState 0
    in runStateT (runReaderT k config) state

constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  cfg <- ask
  rest <- forM contents $ \name -> do
            let newPath = path </> name
            isDir <- liftIO $ doesDirectoryExist newPath
            if isDir && curDepth < cfgMaxDepth cfg
              then do
                let newDepth = curDepth + 1
                st <- get
                when (stDeepestReached st < newDepth) $
                  put st { stDeepestReached = newDepth }
                constrainedCount newDepth newPath
              else return []
  return $ (path, length contents) : concat rest

