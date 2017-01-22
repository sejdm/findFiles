{-# LANGUAGE OverloadedStrings #-}
module FileSearch
    (
      opeFile
    , FileAssociation
    , defaultSettings
    , vivid
    , listColour
    , queryColour
    , errorColour
    , loadEntireDatabase
    , printQ
    , emptyAssoc
    , shComm
    , fg
    , bg
    ) where

import Data.Monoid
import Data.Char
import Data.List.Split
import Control.Monad.Trans.Either
import Control.Monad.Except
import Control.Monad
import Control.Monad.IO.Class
import System.Process
import Pipes
import qualified Pipes.Prelude as R
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO
import System.Console.ANSI

type EitherIO s = EitherT s IO

data Settings = Settings {
    listColour :: [SGR]
  , queryColour :: [SGR]
  , outputColour :: [SGR]
  , errorColour :: [SGR]
  , loadEntireDatabase :: Bool
                         }


defaultSettings = Settings
  {
   listColour = fg Vivid Green
 , queryColour = fg Vivid Yellow
 , outputColour = fg Dull Black
 , errorColour = fg Dull Black
 , loadEntireDatabase = False
}

keepSearching s ns = runEitherT . keepSearching' s ns

keepSearching' :: Settings -> [T.Text] -> T.Text -> EitherIO String FilePath
keepSearching' s ns n = case rs of
                          [] -> throwError $ "Cannot find the file " ++ T.unpack n
                          [a] -> pure $ T.unpack a
                          _ -> enumeratePrint (listColour s)  rs >> askMore (queryColour s)  >>= keepSearching' s rs
  where rs = restrict n ns


enumeratePrint fc s = liftIO $ setSGR fc >> (mapM_ TIO.putStrLn . zipWith (\a b -> a <> ": " <> b) (map (T.pack . show) [0..])) s


askMore fc = liftIO $ setSGR fc >> TIO.putStrLn "? " >> TIO.getLine


vivid x = [SetColor Foreground Vivid x]
fg v c = [SetColor Foreground v c]
bg v c = [SetColor Background v c]

restrict :: T.Text -> [T.Text] -> [T.Text]
restrict s ss | all isDigit s' = [ss !! (read s')]
              | otherwise = filter (match s) ss
              where s' = T.unpack s


match :: T.Text -> T.Text -> Bool
match s t = all (flip T.isInfixOf (T.toLower t)) $ T.words $ T.toLower s

getExtension :: FilePath -> String
getExtension = last . splitOn "."


type FileAssociation = String -> Maybe [FilePath]

shComm :: FilePath -> [String] -> FileAssociation
shComm a es e = if e `elem` es then Just (words a) else Nothing

emptyAssoc = M.empty



------



findProgram :: FileAssociation -> String -> Either String ([String], FilePath)
findProgram m s = let ext = getExtension s in
                    case m ext of
                      Nothing -> throwError $ "Could not find a suitable program for the extension " ++ ext
                      Just x -> pure (x, s)

findProg m = hoistEither . findProgram m




opeFile s' f m t = do fs <- (if (loadEntireDatabase s') then getFirstList' else getFirstList) f s
                      r <- runEitherT $ (keepSearching' s' fs s) >>= findProg m
                      case r of
                        Right (x, s) -> let (h:t) = x in spawnProcess h (t++[s]) >> setSGR (outputColour s') >> putStrLn s
                        Left s -> setSGR (errorColour s') >> hPutStrLn stdout s
  where s = T.pack t


firstListPipe :: FilePath -> T.Text -> Producer T.Text IO ()
firstListPipe f s = read' f >-> R.filter (match s)

getFirstList :: FilePath -> T.Text -> IO [T.Text]
getFirstList f = R.toListM . firstListPipe f


-- Loads the entire database in memory
getFirstList' :: FilePath -> T.Text -> IO [T.Text]
getFirstList' f s = filter (match s) . T.lines <$> TIO.readFile f


readFile' :: Handle -> Producer T.Text IO ()
readFile' h = do
    eof <- lift $ hIsEOF h
    when (not eof) $ do
        s <- lift $ TIO.hGetLine h
        yield s
        readFile' h

read' :: FilePath -> Producer T.Text IO ()
read' file = do
    h <- lift $ openFile file ReadMode
    readFile' h
    lift $ hClose h


--- Experimental...for printer dialogue

data Pages = PageRaw String | OnePage Int | PageRange Int Int | PageList [Pages] | AllPages

getPages :: String -> EitherIO String Pages
getPages "" = pure AllPages
getPages n = case words n of
  [m] -> pure $ OnePage (read m)



printQ s' f f' t = do fs <- (if (loadEntireDatabase s') then getFirstList' else getFirstList) f s
                      r <- runEitherT $ (keepSearching' s' fs s)
                      case r of
                        Right x -> appendFile f' x
                        Left s -> setSGR (errorColour s') >> hPutStrLn stdout s
  where s = T.pack t
