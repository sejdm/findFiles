{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
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
    , fg
    , bg
    , simpleCommand
    , (.=)
    , usingMonoid
    , usingList
    ) where

import Data.Monoid
import EitherExtras
import Data.Char
import Data.List
import Control.Applicative
import Data.List.Split
import Control.Monad.Trans.Either
import Control.Monad.Except
import System.Process
import Pipes
import qualified Pipes.Prelude as R
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO
import System.Exit
import System.Console.ANSI
import Control.Exception

type EitherIO s = EitherT s IO

data Settings = Settings {
    listColour :: [SGR]
  , queryColour :: [SGR]
  , outputColour :: [SGR]
  , errorColour :: [SGR]
  , loadEntireDatabase :: Bool
                         }


defaultSettings :: Settings
defaultSettings = Settings
  {
   listColour = fg Vivid Green
 , queryColour = fg Vivid Yellow
 , outputColour = fg Dull Green
 , errorColour = fg Dull Black
 , loadEntireDatabase = False
}

keepSearching'' f s = 
  do (e , o, _) <- liftIO $ readCreateProcessWithExitCode (shell ("cat " ++ f ++ " | fzf -q '" ++ T.unpack s ++ "'")) ""
     if e == ExitSuccess then return o else throwError "Cannot find the file " 

keepSearching' f s = do x <- liftIO $ fmap (reverse . tail . reverse) $ catch (readCreateProcess (shell ("cat " ++ f ++ " | fzf --height=10% -q '" ++ T.unpack s ++ "'")) "") ((\_ -> return "a") :: IOError -> IO String)
                        if x == "" then throwError "Cannot find the file" else return x

keepSearching :: (MonadIO m, MonadError String m) => Settings -> [T.Text] -> T.Text -> m FilePath
keepSearching s ns n = case rs of
                          [] -> throwError $ "Cannot find the file " ++ T.unpack n
                          [a] -> pure $ T.unpack a
                          _ -> enumeratePrint (listColour s)  rs >> askMore (queryColour s)  >>= keepSearching s rs
  where rs = restrict n ns


enumeratePrint :: MonadIO m => [SGR] -> [T.Text] -> m ()
enumeratePrint fc s = liftIO $ setSGR fc >> (mapM_ TIO.putStrLn . zipWith (\a b -> a <> ": " <> b) (map (T.pack . show) [0..])) s


askMore :: MonadIO m => [SGR] -> m T.Text
askMore fc = liftIO $ setSGR fc >> TIO.putStrLn "? " >> TIO.getLine

vivid :: Color -> [SGR]
vivid x = [SetColor Foreground Vivid x]

fg :: ColorIntensity -> Color -> [SGR]
fg v c = [SetColor Foreground v c]


bg :: ColorIntensity -> Color -> [SGR]
bg v c = [SetColor Background v c]

restrict :: T.Text -> [T.Text] -> [T.Text]
restrict s ss | all isDigit s' = [ss !! (read s')]
              | otherwise = filter (match s) ss
              where s' = T.unpack s


match :: T.Text -> T.Text -> Bool
match s t = all (flip T.isInfixOf (T.toLower t)) $ T.words $ T.toLower s

getExtension :: FilePath -> String
getExtension = last . splitOn "."


type FileExtension = String
type FileAssociation f = FileExtension -> f (FilePath -> CreateProcess)


usingList :: [FileAssociation Maybe] -> FileAssociation Maybe
usingList fs s = foldl1' (<|>) $ map ($ s) fs


simpleCommand :: Alternative f => FilePath -> [FileExtension] -> FileAssociation f
simpleCommand p es e |  e `elem` es = pure (\fp -> proc h (t++[fp]))
                     | otherwise = empty
  where (h:t) = words p

(.=) :: Alternative f => FilePath -> [FileExtension] -> FileAssociation f
(.=) = simpleCommand

usingMonoid  :: (FileExtension -> Alt Maybe (FilePath -> CreateProcess)) -> FileAssociation Maybe
usingMonoid x = getAlt . x




findProgram :: MaybeType f => FileAssociation f -> String -> Either String (CreateProcess, FilePath)
findProgram m s = ( \t -> (t s, s) ) <$> maybeToEither "Could not find a suitable program for the extension" (m ext)
  where ext = getExtension s


findProg :: MaybeType f => FileAssociation f -> String -> EitherIO String (CreateProcess, FilePath)
findProg m = hoistEither . findProgram m


opeFile :: MaybeType f => Settings -> FilePath -> FileAssociation f -> String -> IO ()
opeFile s' f m t = do fs <- (if (loadEntireDatabase s') then getFirstList' else getFirstList) f s
                      r <- runEitherT $ (keepSearching' f s) >>= findProg m
                      case r of
                        Right (x, s'') -> do _ <- createProcess x
                                             setSGR (outputColour s')
                                             putStrLn s''
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



printQ :: Settings -> FilePath -> FilePath -> String -> IO ()
printQ s' f f' t = do fs <- (if (loadEntireDatabase s') then getFirstList' else getFirstList) f s
                      r <- runEitherT $ (keepSearching s' fs s)
                      case r of
                        Right x -> appendFile f' (x ++ "\n")
                        Left s -> setSGR (errorColour s') >> hPutStrLn stdout s
  where s = T.pack t


