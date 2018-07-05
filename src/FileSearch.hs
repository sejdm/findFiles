{-# LANGUAGE OverloadedStrings, FlexibleContexts, NoMonomorphismRestriction #-}
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
    , usingExternalMap
    , shellFinder
    , defaultFinder
    , findProgram
    ) where

import Data.Monoid
import System.IO.Error
import EitherExtras
import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Control.Applicative
import Data.List.Split
import Control.Monad.Trans.Except
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
import qualified Data.Map.Strict as M

type EitherIO s = ExceptT s IO

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

defaultFinder :: Settings -> [T.Text] -> T.Text -> IO FilePath
defaultFinder s rs n = case rs of
                          [] -> pure $ T.unpack ""
                          [a] -> pure $ T.unpack a
                          _ -> enumeratePrint (listColour s)  rs >> askMore (queryColour s)  >>= defaultFinder s rs


enumeratePrint :: MonadIO m => [SGR] -> [T.Text] -> m ()
enumeratePrint fc s = liftIO $ setSGR fc >> (mapM_ TIO.putStrLn . zipWith (\a b -> a <> ": " <> b) (map (T.pack . show) [0..])) s


askMore :: MonadIO m => [SGR] -> m T.Text
askMore fc = liftIO $ setSGR fc >> TIO.putStrLn "? " >> TIO.getLine


--shellFinder' ::  String -> [T.Text] -> T.Text -> IO (Either String FilePath)



shellFinder s rs n = ExceptT $  catch (Right <$> readCreateProcess(shell (s ++ " '" ++ T.unpack n ++ " '"))  (unlines$ map T.unpack rs)) ((return . Left . errorString) :: IOError -> IO (Either String String))
  where errorString e | "127" `isInfixOf` (show e) = "The path for the external program is not correct"
                      | otherwise = "File not found"


keepSearching fp rs n = case rs of
                        [] -> throwError $ "Cannot find the file " ++ T.unpack n
                        [a] -> pure $ T.unpack a
                        _ -> init <$> fp rs n
                        {-
                        _ -> do x <- init <$> fp rs n
                                if x == "" then throwError "Cannot find the file" else return x
                                -}



vivid :: Color -> [SGR]
vivid x = [SetColor Foreground Vivid x]

fg :: ColorIntensity -> Color -> [SGR]
fg v c = [SetColor Foreground v c]


bg :: ColorIntensity -> Color -> [SGR]
bg v c = [SetColor Background v c]

restrict :: T.Text -> [T.Text] -> [T.Text]
restrict s ss | all isDigit s' = [ss !! read s']
              | otherwise = filter (match s) ss
              where s' = T.unpack s


match :: T.Text -> T.Text -> Bool
match s t = all (`T.isInfixOf` T.toLower t) $ T.words $ T.toLower s

getExtension :: FilePath -> String
getExtension = last . splitOn "."


type FileExtension = String
type FileAssociation f = FileExtension -> f (FilePath -> IO ())


usingExternalMap :: M.Map String String -> FileAssociation Maybe
usingExternalMap m = fmap strToIOCmd . flip M.lookup m

usingList :: [FileAssociation Maybe] -> FileAssociation Maybe
usingList fs s = foldl1' (<|>) $ map ($ s) fs


simpleCommand :: Alternative f => FilePath -> [FileExtension] -> FileAssociation f
simpleCommand p es e |  e `elem` es = pure (strToIOCmd p)
                     | otherwise = empty

strToProcess p fp = proc h (t++[fp])
  where (h:t) = words p

strToIOCmd :: String -> FilePath -> IO ()
strToIOCmd p = (>> return ()) . createProcess . strToProcess p

(.=) :: Alternative f => FilePath -> [FileExtension] -> FileAssociation f
(.=) = simpleCommand

usingMonoid  :: (FileExtension -> Alt Maybe (FilePath -> IO ())) -> FileAssociation Maybe
usingMonoid x = getAlt . x



findProgram :: MaybeType f => FileAssociation f -> String -> Either String (IO (), FilePath)
findProgram m s = ( \t -> (t s, s) ) <$> maybeToEither ("Could not find a suitable program for the extension, " ++ ext) (m ext)
  where ext = getExtension s


findProg :: MaybeType f => FileAssociation f -> String -> EitherIO String (IO (), FilePath)
findProg m = hoistEither . findProgram m


opeFile s' fp f m t = do fs <- (if loadEntireDatabase s' then getFirstList' else getFirstList) f s
                         r <- runExceptT $ keepSearching fp fs s >>= findProg m
                         case r of
                            Right (x, s'') -> do x
                                                 setSGR (outputColour s')
                                                 putStrLn s''
                            Left s -> setSGR (errorColour s') >> putStrLn s
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
    unless eof $ do
        s <- lift $ TIO.hGetLine h
        yield s
        readFile' h

read' :: FilePath -> Producer T.Text IO ()
read' file = do
    h <- lift $ openFile file ReadMode
    readFile' h
    lift $ hClose h


--- Experimental...for printer dialogue

hoistEither = ExceptT . return

printQ s' fp f f' t = do fs <- (if loadEntireDatabase s' then getFirstList' else getFirstList) f s
                         r <- runExceptT (keepSearching fp fs s)
                         case r of
                          Right x -> appendFile f' (x ++ "\n")
                          Left s -> setSGR (errorColour s') >> putStrLn s
  where s = T.pack t
