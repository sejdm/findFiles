{-# LANGUAGE NoMonomorphismRestriction #-}
module FileSearch
    (
      opeFile
    , FileAssociation
    , defaultSettings
    , vivid
    , listFgColour
    , listBgColour
    , queryFgColour
    , queryBgColour
    , errorFgColour
    , errorBgColour
    ) where

import Data.Char
import Data.List
import Data.List.Split
import Control.Monad.Trans.Either
import Control.Monad
import Control.Monad.IO.Class
import System.Process
import qualified Pipes.Text.IO as PT
import Pipes
import Pipes.Safe
import qualified Pipes.Text as PTE
import qualified Pipes.Prelude as R
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO 
import System.Console.ANSI

type EitherIO = EitherT String IO

data Settings = Settings {
    listFgColour :: (ColorIntensity, Color)
  , listBgColour :: (ColorIntensity, Color)
  , queryFgColour :: (ColorIntensity, Color)
  , queryBgColour :: (ColorIntensity, Color)
  , outputBgColour :: (ColorIntensity, Color)
  , outputFgColour :: (ColorIntensity, Color)
  , errorBgColour :: (ColorIntensity, Color)
  , errorFgColour :: (ColorIntensity, Color)
                         }


defaultSettings = Settings
  {
 listFgColour = (Vivid, Green), listBgColour = (Vivid, Black), queryFgColour = (Vivid, Yellow), queryBgColour = (Vivid, Black), outputBgColour = (Dull, Black), outputFgColour = (Vivid, Green), errorBgColour = (Dull, Black), errorFgColour = (Vivid, Red)
}

keepSearching s ns = runEitherT . keepSearching' s ns

keepSearching' :: Settings -> [String] -> String -> EitherIO String
keepSearching' s ns n = case rs of
                          [] -> left $ "Cannot find the file " ++ n
                          [a] -> pure a
                          _ -> enumeratePrint (listFgColour s) (listBgColour s) rs >> askMore (queryFgColour s) (queryBgColour s) >>= keepSearching' s rs
  where rs = restrict n ns

--enumeratePrint :: [SGR] -> [SGR] -> [String] -> EitherIO ()
enumeratePrint fc bc s = liftIO $ fg fc >> bg bc >> (mapM_ putStrLn . zipWith (\a b -> a ++ ": " ++ b) (map show [0..])) s

askMore fc bc = liftIO $ fg fc >> bg bc >> putStrLn "? " >> getLine

vivid x = [SetColor Foreground Vivid x]
fg (v, c) = setSGR [SetColor Foreground v c]
bg (v, c) = setSGR [SetColor Background v c]

restrict :: String -> [String] -> [String]
restrict s ss | all isDigit s = [ss !! (read s)]
              | otherwise = filter (match s) ss

match :: String -> String -> Bool
match s t = all (flip isInfixOf (map toLower t)) $ words $ map toLower s

match' :: T.Text -> T.Text -> Bool
match' s t = all (flip T.isInfixOf (T.toLower t)) $ T.words $ T.toLower s

getExtension :: FilePath -> String
getExtension = last . splitOn "."

runProgram :: FileAssociation -> String -> EitherIO ()
runProgram m s = case M.lookup (getExtension s) m of
  Nothing -> left "Could not find a suitable program"
  Just x -> liftIO $ callProcess x [s]


findProgram :: FileAssociation -> String -> Either String (String, FilePath)
findProgram m s = let ext = getExtension s in
                    case M.lookup ext m of
                      Nothing -> Left $ "Could not find a suitable program for the extension " ++ ext
                      Just x -> pure (x, s)

findProg m = hoistEither . findProgram m


type FileAssociation = M.Map String FilePath

getProf :: Settings -> [String] -> FileAssociation -> String -> EitherIO ()
getProf s' fs m s = (keepSearching' s' fs s) >>= runProgram m

opeFile s' f m s = do fs <- getFirstList f s
                      r <- runEitherT $ (keepSearching' s' fs s) >>= findProg m
                      case r of
                        Right (x, s) -> let (h:t) = words x in spawnProcess h (t++[s]) >> fg (outputFgColour s') >> bg (outputBgColour s') >> putStrLn s
                        Left s -> fg (errorFgColour s') >> bg (errorBgColour s') >> hPutStrLn stdout s


firstListPipe :: FilePath -> String -> Producer String IO ()
firstListPipe f s = read' f >-> R.filter ( match' (T.pack s)) >-> R.map T.unpack

getFirstList :: FilePath -> String -> IO [String]
getFirstList f = R.toListM . firstListPipe f


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
