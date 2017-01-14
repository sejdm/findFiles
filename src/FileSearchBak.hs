{-# LANGUAGE NoMonomorphismRestriction #-}
module FileSearch
    ( 
      opeFile
    , FileAssociation
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

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type EitherIO = EitherT String IO

keepSearching ns = runEitherT . keepSearching' ns

keepSearching' :: [String] -> String -> EitherIO String
keepSearching' ns n = case rs of
                        [] -> left $ "Cannot find the file " ++ n
                        [a] -> pure a
                        _ -> enumeratePrint rs >> askMore >>= keepSearching' rs
  where rs = restrict n ns

enumeratePrint :: [String] -> EitherIO ()
enumeratePrint = liftIO . mapM_ putStrLn . zipWith (\a b -> a ++ ": " ++ b) (map show [0..])

askMore = liftIO $ putStrLn "? " >> getLine

restrict :: String -> [String] -> [String]
restrict s ss | all isDigit s = [ss !! (read s)]
              | otherwise = filter (match s) ss

match :: String -> String -> Bool
match s t = all (flip isInfixOf (map toLower t)) $ words $ map toLower s

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

getProf :: [String] -> FileAssociation -> String -> EitherIO ()
getProf fs m s = (keepSearching' fs s) >>= runProgram m

opeFile f m s = do fs <- testing2 f s
                   r <- runEitherT $ (keepSearching' fs s) >>= findProg m
                   case r of
                      Right (x, s) -> spawnProcess x [s] >> return ()
                      Left s -> putStrLn s
match' s t = match (T.unpack s) (T.unpack t)

testing :: FilePath -> String -> Producer String IO ()
testing f s = read' f >-> R.filter ( match' (T.pack s)) >-> R.map T.unpack

testing2 :: FilePath -> String -> IO [String]
testing2 f = R.toListM . testing f


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
