{-# LANGUAGE OverloadedStrings, FlexibleContexts, NoMonomorphismRestriction #-}
module Main where

import FileSearch
import System.Environment
import System.Console.ANSI
import Data.Monoid
import qualified Data.Yaml as Y
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.Libyaml


fileTypes = usingMonoid $
    "zathura" .= ["pdf", "djvu", "ps", "dvi"]
  -- <> "screen -d -m mpv -input file=/home/shane/mplayerfifo" .= ["mp3", "mp4", "webm", "flv", "wav", "avi"]
  <> "tmux new -s Media -d mpv --audio-display=no -input file=/home/shane/mplayerfifo" .= ["mp3", "mp4", "webm", "flv", "wav", "avi","mkv"]
  <> "tmux new -s Media -d mpv --audio-display=no -input file=/home/shane/mplayerfifo -playlist" .= ["pls"]
  <> "mcomix" .= ["cbr", "cbz"]
  <> "emacs24" .= ["tex", "hs"]
  <> "ebook-viewer" .= ["epub", "mobi", "lit"]
  <> "chromium-browser" .= ["html"]


--finder = defaultFinder mySettings
finder = shellFinder "fzf --height=10% -q"
fileDB = "/home/shane/db/files"
printQFile = "/home/shane/printQ"


mySettings = defaultSettings {
    listColour = fg Dull Blue
  , queryColour = fg Dull Green
  , errorColour = fg Vivid Red
  , loadEntireDatabase = False
                             }

toEitherString (Right y) = Right y
toEitherString (Left x)  = Left $ parseExcStr x

parseExcStr Y.NonScalarKey = "Non scalar key"
parseExcStr (Y.UnknownAlias a) = "Unknown alias: " ++ a
parseExcStr (Y.InvalidYaml (Just (YamlException x))) = "Yaml exception: " ++ x
parseExcStr (Y.InvalidYaml (Just (YamlParseException x y (YamlMark a b c)))) =  x ++ ": " ++ y ++ ". The error is at index: " ++ show a ++ ", line: " ++ show b ++ ", column: " ++ show c
parseExcStr z = show z


fromEitherIO (Right x) = x
fromEitherIO (Left e) = putStrLn e

fromJust _ (Just x) = x
fromJust d Nothing = error d

opeFileExt s a v = opeFile s <$> (shellFinder <$> (v Y..: "finder")) <*> v Y..: "fileDB" <*> (usingExternalMap <$> (v Y..: "extensions")) <*> pure a

opeFileEither s a = Y.parseEither (opeFileExt s a)



printQExt s a v = printQ s <$> (shellFinder <$> (v Y..: "finder")) <*> v Y..: "fileDB" <*> (v Y..: "printQFile") <*> pure a

printQEither s a = Y.parseEither (printQExt s a)

main :: IO ()
main = do a@(n:m) <- getArgs
          ft <- Y.decodeFileEither "/home/shane/.fileExtensions.yml"

          fromEitherIO (toEitherString ft >>= (if n /= "p" then  opeFileEither else  printQEither) mySettings (unwords a))
          --if n /= "p" then opeFile mySettings finder fileDB (usingExternalMap $ extensions) ( unwords a) else  printQ mySettings finder fileDB printQFile (unwords m)
