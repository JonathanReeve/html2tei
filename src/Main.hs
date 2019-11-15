
-- -*- dante-command-line: '("nix-shell" "--run" "ghci"); -*-

module Main where

import Text.XML.HXT.Core
import Text.HandsomeSoup

-- Pride and Prejudice: https://www.gutenberg.org/files/1342/1342-h/1342-h.htm
-- Dracula: https://www.gutenberg.org/files/345/345-h/345-h.htm

-- Handle <a name="#link2HCH0017"> where CH0017 stands for Chapter 17

data Novel =  Novel { title :: String,
                      chaps :: [Chapter] } deriving Show

data Chapter = Chapter [Para] deriving Show

data Para = Para String deriving Show

-- >>> main
-- Chapter [Para "Contents of chapter 2, para 1",Para "Contents of chapter 2, para 2",Para "Contents of chapter 2, para 3",Para "Contents of chapter 2, para 4",Para "Contents of chapter 3, para 1",Para "Contents of chapter 3, para 2",Para "Contents of chapter 3, para 3",Para "Contents of chapter 3, para 4"]
main :: IO ()
main = do
  contents <- readFile "src/test.html"
  let doc = parseHtml contents
  -- Get all divs that have the child <a name="">
  let chapsRaw = doc >>> css "div" >>> (ifA (css "a" >>> hasAttr "name")(this)(none))
  chaps <- runX chapsRaw
  names <- runX $ chapsRaw >>> css "a" ! "name"
  -- print $ names
  -- print chaps
  -- Gets text of all paragraphs
  paraText <- runX $ chapsRaw >>> css "p" /> getText
  let chap = Chapter [Para text | text <- paraText]
  print chap
