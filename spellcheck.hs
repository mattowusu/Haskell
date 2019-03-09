-- see repo below for list of words:
-- https://github.com/dwyl/english-words

import Trie
import Data.Char

printmsg :: Maybe Bool -> String
printmsg (Just True) = "Yes! That is a real word"
printmsg _           = "No. That is not a word :("

main = do
  allText <- readFile "words.txt"
  putStrLn "What word shall I check?"
  let
    allWords = lines allText
    theTrie = addAll True allWords
  word <- getLine
  let
    lastTrie = getTrie theTrie (fmap toLower word)
  putStrLn (printmsg (getVal lastTrie))
  -- putStrLn "Here are some suggestions:"
  -- putStrLn (word ++
  return ()
