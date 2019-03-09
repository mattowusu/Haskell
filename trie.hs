module Trie where

import qualified Data.Map as Map

type Map = Map.Map
fromJust (Just a) = a
data Trie k v = Trie (Maybe v) (Map k (Trie k v)) deriving Show


zeroTrie = Trie Nothing Map.empty

updateTrie :: Ord k => ((Maybe v) -> v) -> [k]  -> Trie k v -> Trie k v
updateTrie f [] (Trie mv ts) = Trie (Just (f mv)) ts
updateTrie f (x:xs) (Trie mv ts)
  | Map.member x ts = Trie mv (Map.insert x (updateTrie f xs (fromJust (Map.lookup x ts))) ts)
  | otherwise       = Trie mv (Map.insert x (updateTrie f xs zeroTrie) ts)

addAll :: Ord k => v -> [[k]] -> Trie k v
addAll v = foldr (updateTrie (\_ -> v)) zeroTrie

getTrie :: Ord k => Trie k v -> [k] -> Maybe (Trie k v)
getTrie (Trie mv ts) [] = Just (Trie mv ts)
getTrie (Trie mv ts) (x:xs)
  | Map.member x ts = getTrie (fromJust (Map.lookup x ts)) xs
  | otherwise = Nothing

getVal :: Ord k => Maybe (Trie k v) -> Maybe v
getVal (Just (Trie mv ts)) = mv
getVal Nothing = Nothing

-- get strings that finish word....e.g children of th is ["e", "en", "ey", "ere"]
-- getSuggestions :: Ord k => Trie k v -> [[k]]
-- getSuggestions (Trie mv ts) = foldr (++) [] (zipWith (++) (keys ts) (getSuggestions (elems ts)))

-- test stuff
myWrds = ["hello", "hell", "OK"]
testTrie = addAll True myWrds
