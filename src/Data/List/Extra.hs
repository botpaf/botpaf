module Data.List.Extra
  ( chunksBy
  ) where

-- > take 4 $ chunksBy (Sum . Text.length) (<=6) $ repeat "aA"
-- [["aA","aA","aA"],["aA","aA","aA"],["aA","aA","aA"],["aA","aA","aA"]]

-- Build the list of chunks lazily
-- Build each chunk (strictly?) as a Difference List

chunksBy :: Semigroup g => (a -> g) -> (g -> Bool) -> [a] -> [[a]]
chunksBy _ _ []     = [[]]
chunksBy f p (x:xs) = go (f x) (x:) xs
  where
    go _ ys []     = [ys []]
    go e ys (x:xs) | p e'      =         go e'    (ys . (x:)) xs
                   | otherwise = ys [] : go (f x) (x:)        xs
      where
        e' = e <> f x
