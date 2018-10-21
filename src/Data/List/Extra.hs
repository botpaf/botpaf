{-# LANGUAGE ViewPatterns #-}

module Data.List.Extra
  ( chunksOf
  ) where

import Data.List ( foldl' )


-- TODO doesn't work with infinite sequences
--
-- chunksOf is strict in its arguments
--
-- Example:
--
-- > chunksOf (Sum . Text.length) (<=6) $ take 12 $ cycle ["aA"]
-- [["aA","aA","aA"],["aA","aA","aA"],["aA","aA","aA"],["aA","aA","aA"]]
--

chunksOf :: (Foldable t, Monoid m)
         => (a -> m) -> (m -> Bool) -> t a -> [[a]]
chunksOf f p xs = snd $ foldl' (collect p) (mempty,[]) xs

  where

    collect p (m,xss) x@(f -> fx)
      | p $ m <> fx = ( m <> fx, augment x xss )
      | otherwise   = (      fx,   begin x xss )

    augment x []       = [[x]]
    augment x (xs:xss) = (x:xs):xss

    begin x xss = [x]:xss

    -- version with accumulator being a triple
    --
    -- collect p (m,xs,xss) x@(f -> fx)
    --   | p $ m <> fx = ( m <> fx, x:xs,    xss )
    --   | otherwise   = (      fx,  [x], xs:xss )
