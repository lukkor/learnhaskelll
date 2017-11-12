{-# OPTIONS_GHC -Wall #-}

module Golf where

everyNth :: Int -> [a] -> [a]
everyNth n xs = case splitAt (n - 1) xs of
                  (_, (y:ys)) -> y : everyNth n ys
                  _           -> []

skips :: [a] -> [[a]]
skips [] = []
skips xs = map (\n -> everyNth n xs) [1..length xs]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | x < y && y > z = y:localMaxima (y:z:zs)
  | otherwise      = localMaxima (y:z:zs)
localMaxima _ = []
