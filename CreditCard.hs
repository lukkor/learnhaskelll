module CreditCard where

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = (mod n 10):(toDigitsRev (div n 10))

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEven :: [Integer] -> [Integer]
doubleEven (x:xs) = (2 * x):(doubleOdd xs)
doubleEven [] = []

doubleOdd :: [Integer] -> [Integer]
doubleOdd (x:xs) = x:(doubleEven xs)
doubleOdd [] = []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs
  | (length xs) `mod` 2 == 0 = doubleEven xs
  | otherwise = doubleOdd xs

sumDigits :: [Integer] -> Integer
sumDigits = foldr (+) 0 . concat . map toDigits

transform :: Integer -> Integer
transform = sumDigits . doubleEveryOther . toDigits

validate :: Integer -> Bool
validate n = (transform n) `mod` 10 == 0
