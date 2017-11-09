main = print "Hello, world!"

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = (mod n 10):(toDigitsRev (div n 10))

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:[]) = (2 * x):[y]
doubleEveryOther (x:y:zs) = (2 * x):y:(doubleEveryOther zs)

sumDigits :: [Integer] -> Integer
sumDigits xs = foldl (+) 0 (xs >>= toDigits)

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

--------------------------------------------------------------------------------

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = (hanoi (n - 1) a c b) . (hanoi (n - 1) c b a)
