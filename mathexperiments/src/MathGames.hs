module MathGames ( allbeginnum,  isgoodnumber, diffdigits, sumTo186, fib, fac, luk, samebirthday, bincoeff, fish_in_pond, digitsum) where

-- Factorial
fac :: Integer -> Integer
fac 0 = 1
fac n = product [1 .. n]

-- Fibonachi numbers
fib n = snd $ help n (1,0) where 
    help 0 (x,y) = (x,y)
    help n (x,y) = help (n -1) (y, x+y)

-- Lucas numbers
luk :: Word -> Integer
luk n = snd $ help n (1,2) where 
    help 0 (x, y) = (x, y)
    help n (x,y) = help (n - 1) (x+y, x)


-- Numbers with different digits
nmbs :: Int -> [[Int]]
nmbs 1 = map return [1 .. 9]
nmbs n = [x : n | n <- nmbs (n - 1), x <- [0 .. 9], x `notElem` n]

allNmbs :: Int -> [[Int]]
allNmbs 1 = nmbs 1
allNmbs n = allNmbs (n - 1) ++ nmbs n

eval :: [Int] -> Int
eval = snd . foldl (\(m, v) x -> (m * 10, x * m + v)) (1, 0)

-- Generates list of number with different digits
diffdigits = map eval $ allNmbs 10


-- Begin number is a number gotten by removing all last digests --
allbeginnum :: Int -> [Int]
allbeginnum x = if x < 10 
                then [] 
                else let y = x `div` 10 in y : (allbeginnum y)

-- Is the digit devided by all begin numbers
isgoodnumber :: Int -> Bool
isgoodnumber x = all (\t -> x `mod` t == 0) $ allbeginnum x

-- Max 6 digit number that divided by all it begins number
max6digit = undefined

-- 186 into 186 = x + y + z and sum of each 2 is divided by the third
sumTo186 = [(x,y,z)| x<-[1..186], y<-[1..186], let z = 186-x-y, y >= x, z >= y, (x + y) `mod` z == 0, (x + z) `mod` y == 0, (z+y) `mod` x == 0]

-- Birthday paradox
samebirthday n = 1.0 - (fromIntegral $ product $ map (\i -> (365 -i + 1)) [1 .. n])/(fromIntegral $ 365^n)

-- Binomial coefficient
bincoeff :: Integer -> Integer -> Integer
bincoeff n m = (fac n) `div` ((fac m) * fac (n - m))

-- Fish in a pond
-- How many fishes are in the pond if in
-- every 15 fishes with high probability we get 5 marked ones?
fish_in_pond n = (fromIntegral sizeA) / (fromIntegral sizeOmega) where 
    sizeA = (bincoeff 15 5) * (bincoeff (n - 15) 10)
    sizeOmega =  bincoeff n 15


-- Calculate a sum of digits in a number
digitsum :: Integer -> Integer
digitsum x 
  | x == 0 = 0
  | x < 0 = undefined
  | otherwise = (x `mod` 10) + digitsum (x `div` 10)

