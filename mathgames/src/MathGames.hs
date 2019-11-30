module MathGames ( allbeginnum,  isgoodnumber, diffdigits, sumTo186, fib, fac, luk) where

-- Factorial
fac 0 = 1
fac n = n * fac (n - 1)

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
