import Data.List
import Data.Char

-- problem 1
problem1' :: [Int] -> Int -> Int
problem1' [] a = a
problem1' (x:xs) a
  | x `mod` 3 == 0 || x `mod` 5 == 0 = problem1' xs $ a+x
  | otherwise = problem1' xs a

problem1 = problem1' [1..999] 0

-- problem 2
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
problem2 = sum (filter even $ (takeWhile (<= 4000000) fibs))

-- problem 3
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]
 
primeFactors n = factor n primes
  where factor n (p:ps) 
          | p*p > n        = [n]
          | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
          | otherwise      = factor n ps

problem3 = last $ primeFactors 600851475143

-- problem 4
problem4 = maximum [ x | y <- [100..999],
                     z <- [100..999],
                     let x = y*z,
                     let s = show x, s == reverse s ]

-- problem 5
problem5 = foldr1 lcm [1..20]

-- problem 6
problem6 = 
  abs $ sumofsquares - squareofsum
  where
    sumofsquares = sum $ map (^2) [1..100]
    squareofsum  = (sum [1..100]) ^ 2

-- problem 7
problem7 = primes !! 10000

-- problem 8
problem8'' :: [Int] -> Int -> Int
problem8'' [] curmax = curmax
problem8'' (x:xs) curmax
  | curmax < prod = problem8'' xs prod
  | otherwise     = problem8'' xs curmax
    where prod    = product $ x : (take 4 xs)

problem8' :: [Int] -> Int
problem8' xs = problem8'' xs 0

main_problem8 = do f <- readFile "1000digits.txt"
                   let digits = map digitToInt $ concat $ lines f
                   print $ problem8' digits

problem8 = main_problem8

-- problem 9
pythagorean_triplets tot = [ [a,b,c] | a <- [1..999],
                                       b <- [1..999],
                                       let c = floor . sqrt . fromIntegral $ (a^2 + b^2),
                                       a + b + c == tot,
                                       a^2 + b^2 == c^2 ]
problem9 = product . head . pythagorean_triplets $ 1000

-- problem 10
problem10 = sum $ takeWhile (<=2000000) primes


-- problem 16
problem16 = sum . map (digitToInt) $ show (2^1000)

--problem 20
problem20 = sum . map (digitToInt) $ show (factorial' 100)
  where factorial' n
          | n == 1    = 1
          | otherwise = n * factorial' (n-1)
