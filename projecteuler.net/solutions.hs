import Data.List
import Data.Char
import Data.Ord (comparing)
import Data.Maybe
import Data.Array 

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
-- http://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple
-- I'am not currently sure about the generation of k
pythagorean_triplets tot = nub . sort $
                             [ sort $ [a,b,c] | 
                                m <- [1..lim],
                                n <- [1..(m-1)],
                                k <- [1..lim],
                                let a   = k * (m^2 - n^2),
                                let b   = k * (2 * m * n),
                                let c   = k * (m^2 + n^2),
                                a + b + c == tot ]
  where lim = floor . sqrt . fromIntegral $ tot
problem9 = product . head . pythagorean_triplets $ 1000

-- problem 10
problem10 = sum $ takeWhile (<=2000000) primes

-- problem 11
problem11 =
  do file <- readFile "problem11.txt"
     let content = map (map read) $ (map words $ lines file) :: [[Integer]]
     let maxH    = maximum $ map (maximum . product4) $ content
     let maxV    = maximum $ map (maximum . product4) $ transpose content
     let maxD    = maximum $ map (maximum . product4) $ transpose . transpose'    $ content
     let maxD2   = maximum $ map (maximum . product4) $ transpose . transposeInv' $ content
     print $ maximum [maxH, maxV, maxD, maxD2]
  where product4 []             = []
        product4 lst            | length lst < 4 = []
                                | otherwise      = (product $ take 4 lst) : (product4 $ tail lst)
        transpose'    lst       = transpose'' ((length $ head lst) - 1) 0 lst  1
        transposeInv' lst       = transpose'' 0 ((length $ head lst) - 1) lst (-1)
        transpose'' _ _ [] _    = []
        transpose'' b a lst dir = fill' b a (head lst) : transpose'' (b-(1*dir)) (a+(1*dir)) (tail lst) dir
        fill' nbB nbA lst       = (take nbB $ repeat 0) ++ lst ++ (take nbA $ repeat 0)

-- problem 12
problem12 = triangle_numbers !! (length $ takeWhile (<=500) tr_num_divisors) 
  where tr_num_divisors  = map (numDivisors) triangle_numbers
        triangle_numbers = map (\x -> sum $ take x [1..]) [1..]
        numDivisors n    = product [ toInteger (a+1) | 
                             (p,a) <- gatherPrimeFactors . primeFactors $ n ]
        gatherPrimeFactors lst = nub $ [ (a,n) | a <- lst, n <- [length $ filter (==a) lst] ]

-- problem 13
problem13 = do f <- readFile "5000digits.txt"
               let lst = (map read (lines $ f)) :: [Integer]
               print . take 10 . show . sum $ lst

-- problem 14 - right but to parallelize ...
problem14' []         = []
problem14' lst@(1:xs) = lst
problem14' lst@(x:xs) = problem14' $ n:lst
  where n | even x    = x `div` 2
          | otherwise = x*3 + 1

problem14 = do
  let a = solve [1..100000]
  let b = solve [100001..200000]
  let c = solve [200001..300000]
  let d = solve [300001..400000]
  let e = solve [400001..500000]
  let f = solve [500001..600000]
  let g = solve [600001..700000]
  let h = solve [700001..800000]
  let i = solve [800001..900000]
  let j = solve [900001..999999]
  print $ a
  print $ b
  print $ c
  print $ d
  print $ e
  print $ f
  print $ g
  print $ h
  print $ i
  print $ j
  print $ fst $ maximumBy (comparing snd) [a,b,c,d,e,f,g,h,i,j]
  where solve l = maximumBy (comparing snd) $ map (\x -> (x,length . problem14' $ [x])) l

-- problem 15
-- permutations with repeated elemeents
-- 20 rights and 20 down
-- total length = 40
-- problem15 = length! / 20!20!
-- problem15 = f 40 `div` (f 20 * f 20)
problem15 = product [21..40] `div` product [1..20]

-- problem 16
problem16 = sum . map (digitToInt) $ show (2^1000)

-- problem 17
problem17 = sum $ map (length . get_in_letters) [1..1000]

one_to_nineteen = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
                   "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen",
                   "eighteen", "nineteen"]
ty_suffix       = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eigthy", "ninety"]

get_in_letters :: Int -> [Char]
get_in_letters n
  | n < 20             = one_to_nineteen !! n
  | n >= 20 && n < 100 = ty_suffix !! (n `div` 10) ++ one_to_nineteen !! (n `mod` 10)
  | n >= 100 && n < 1000 && n `mod` 100 /= 0 = gethundred n ++ "and" ++ get_in_letters (n `mod` 100)
  | n /= 1000 && n `mod` 100 == 0 = gethundred n
  | n == 1000 = "onethousand"
  where gethundred n' = one_to_nineteen !! (n' `div` 100) ++ "hundred"

-- problem 18
problem18 = do f <- readFile "triangle-p18.txt"
               let l = map (map read) $ (map words $ lines f) :: [[Integer]]
               print $ l

-- problem 20
problem20 = sum . map (digitToInt) $ show (product [1..100])

-- problem 21
problem21 = sum $ [ a | a <- [1..10000], let b = d a, b > 1, b < 10000, d b == a, a /= b ]
  where d n = sum $ divisors n
divisors n = [ r | r <- [1..(n `div` 2)], n `mod` r == 0]

-- problem 22
problem22 = do file <- readFile "names.txt"
               let names = sort $ read $ "[" ++ file ++ "]" :: [String]
               let score = sum $ zipWith computeScore [1..] names
               print $ score
               where computeScore n lst = (*) n . sum . map alphaAscii $ lst
                     alphaAscii x = ord x - ord 'A' + 1

-- problem 25
problem25 = length $ takeWhile (<limit) fibs
            where limit = 10^999

-- problem 39
problem39 = fromJust (elemIndex (maximum lst) lst) + 1
  where lst            = map getNbSolutions $ [1..1000]
        getNbSolutions = length . pythagorean_triplets

-- problem 40
problem40 = product . map irrational_decimal $ exp
  where irrational_decimal n = digitToInt $ (concat . map show $ [1..]) !! (n-1)
        exp                  = take 7 . map (\x -> 10^x) $ [0..]

-- problem 48
problem48 :: Integer
problem48 = read . concat . filter last_ten . tails . show . sum . take n $ series
  where n = 1000
        last_ten x = length x == 10
        series = zipWith (^) [1..] [1..] 

