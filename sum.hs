import Data.Char

-- Sum all stdin Integers
main = do input <- getContents
          print . sum $ (map read $ lines input :: [Integer])
