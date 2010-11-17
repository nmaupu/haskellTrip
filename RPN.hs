import Data.Maybe
import Control.Applicative

foldingFunc :: [Maybe Double] -> String -> [Maybe Double]
foldingFunc []  [] = []
foldingFunc xs  [] = xs
foldingFunc []  s  = (Just . read $ s):[]
foldingFunc [x] s  = (Just . read $ s):[x]
foldingFunc (Just 0:_:xs) "/" = [Nothing]
foldingFunc lst@(a:b:xs) s 
  | s == "+"            = (pure (+) <*> b <*> a):xs
  | s == "-"            = (pure (-) <*> b <*> a):xs
  | s == "/"            = (pure (/) <*> b <*> a):xs
  | s == "*"            = (pure (*) <*> b <*> a):xs
  | s == "sum"          = [Just . sum $ fmap fromJust lst]
  | otherwise           = (Just . read $ s):lst

computeRPN :: String -> Maybe Double
computeRPN s = head . foldl foldingFunc [] $ words s
