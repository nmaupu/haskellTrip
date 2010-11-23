import Data.Maybe

type Stack a = [a]

push :: a -> Stack a -> Stack a
push a = (:) a

pop :: Stack a -> (a,Stack a)
pop [] = error "Empty Stack"
pop [x] = (x,[])
pop (x:xs) = (x,xs)

getOp :: (Fractional a) => String -> Maybe (a -> a -> a)
getOp "+" = Just (+)
getOp "-" = Just (-)
getOp "/" = Just (/)
getOp "*" = Just (*)
getOp _   = Nothing

applyOp :: (Fractional a) => (a -> a -> a) -> a -> a -> a
applyOp f a b = f a b

isNumeric :: String -> Bool
isNumeric s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False

isOperator :: String -> Bool
isOperator s = isJust (getOp s)

pushValOrCompute :: String -> Stack Double -> Stack Double
pushValOrCompute str s 
  | isNumeric str  = push (read str) s
  | isOperator str && length s >= 2 = push 
      (applyOp 
        (fromJust . getOp $ str) 
        (fst . pop $ s) 
        (fst . pop . snd . pop $ s)) 
      (snd . pop . snd. pop $ s)
  | otherwise = s


--

mainLoop :: Stack Double -> IO ()
mainLoop stack = do
  str <- getLine
  mainLoop $ pushValOrCompute str stack


--  str <- getLine
--  --return ()
--  mainLoop $ if isNumeric str
--    then push (read str) stack
--    else if (length stack) < 2
--      then do
--        putStrLn "Less than 2 numbers in stack"
--        return stack
--      else do
--        a <- fst . pop $ stack
--        b <- fst . pop . snd . pop $ stack
--        newStack <- snd . pop . snd . pop $ stack
--        return (push ((getOp str) a b) newStack)
--      
--    --then push (read str) stack
--    --else push (applyOp (getOp str) (fst . pop $ stack) (fst . pop $ stack)) stack
