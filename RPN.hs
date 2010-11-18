import Data.Either

isLeft :: Either a b -> Bool
isLeft (Left a) = True
isLeft _        = False

printStack :: Either String [Double] -> IO ()
printStack (Right x) = putStrLn $ "Stack = " ++ (show x)
printStack (Left x)  = putStrLn $ x

printLastValue :: Either String [Double] -> IO ()
printLastValue (Left x)  = putStrLn x
printLastValue (Right x) = putStrLn . show . head $ x

printLastError :: Either String [Double] -> IO ()
printLastError e@(Left x) = printLastValue e
printLastError _ = return ()


push :: String -> Either String [Double] -> Either String [Double]
push a (Right [])   = Right $ [(read a)]
push a (Right [x])  = Right $ (read a):[x]
push "/" (Right (0:xs)) = Left "Error: divide by zero"
push a (Right lst@(x:y:xs)) = case a of
  "+"   -> Right $ ((+) y x):xs
  "-"   -> Right $ ((-) y x):xs
  "/"   -> Right $ ((/) y x):xs
  "*"   -> Right $ ((*) y x):xs
  "sum" -> Right $ [(sum lst)]
  _     -> Right $ (read a):lst

pop :: Either String [Double] -> Either String [Double]
pop (Right (x:xs)) = Right xs


mainLoop :: Either String [Double] -> IO()
mainLoop stack = do
  entry <- getLine
  st <- case entry of
          "P" -> do printLastValue stack
                    return stack
          "p" -> do printLastValue stack
                    return . pop $ stack
          "f" -> do printStack stack
                    return stack
          _   -> return $ push entry stack

  
  printLastError st
  mainLoop (if isLeft st then stack else st)

main = do
  mainLoop $ Right []
