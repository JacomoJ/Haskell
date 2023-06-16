import Text.Read (readMaybe)

-- compute the sum
sum' :: Int -> Int
sum' n = foldr (+) 0 [1 .. n]

-- compute the sum of the squares
sumsq :: Int -> Int
-- Processes as x1 \`f\` (x2 \`f\` ... (xn \`f\` z)...)
-- so the first argument is always the element in the list
sumsq n = foldr (\x acc -> x ^ 2 + acc) 0 [1 .. n]

-- compute the length of the list
len' :: [a] -> Int
len' = foldr (\_ acc -> acc + 1) 0

-- compute the length of the list
len'' :: [a] -> Int
len'' = foldl (\acc _ -> acc + 1) 0

main :: IO ()
main = do
  putStrLn "Run function: "
  choice <- getLine
  case choice of
    "1" -> do
      putStrLn "Computing the sum up to: "
      n <- getLine
      let res = sum' (read n :: Int)
      putStrLn ("Sum of the number is " ++ show res)
    "2" -> do
      putStrLn "Computing the sum of the squares up to: "
      n <- getLine
      let res = sumsq (read n :: Int)
      putStrLn ("Sum of the squares is " ++ show res)
    "3" -> do
      putStrLn "Computing the length of the list w/ foldr: "
      xs <- getLine
      let list = readMaybe xs :: Maybe [Int]
      case list of
        Just lst -> do
          let res = len' lst
          putStrLn ("The length is: " ++ show res)
        Nothing -> do
          putStrLn "Please provide a list"
    "4" -> do
      putStrLn "Computing the length of the list w/ foldl: "
      xs <- getLine
      let list = readMaybe xs :: Maybe [Int]
      case list of
        Just lst -> do
          let res = len'' lst
          putStrLn ("The length is: " ++ show res)
        Nothing -> do
          putStrLn "Please provide a list"
    _ -> putStrLn "Nothing"
