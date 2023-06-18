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

-- compute the minimum value in a list
minlist :: [Int] -> Int
minlist = foldr1 (min)

-- compute the minimum value in a list
minlist' :: [Int] -> Int
minlist' = foldl1 (min)

-- reverse the list
reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++ [x]) []

-- reverse the list
reverse'' :: [a] -> [a]
-- reverse'' = foldl (\acc x -> x : acc) []
reverse'' = foldl (flip (:)) []

-- takes two strings and remove the letters in the first string from the second one
remove :: Eq s => [s] -> [s] -> [s]
remove as = foldr (\c acc -> if c `notElem` as then c : acc else acc) []

-- takes two strings and remove the letters in the first string from the second one
remove' :: Eq s => [s] -> [s] -> [s]
remove' as = foldl (\acc c -> if c `notElem` as then acc ++ [c] else acc) []

-- filter using foldr
filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if f x then x : acc else acc) []

-- filter using foldl
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldl (\acc x -> if f x then acc ++ [x] else acc) []

-- remove the adjancet duplicates
remdups :: Eq a => [a] -> [a]
remdups = foldr (\x acc -> if isNotHead x acc then x : acc else acc) []
  where
    isNotHead x [] = True
    isNotHead x xs = x /= head xs

-- remove the adjancet duplicates
remdups' :: Eq a => [a] -> [a]
remdups' = foldl (\acc x -> if isNotLast x acc then acc ++ [x] else acc) []
  where
    isNotLast x [] = True
    isNotLast x xs = x /= last xs -- needs to be last xs

-- returns the list of all initial segments of a list
inits :: [Char] -> [[Char]]
inits = foldr (\x acc -> [] : map (x :) acc) []

main :: IO ()
main = do
  putStrLn "Run function: "
  choice <- getLine
  case choice of
    "sum'" -> do
      putStrLn "Computing the sum up to: "
      n <- getLine
      let res = sum' (read n :: Int)
      putStrLn ("Sum of the number is " ++ show res)
    "sumsq" -> do
      putStrLn "Computing the sum of the squares up to: "
      n <- getLine
      let res = sumsq (read n :: Int)
      putStrLn ("Sum of the squares is " ++ show res)
    "len'" -> do
      putStrLn "Computing the length of the list w/ foldr: "
      xs <- getLine
      let list = readMaybe xs :: Maybe [Int]
      case list of
        Just lst -> do
          let res = len' lst
          putStrLn ("The length is: " ++ show res)
        Nothing -> do
          putStrLn "Please provide a list"
    "len''" -> do
      putStrLn "Computing the length of the list w/ foldl: "
      xs <- getLine
      let list = readMaybe xs :: Maybe [Int]
      case list of
        Just lst -> do
          let res = len'' lst
          putStrLn ("The length is: " ++ show res)
        Nothing -> do
          putStrLn "Please provide a list"
    "minlist" -> do
      putStrLn "Computing the minimum using foldr1: "
      xs <- getLine
      let list = readMaybe xs :: Maybe [Int]
      case list of
        Just lst -> do
          let res = minlist lst
          putStrLn ("The min is: " ++ show res)
        Nothing -> do
          putStrLn "Please provide a list"
    "minlist'" -> do
      putStrLn "Computing the minimum using foldl1: "
      xs <- getLine
      let list = readMaybe xs :: Maybe [Int]
      case list of
        Just lst -> do
          let res = minlist lst
          putStrLn ("The min is: " ++ show res)
        Nothing -> do
          putStrLn "Please provide a list"
    "reverse'" -> do
      putStrLn "Computing the reverse of the list w/ foldr: "
      xs <- getLine
      let list = readMaybe xs :: Maybe [Int]
      case list of
        Just lst -> do
          let res = reverse' lst
          putStrLn ("The reversed list is: " ++ show res)
        Nothing -> do
          putStrLn "Please provide a list"
    "reverse''" -> do
      putStrLn "Computing the reverse of the lis w/ foldl: "
      xs <- getLine
      let list = readMaybe xs :: Maybe [Int]
      case list of
        Just lst -> do
          let res = reverse' lst
          putStrLn ("The reversed list is: " ++ show res)
        Nothing -> do
          putStrLn "Please provide a list"
    "remove" -> do
      putStrLn "Gimme the first string: "
      first <- getLine
      putStrLn "Gimme the second string: "
      second <- getLine
      let res = remove first second
      putStrLn res
    "remove'" -> do
      putStrLn "Gimme the first string: "
      first <- getLine
      putStrLn "Gimme the second string: "
      second <- getLine
      let res = remove first second
      putStrLn res
    "filter'" -> do
      putStrLn "Give the list: "
      xs <- getLine
      let list = readMaybe xs :: Maybe [Int]
      case list of
        Just list -> do
          putStrLn ("New the list is: " ++ show (filter' even list))
        Nothing -> do
          putStrLn "Please provide a list"
    "filter''" -> do
      putStrLn "Give the list: "
      xs <- getLine
      let list = readMaybe xs :: Maybe [Int]
      case list of
        Just list -> do
          putStrLn ("New the list is: " ++ show (filter'' even list))
        Nothing -> do
          putStrLn "Please provide a list"
    "remdups" -> do
      putStrLn "Give the list: "
      xs <- getLine
      let list = readMaybe xs :: Maybe [Int]
      case list of
        Just list -> do
          putStrLn ("New the list is: " ++ show (remdups list))
        Nothing -> do
          putStrLn "Please provide a list"
    "remdups'" -> do
      putStrLn "Give the list: "
      xs <- getLine
      let list = readMaybe xs :: Maybe [Int]
      case list of
        Just list -> do
          putStrLn ("New list is: " ++ show (remdups' list))
        Nothing -> do
          putStrLn "Please provide a list"
    "inits" -> do
      putStrLn "Give me a string: "
      str <- getLine
      let res = inits str
      putStrLn ("New list is: " ++ show res)
    _ -> putStrLn "Not a valid option"
