import GHC.Base (VecElem (Int16ElemRep))
import Text.Read (readMaybe)

-- compute head
head' :: [a] -> Maybe a
head' [] = Nothing
head' (a : _) = Just a

-- last
last' :: [a] -> Maybe a
last' [] = Nothing
last' (a : []) = Just a
last' (a : as) = last' as

-- init
init' :: [a] -> [a]
init' [] = []
init' (a : []) = []
init' (a : as) = a : init' as

-- tail
tail' :: [a] -> [a]
tail' [] = []
tail' (a : as) = as

main :: IO ()
main = do
  putStrLn "Run function: "
  choice <- getLine
  case choice of
    "head" -> do
      putStrLn "Gimme the list: "
      input <- getLine
      let list = read input :: [Int]
      print (head' list)
    "tail" -> do
      putStrLn "Gimme the list: "
      input <- getLine
      let list = read input :: [Int]
      print (tail' list)
    "init" -> do
      putStrLn "Gimme the list: "
      input <- getLine
      let list = read input :: [Int]
      print (init' list)
    "last" -> do
      putStrLn "Gimme the list: "
      input <- getLine
      let list = read input :: [Int]
      print (last' list)
    _ -> error "Not a valid option"