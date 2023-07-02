module FSEntry where

data FSEntry = Folder String [FSEntry] | File String String

data FSEntry2 name = Folder2 name [FSEntry2 name] | File2 name String

fFSE :: (String -> [a] -> a) -> (String -> String -> a) -> FSEntry -> a
fFSE f g (Folder name xs) = f name (map (fFSE f g) xs)
fFSE f g (File name text) = g name text
