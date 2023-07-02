import FSEntry

fse1 :: FSEntry
fse1 = Folder "root" [Folder "sub1" [File "file1" "content1", File "file2" "content2"], File "file3" "content3"]

fse2 :: FSEntry2 String
fse2 = Folder2 "root" [Folder2 "sub1" [File2 "file1" "content1", File2 "file2" "content2"], File2 "file3" "content3"]

fse3 :: FSEntry
fse3 = Folder "Home" [Folder "Work" [File "students.txt" "Alice, Bob", File "hint" "You can use fFSE!"], Folder "Empty" [], File "Fun" "FMFP"]

main :: IO ()
main = do
  print (fFSE (const concat) (const id) fse1)