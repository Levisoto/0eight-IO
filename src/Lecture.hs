module Lecture where

main = do
  -- putStrLn "Hello, World!"
  -- name <- getLine
  -- putStrLn $ "Hey " ++ name ++ ", you rock!"
  line <- getLine
  if null line
  then return ()
  else do
    putStrLn $ reverseWords line
    main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

