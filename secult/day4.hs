module Day4 (
              askForName,
              )
  where

main = do
  putStrLn "Please enter your name: "
  name <- getLine
  putStrLn ("Hello, " ++ name ++ ", how are you?")

doGuessing num=  do
      putStrLn "find a number"
      guess <- getLine
      if (read guess < num)
      then do
        putStrLn "Too low!"
        doGuessing num
      else if (read guess > num)
           then do
             putStrLn "Too high!"
             doGuessing num
           else putStrLn "You Win!"

isIn :: Eq a => a -> [a] -> Bool
isIn a [] = False
isIn a (x:xs) | (a == x) = True
              | otherwise = (isIn a xs)



askForName = do
  putStrLn "whats your name"
  name <-getLine
  if ( name `isIn` ["John", "Simon","Phil"] )
  then putStrLn "haskell is a greate lang"
  else if (name == "Koen")
  then putStrLn "from the debug team"
  else putStrLn "dont know you"


lCaseString = map Data.toLower
