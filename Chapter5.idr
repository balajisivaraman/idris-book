module Main

import System

printLength : IO ()
printLength = getLine >>= \input => let len = length input in putStrLn $ show len

printLengthDo : IO ()
printLengthDo = do
              input <- getLine
              let len = length input
              putStrLn $ show len

printLonger : IO ()
printLonger = putStrLn "Enter the first string:" >>= \_ =>
              getLine >>= \inp1 =>
              putStrLn "Enter the second string:" >>= \_ =>
              getLine >>= \inp2 =>
              let len1 = length inp1 in
              let len2 = length inp2 in
              if len1 > len2 then
                 putStr ("The length of the longer string is " ++ (show len1) ++ "\n")
              else
                 putStr ("The length of the longer string is " ++ (show len2) ++ "\n")

printLongerDo : IO ()
printLongerDo = do
            putStrLn "Enter the first string:"
            inp1 <- getLine
            putStrLn "Enter the second string:"
            inp2 <- getLine
            let len1 = length inp1
            let len2 = length inp2
            if len1 > len2 then
               putStr ("The length of the longer string is " ++ (show len1) ++ "\n")
            else
               putStr ("The length of the longer string is " ++ (show len2) ++ "\n")

readNumber : IO (Maybe Nat)
readNumber = do
           input <- getLine
           if all isDigit (unpack input) then
              pure $ Just (cast input)
           else
              pure Nothing

readPair : IO (String, String)
readPair = do
         str1 <- getLine
         str2 <- getLine
         pure (str1, str2)

usePair : IO ()
usePair = do
          (str1, str2) <- readPair
          putStrLn ("You entered " ++ str1 ++ " and " ++ str2)

-- We can directly pattern match on values obtained by executing some action
-- Alternative case matches can be provided after the vertical bar like below
readNumbers : IO (Maybe (Nat, Nat))
readNumbers = do
              Just num1_ok <- readNumber | Nothing => pure Nothing
              Just num2_ok <- readNumber | Nothing => pure Nothing
              pure (Just (num1_ok, num2_ok))

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do
             putStrLn "Guess the number:"
             Just no <- readNumber
           | Nothing => do putStrLn "The entered input is invalid. Please try again!"
                           guess target guesses
             let newGuesses = guesses + 1
             putStrLn ("You have guessed " ++ (show newGuesses) ++ " times")
             case compare no target of
                  LT => do putStrLn "The number you have entered is too low. Please try again!"
                           guess target newGuesses
                  GT => do putStrLn "The number you have entered is too high. Please try again!"
                           guess target newGuesses
                  EQ => putStrLn "The number you have entered is correct. Congrats!"

myrepl : (prompt : String) -> (onInput : String -> String) -> IO ()
myrepl prompt onInput = do
                      putStrLn prompt
                      input <- getLine
                      putStrLn $ onInput input
                      myrepl prompt onInput

myreplWith : (state : a) -> (prompt : String) ->
             (onInput : a -> String -> Maybe (String, a)) -> IO ()
myreplWith state prompt onInput =
           do putStrLn prompt
              input <- getLine
              case onInput state input of
                   Just (output, newState)
                        => do putStrLn output
                              myreplWith newState prompt onInput
                   Nothing => putStrLn ""

main : IO ()
main = do
     rint <- time
     let no = fromInteger (rint) `modNat` 100
     guess no 0
