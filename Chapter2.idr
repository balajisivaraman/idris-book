module Chapter2

||| Calculate the average length of words in a string
||| @str a string containing words separated by whitespace
export
average : (str : String) -> Double
average str = let num_words = word_count str
                  total_length = sum (word_lengths (words str)) in
                  cast total_length / cast num_words
where
  word_count : String -> Nat
  word_count str = length (words str)

  word_lengths : List String -> List Nat
  word_lengths strs = map length strs

showAverage : String -> String
showAverage str = "The average word length is: " ++
                   show (average str) ++ "\n"

palindrome : Nat -> String -> Bool
palindrome nat str = if ((length str) > nat) then (toLower str) == (reverse (toLower str)) else False

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

top_ten : Ord a => List a -> List a
top_ten lst = take 10 $ reverse $ sort lst

over_length : Nat -> List String -> Nat
over_length nat lst = length $ filter (\s => (length s) > nat) lst

main : IO ()
main = repl "Enter a string: " showAverage
