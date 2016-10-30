module Chapter3

import Data.Vect

invert : Bool -> Bool
invert False = True
invert True = False

describe_list : List Int -> String
describe_list [] = "Empty List"
describe_list (x :: xs) = "Non-empty, tail = " ++ show xs

word_lengths : List String -> List Nat
word_lengths [] = []
word_lengths (word :: words) = length word :: word_lengths words

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

mutual
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = not (isEven k)

  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k

fourInts : Vect 4 Int
fourInts = [0, 1, 2, 3]

word_lengths_vec : Vect len String -> Vect len Nat
word_lengths_vec [] = []
word_lengths_vec (word :: words) = length word :: word_lengths_vec words

insert : Ord elem => (x : elem) -> (xs_sorted : Vect k elem) -> Vect (S k) elem
insert x [] = [x]
insert x (y :: xs) =  if x < y then x :: y :: xs
                               else y :: insert x xs

ins_sort : Ord elem => Vect n elem -> Vect n elem
ins_sort [] = []
ins_sort (x :: xs) = let xs_sorted = ins_sort xs in
                        insert x xs_sorted

list_length : List a -> Nat
list_length [] = 0
list_length (x :: xs) = 1 + list_length xs

list_reverse : List a -> List a
list_reverse [] = []
list_reverse (x :: xs) = (list_reverse xs) ++ [x]

list_map : (a -> b) -> List a -> List b
list_map _ [] = []
list_map f (x :: xs) = f x :: list_map f xs

vec_map : (a -> b) -> Vect n a -> Vect n b
vec_map _ [] = []
vec_map f (x :: xs) = f x :: vec_map f xs
