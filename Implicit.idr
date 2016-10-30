module Implicit

import Data.Vect

-- n, m, elem are unbound implicits
-- Below is converted interanally by Idris into Bound implicits like below.
-- reverse : List elem -> List elem
-- append : Vect n elem -> Vect m elem -> Vect (n + m) elem

-- n, m, elem are bound implicits
reverse : {elem : Type} -> List elem -> List elem
append : {elem : Type} -> {n : Nat} -> {m : Nat} ->  Vect n elem -> Vect m elem -> Vect (n + m) elem

my_length : Vect n elem -> Nat
my_length [] = Z
my_length (x :: xs) = 1 + length xs

-- We can refer to n directly using implicit syntax
my_implicit_length : Vect n elem -> Nat
my_implicit_length {n} xs = n
