module Ch6Ex

import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix r c = Vect r (Vect c Double)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

TupleVect : (numargs : Nat) -> Type -> Type
TupleVect Z ty = ()
TupleVect (S k) ty = (ty, TupleVect k ty)

test : TupleVect 4 Nat
test = (1,2,3,4,())
