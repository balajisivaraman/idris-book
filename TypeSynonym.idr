module TypeSynonym

import Data.Vect

tri : Vect 3 (Double, Double)
tri = [(0.0, 0.0), (3.0, 0.0), (0.0, 4.0)]

-- Type Synonym, similar to type alias in Scala and Haskell
Position : Type
Position = (Double, Double)

triBetter : Vect 3 Position
triBetter = [(0.0, 0.0), (3.0, 0.0), (0.0, 4.0)]

-- Type Synonym, which is actually a function which computes a type
Polygon : Nat -> Type
Polygon n = Vect n Position

triPoly : Polygon 3
triPoly = [(0.0, 0.0), (3.0, 0.0), (0.0, 4.0)]

triEval : Polygon 3
triEval = [(?triEval_rhs1, ?triEval_rhs2),
           (?triEval_rhs3, ?triEval_rhs4),
           (?triEval_rhs5, ?triEval_rhs6)]
