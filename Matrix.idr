module Matrix

import Data.Vect

addMatrix : Num num => Vect rows (Vect cols num) -> Vect rows (Vect cols num) -> Vect rows (Vect cols num)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

-- We have to bring the implicit {n} into scope before case splitting on it
create_empties : Vect n (Vect 0 elem)
create_empties {n = Z} = []
create_empties {n = (S k)} = [] :: create_empties

transpose_helper : (x : Vect n elem) -> (xs_transpose : Vect n (Vect k elem)) -> Vect n (Vect (S k) elem)
transpose_helper = zipWith (::)
-- transpose_helper [] [] = []
-- transpose_helper (x :: xs) (y :: ys) = (x :: y) :: transpose_helper xs ys

transposeMatrix : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMatrix [] = create_empties
transposeMatrix (x :: xs) = let xs_transpose = transposeMatrix xs in
                                transpose_helper x xs_transpose

total multiplyOneRow : Num num => Vect m num -> Vect p (Vect m num) -> Vect p num
multiplyOneRow xs [] = []
multiplyOneRow xs (x :: ys) = (foldl (+) 0 (zipWith (*) xs x)) :: multiplyOneRow xs ys

total multiplyTransposedMatrices : Num num => Vect n (Vect m num) -> Vect p (Vect m num) -> Vect n (Vect p num)
multiplyTransposedMatrices [] ys = []
multiplyTransposedMatrices (x :: xs) ys = multiplyOneRow x ys :: multiplyTransposedMatrices xs ys

total multMatrix : Num num => Vect n (Vect m num) -> Vect m (Vect p num) -> Vect n (Vect p num)
multMatrix xs ys = multiplyTransposedMatrices xs (transposeMatrix ys)
