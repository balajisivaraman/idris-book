module Chapter4

import Data.Vect as V

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double
%name Shape shape, shape1, shape2

-- Both of these are the same
-- This second syntax is more flexible for defining dependent types
-- data Shape : Type where
--      Triangle : Double -> Double -> Shape
--      Rectangle : Double -> Double -> Shape
--      Circle : Double -> Shape

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture
%name Picture pic, pic1, pic2, pic3

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

test_picture : Picture
test_picture = Combine (Translate 5 5 rectangle)
               (Combine (Translate 35 5 circle)
               (Translate 15 25 triangle))

picture_area : Picture -> Double
picture_area (Primitive x) = area x
picture_area (Combine x y) = picture_area x + picture_area y
picture_area (Rotate _ y) = picture_area y
picture_area (Translate _ _ z) = picture_area z

data Biggest = NoTriangle
             | Size Double

biggest : Biggest -> Biggest -> Biggest
biggest NoTriangle y = y
biggest x NoTriangle = x
biggest (Size x) (Size y) = if x > y then Size x else Size y

biggest_triangle : Picture -> Biggest
biggest_triangle (Primitive (Rectangle x y)) = NoTriangle
biggest_triangle (Primitive (Circle x)) = NoTriangle
biggest_triangle (Primitive pic) = Size (area pic)
biggest_triangle (Combine pic pic1) = biggest (biggest_triangle pic) (biggest_triangle pic1)
biggest_triangle (Rotate x pic) = biggest_triangle pic
biggest_triangle (Translate x y pic) = biggest_triangle pic


data Expr = ENum Int
          | EAdd Expr Expr
          | ESub Expr Expr
          | EMul Expr Expr
%name Expr expr, expr1, expr2


evalExpr : Expr -> Int
evalExpr (ENum x)          = x
evalExpr (EAdd expr expr1) = evalExpr expr + evalExpr expr1
evalExpr (ESub expr expr1) = evalExpr expr - evalExpr expr1
evalExpr (EMul expr expr1) = evalExpr expr * evalExpr expr1

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing y = y
maxMaybe x Nothing = x
maxMaybe xval@(Just x) yval@(Just y) = if x > y then xval else yval

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} i xs = case integerToFin i n of
                         Nothing => Nothing
                         (Just x) => Just $ V.index x xs

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                (Just x) => Just $ (V.index x xs) + (V.index x ys)

sumInputs : Integer -> String -> Maybe (String, Integer)
sumInputs sum istr = let i = cast istr in
                         if i < 0 then Nothing
                         else let newSum = sum + i in
                                  Just ("Subtotal: " ++ show newSum ++ "\n", newSum)

main : IO ()
main = replWith 0 "Enter a value: " sumInputs
