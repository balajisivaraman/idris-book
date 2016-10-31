module Main

import Data.Vect

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do x <- getLine
                       xs <- readVectLen k
                       pure (x :: xs)

data VectUnknown : Type -> Type where
     MkVect : (len : Nat) -> Vect len a -> VectUnknown a

-- We can use implicit underscore notation because there is only one valid value for length in the below
readVect : IO (VectUnknown String)
readVect = do x <- getLine
              if (x == "") then pure (MkVect _ [])
              else do MkVect _ xs <- readVect
                      pure (MkVect _ (x :: xs))

readVectPair : IO (len ** Vect len String)
readVectPair = do x <- getLine
                  if (x == "") then pure (_ ** [])
                  else do (_ ** xs) <- readVectPair
                          pure (_ ** (x :: xs))

printVect : Show a => VectUnknown a -> IO ()
printVect (MkVect len xs) = putStrLn (show xs ++ " (length " ++ show len ++ ")")

-- We cannot use the below format to define VectUnknown because we need to state the length of the Vector
-- data MyVectUnknown = MyMkVect Nat Vect

zipInputs : IO ()
zipInputs = do putStrLn "Enter first vector (blank line to end):"
               (len1 ** vec1) <- readVectPair
               putStrLn "Enter second vector (blank line to end):"
               (len2 ** vec2) <- readVectPair
               case exactLength len1 vec2 of
                    Nothing      => putStrLn "Vectors are of different lengths"
                    (Just vec2') => printLn (zip vec1 vec2')
