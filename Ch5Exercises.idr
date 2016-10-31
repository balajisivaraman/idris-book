module Main

import Data.Vect as V

readToBlank : IO (List String)
readToBlank = do
            inp <- getLine
            if inp == "" then pure []
            else do nextLines <- readToBlank
                    pure $ inp :: nextLines

readAndSave : IO ()
readAndSave = do
            putStrLn "Please enter the lines to be read:"
            lines <- readToBlank
            putStrLn "Please enter the file name:"
            fileName <- getLine
            Right done <- writeFile fileName (unlines lines)
          | Left err   => putStrLn ("Unable to open file " ++ fileName ++ " for writing")
            pure done

total listToVect : List String -> (len ** Vect len String)
listToVect [] = (_ ** [])
listToVect (x :: xs) = let (_ ** vec) = listToVect xs in
                           (_ ** x :: vec)

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
            Right contents <- readFile filename
           | Left err => pure (_ ** V.Nil)
            let lines = lines contents
            pure (listToVect lines)
