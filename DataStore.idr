module Main

import Data.Vect as V

infixr 5 .+.

data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

data Command : Schema -> Type where
     SetSchema : (newSchema : Schema) -> Command schema
     Add : SchemaType schema -> Command schema
     Get : Maybe Integer -> Command schema
     Quit : Command schema

record DataStore where
       constructor MkData
       schema : Schema
       size : Nat
       items : Vect size (SchemaType schema)

-- Writing definitons like below to extract fields for types will become tedious.
-- Instead we can use Idris' record syntax (above), which gives accessor functions as default.
{-
data DataStore : Type where
     MkData : (schema : Schema) ->
              (size : Nat) ->
              (items : Vect size (SchemaType schema)) ->
              DataStore

size : DataStore -> Nat
size (MkData schema size items) = size

schema : DataStore -> Schema
schema (MkData schema size items) = schema

items : (store : DataStore) -> Vect (size store) (SchemaType (schema store))
items (MkData schema size items) = items
-}

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) = case xs of
                                   [] => Just SString
                                   _  => do xs_sch <- parseSchema xs
                                            Just (SString .+. xs_sch)
parseSchema ("Int" :: xs) = case xs of
                                [] => Just SInt
                                _ => do xs_sch <- parseSchema xs
                                        Just (SInt .+. xs_sch)
parseSchema ("Char" :: xs) = case xs of
                                 [] => Just SChar
                                 _  => do xs_sch <- parseSchema xs
                                          Just (SChar .+. xs_sch)
parseSchema _ = Nothing

addToStore : (store : DataStore) -> (SchemaType (schema store)) -> DataStore
addToStore (MkData schema size store) newItem = MkData schema _ (addToData store)
           where
            addToData : Vect oldSize (SchemaType schema) -> Vect (S oldSize) (SchemaType schema)
            addToData [] = [newItem]
            addToData (item :: items) = item :: (addToData items)

display : SchemaType schema -> String
display {schema = SString} item = item
display {schema = SInt} item = cast item
display {schema = SChar} item = cast item
display {schema = (x .+. y)} (iteml, itemr) = "(" ++ (display $ iteml) ++ "," ++ (display $ itemr) ++ ")"

getItem : (id : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getItem id store = let storeItems = items store in
                       case integerToFin id (size store) of
                            Nothing    => Just $ ("Out of range\n", store)
                            (Just pos) => Just $ ((display (V.index pos storeItems)) ++ "\n", store)

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input   = getQuoted (unpack input)
            where
            getQuoted : List Char -> Maybe (String, String)
            getQuoted ('"' :: xs)
                      = case span (/= '"') xs of
                             (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                             _                     => Nothing
            getQuoted _ = Nothing
parsePrefix SChar input     = case unpack input of
                                   (c :: ' ' :: rest) => Just (c, ltrim (pack rest))
                                   _ => Nothing
parsePrefix SInt input      = case span isDigit input of
                                   ("", rest)  => Nothing
                                   (num, rest) => Just (cast num, ltrim rest)
parsePrefix (schemal .+. schemar) input = case parsePrefix schemal input of
                                               Nothing => Nothing
                                               Just (l_val, input') =>
                                                    case parsePrefix schemar input' of
                                                         Nothing => Nothing
                                                         Just (r_val, input'') => Just ((l_val, r_val), input'')

parseBySchema : (schema : Schema) -> (value : String) -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Just (res, "") => Just res
                                  Just _         => Nothing
                                  Nothing        => Nothing

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z     => Just (MkData schema _ [])
                              (S k) => Nothing

total parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "add" value  = do schemaType <- parseBySchema schema value
                                      Just $ Add schemaType
parseCommand schema "get" id     = case unpack id of
                                        [] => Just $ Get Nothing
                                        l  => case all isDigit l of
                                                  False => Nothing
                                                  True  => Just $ Get (Just $ cast id)
-- parseCommand "size" _     = Just Size
parseCommand schema "quit" _     = Just Quit
-- parseCommand "search" str = Just $ Search str
{- ... rest of definition as before ... -}
parseCommand schema "schema" rest
             = do schemaok <- parseSchema (words rest)
                  Just (SetSchema schemaok)
parseCommand _ _ _          = Nothing

parse : (schema : Schema) ->
        (input : String) ->
        Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                     (cmd, args) => parseCommand schema cmd (ltrim args)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input
             = case parse (schema store) input of
                    Nothing         => Just ("Invalid command\n", store)
                    Just (Add item) =>
                      Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                    Just (SetSchema schema') =>
                      case setSchema store schema' of
                           Nothing     => Just ("Can't update schema\n", store)
                           Just store' => Just ("OK\n", store')
                    Just (Get pos)  => case pos of
                                           Just position => getItem position store
                                           Nothing       => Just $ (unlines $ toList $ map display (items store), store)
                    Just Quit       => Nothing

main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " processInput

{-

-- Below seems unnecessarily long and verbose to me, need to refactor!
searchStoreItems : (str : String) -> (storeItems : Vect size String) -> (count : Int) -> (acc : String) -> String
searchStoreItems str [] count acc = acc
searchStoreItems str (x :: xs) count acc = if (isInfixOf str x) then searchStoreItems str xs (count + 1) (acc ++ (show count) ++ ": " ++ x ++ "\n")
                                           else searchStoreItems str xs (count + 1) acc

searchStore : (str : String) -> (store : DataStore) -> String
searchStore str store = let storeItems = items store in (searchStoreItems str storeItems 0 "")

processCommand : (command : Command) -> (store : DataStore) -> Maybe (String, DataStore)
processCommand (Add str) store    = Just $ ("Added " ++ str ++ " to store at id " ++ show (size store) ++ "\n", addToStore store str)
processCommand (Get id) store     = getItem id store
processCommand Size store         = Just $ ("No. of Items in Store is " ++ show (size store) ++ "\n", store)
processCommand (Search str) store = Just $ (searchStore str store, store)
processCommand Quit store         = Nothing

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse input of
                                Nothing        => Just $ ("Invalid Command\n", store)
                                (Just command) => processCommand command store
-}
