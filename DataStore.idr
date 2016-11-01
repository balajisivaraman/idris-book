module Main

import Data.Vect as V

infixr 5 .+.

-- Schema is either a String or an Int or some combination of both
-- as denoted by th .+. type
data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString   = String
SchemaType SInt      = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

-- Records and the below commented out code are almost equivalent
-- With records, we don't have to define accessor functions by hand
record DataStore where
       constructor MkData
       schema : Schema
       size : Nat
       items : Vect size (SchemaType schema)

-- data DataStore : Type where
--      MkData : (schema : Schema) ->
--               (size : Nat) ->
--               (items : Vect size (SchemaType schema)) ->
--               DataStore

data Command : Schema -> Type where
      Add : SchemaType schema -> Command schema
      Get : Integer -> Command schema
      Size : Command schema
      Quit : Command schema
   -- Search String

-- size : DataStore -> Nat
-- size (MkData _ size _) = size

-- schema : DataStore -> Schema
-- schema (MkData schema _ _) = schema

-- items : (store : DataStore) -> Vect (size store) (SchemaType $ schema store)
-- items (MkData _ _ items) = items

addToStore : (store : DataStore) -> (SchemaType $ schema store) -> DataStore
addToStore (MkData schema size items) newItem = MkData schema _ (addToData items)
           where
            addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
            addToData [] = [newItem]
            addToData (x :: xs) = x :: (addToData xs)

getItem : (id : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getItem id store = let storeItems = items store in
                       case integerToFin id (size store) of
                            Nothing    => Just $ ("Out of range\n", store)
                            (Just pos) => Just $ (?todo ((V.index pos storeItems)) ++ "\n", store)

total parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "add" value  = Just (Add (?parseBySchema value))
parseCommand schema "get" id     = case all isDigit (unpack id) of
                                        False => Nothing
                                        True  => Just $ Get (cast id)
parseCommand schema "size" _     = Just Size
parseCommand schema "quit" _     = Just Quit
-- parseCommand schema "search" str = Just $ Search str
parseCommand _ _ _          = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                   (cmd, args) => parseCommand schema cmd (ltrim args)

-- processCommand : (command : (Command schema)) -> (store : DataStore) -> Maybe (String, DataStore)
-- processCommand (Add item) store    = Just $ ("Added " ++ (?todo item) ++ " to store at id " ++ show (size store) ++ "\n", addToStore store (?todo item))
-- processCommand (Get id) store     = getItem id store
-- processCommand Size store         = Just $ ("No. of Items in Store is " ++ show (size store) ++ "\n", store)
-- processCommand (Search str) store = Just $ (searchStore str store, store)
-- processCommand Quit store         = Nothing

-- processInput : DataStore -> String -> Maybe (String, DataStore)
-- processInput store input = case parse (schema store) input of
--                                 Nothing        => Just $ ("Invalid Command\n", store)
--                                 (Just command) => processCommand command store

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input
             = case parse (schema store) input of
                    Nothing => Just ("Invalid command\n", store)
                    Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                    Just (Get pos) => getItem pos store
                    Just Size => Just $ ("No. of Items in Store is " ++ show (size store) ++ "\n", store)
                    Just Quit => Nothing

main : IO ()
main = replWith (MkData SString _ [])
                "Command: " processInput

{-
-- Below seems unnecessarily long and verbose to me, need to refactor!
searchStoreItems : (str : String) -> (storeItems : Vect size String) -> (count : Int) -> (acc : String) -> String
searchStoreItems str [] count acc = acc
searchStoreItems str (x :: xs) count acc = if (isInfixOf str x) then searchStoreItems str xs (count + 1) (acc ++ (show count) ++ ": " ++ x ++ "\n")
                                           else searchStoreItems str xs (count + 1) acc

searchStore : (str : String) -> (store : DataStore) -> String
searchStore str store = let storeItems = items store in (searchStoreItems str storeItems 0 "")

-}
