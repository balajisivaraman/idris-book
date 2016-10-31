module Main

import Data.Vect as V

data DataStore : Type where
     MkData : (size : Nat) -> (items : Vect size String) -> DataStore

data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit

size : DataStore -> Nat
size (MkData size items) = size

items : (store : DataStore) -> Vect (size store) String
items (MkData size items) = items

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
           where
            addToData : Vect old String -> Vect (S old) String
            addToData [] = [newItem]
            addToData (x :: xs) = x :: (addToData xs)


total parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" value  = Just $ Add value
parseCommand "get" id     = case all isDigit (unpack id) of
                                 False => Nothing
                                 True  => Just $ Get (cast id)
parseCommand "size" _     = Just Size
parseCommand "quit" _     = Just Quit
parseCommand "search" str = Just $ Search str
parseCommand _ _          = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd args

getItem : (id : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getItem id store = let storeItems = items store in
                       case integerToFin id (size store) of
                            Nothing    => Just $ ("Out of range\n", store)
                            (Just pos) => Just $ ((V.index pos storeItems), store)

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

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
