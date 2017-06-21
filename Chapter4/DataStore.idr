module Main

import Data.Vect

data DataStore : Type where
    MkData : (size : Nat) ->
             (items : Vect size String) ->
             DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items)
    where
        addToData : Vect old String -> Vect (S old) String
        addToData [] = [newitem]
        addToData (x :: xs) = x :: addToData xs

data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "search" substr = Just (Search substr)
parseCommand "size" "" = Just Size
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing


parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) ->
           Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                            case integerToFin pos (size store) of
                                  Nothing => Just ("Out of range\n", store)
                                  Just id => Just (index id store_items ++ "\n", store)


getBySubstr : (substr : String) -> (store : DataStore) -> Maybe (String, DataStore)
getBySubstr substr store@(MkData s is) = Just (printEntries is 0, store)
    where
        printEntries : Vect n String -> Integer -> String
        printEntries [] ind = ""
        printEntries (y :: ys) ind =
            case isInfixOf substr y of
                False => printEntries ys (ind + 1)
                True => show ind ++ " " ++ y ++ "\n" ++ printEntries ys (ind + 1)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp =
    case parse inp of
        Nothing => Just ("Invalid command\n", store)
        Just (Add item) =>
        Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
        Just (Get pos) => getEntry pos store
        Just (Search substr) => getBySubstr substr store
        Just Size => Just ("Store has " ++ show (size store) ++ " entries.\n", store)
        Just Quit => Nothing


main : IO ()
main = replWith (MkData _ []) "Command: " processInput
