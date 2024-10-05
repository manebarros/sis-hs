module Main where

import Control.Monad.Trans.State (State)
import Control.Monad.Trans.State.Lazy (runState)
import Data.Char (digitToInt)
import Data.List (intercalate)
import State (Name, SysState, initial, retrieve, store)
import Util (prompt)

data Command
  = Store {name :: Name, content :: String}
  | Read {name :: Name}
  | List

actions :: [(String, IO Command)]
actions =
  [ ("Store new content", getStoreCommand),
    ("Retrieve content", getRetrieveCommand)
  ]

pickAction :: Int -> Maybe (IO Command)
pickAction x = snd <$> lookup x ([1 ..] `zip` actions)

getStoreCommand :: IO Command
getStoreCommand = do
  n <- prompt "Name: "
  cont <- prompt "Content: "
  return $ Store n cont

getRetrieveCommand :: IO Command
getRetrieveCommand = Read <$> prompt "Name: "

transition :: Command -> State SysState String
transition (Store n cont) = storeMsg n <$> store n cont
transition (Read n) = maybe "Not found." ("Content:\n" ++) <$> retrieve n
transition _ = return "Not defined yet"

storeMsg :: Name -> Bool -> String
storeMsg n True = "Linked \"" ++ n ++ "\" to an existing entry."
storeMsg n False = "Created a new entry for \"" ++ n ++ "\""

showCommands :: String
showCommands = intercalate "\n" $ zipWith labelAction [1 ..] (map fst actions)
  where
    labelAction :: Int -> String -> String
    labelAction num desc = concat ["(", show num, ") ", desc]

run :: SysState -> IO ()
run st = do
  putStrLn $ "Execute a command:\n" ++ showCommands
  choice <- digitToInt . head <$> getLine
  case pickAction choice of
    Nothing -> putStrLn "Not a valid action.\n" >> run st
    Just getCommand -> do
      trans <- transition <$> getCommand
      let (message, newSt) = runState trans st
      putStrLn $ "\n" ++ message ++ "\n"
      run newSt

main :: IO ()
main = run initial
