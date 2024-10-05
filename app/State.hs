module State where

import Control.Monad.Trans.State
import Data.Char (ord)
import qualified Data.Map as M

type Name = String

type Content = String

type Hash = Int

type Translation = M.Map Name Hash

type Storage = M.Map Hash Content

data SysState = St
  { translation :: Translation,
    storage :: Storage
  }

initial :: SysState
initial = St M.empty M.empty

store :: Name -> Content -> State SysState Bool
store name cont = do
  St trans st <- get
  let (hadHash, newSt) = runState (updateStorage h cont) st
  put $ St (M.insert name h trans) newSt
  return hadHash
  where
    h = hash cont

retrieve :: Name -> State SysState (Maybe Content)
retrieve name = do
  St trans st <- get
  return $ M.lookup name trans >>= (`M.lookup` st)

updateStorage :: Hash -> Content -> State Storage Bool
updateStorage h cont = do
  st <- get
  if h `M.member` st
    then return True
    else put (M.insert h cont st) >> return False

hash :: Content -> Hash
hash = sum . map ord
