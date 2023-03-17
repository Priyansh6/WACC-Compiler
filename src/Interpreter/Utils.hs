{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Utils (module Interpreter.Utils) where

import qualified AST
import Control.Monad.Except (ExceptT, liftIO, runExceptT, throwError)
import Control.Monad.State (MonadIO, StateT, execStateT, gets, modify)
import Data.Functor ((<&>))
import qualified Data.Map as M
import qualified Data.Text as T
import Numeric (showHex)
import Semantic.Errors (RuntimeError (..), SemanticError (..), pairErrorType, arrayErrorType)

type Interpreter = StateT Aux (ExceptT SemanticError IO)

runInterpreter :: MonadIO m => Interpreter a -> Aux -> m (Either SemanticError Aux)
runInterpreter executor initialState = liftIO $ runExceptT $ execStateT executor initialState

data Aux = Aux
  { vars :: Variables,
    params :: Parameters,
    funcs :: Functions,
    returnValue :: ReturnValue,
    heap :: Heap,
    freeAddresses :: [Address]
  }

-- data ReplError = Runtime RuntimeError | Semantic SemanticError
type IsMain = Bool

data Scope = Scope IsMain Int deriving (Eq, Show)

instance Ord Scope where
  compare (Scope False _) (Scope True _) = GT
  compare (Scope True _) (Scope False _) = LT
  compare (Scope _ i) (Scope _ i') = compare i i'

instance Num Scope where
  Scope main n + Scope main' n' = Scope (main && main') (n + n')
  Scope main n - Scope main' n' = Scope (main && main') (n - n')
  Scope main n * Scope main' n' = Scope (main && main') (n * n')
  abs (Scope main n) = Scope main (abs n)
  signum (Scope main n) = Scope main (signum n)
  fromInteger n = Scope True (fromInteger n)

type Variables = M.Map AST.Ident (M.Map Scope Value)

type Parameters = M.Map AST.Ident Value

type Functions = M.Map (AST.Ident, ReturnType, ParamsType) AST.Func

type Heap = M.Map Address HeapValue

type ReturnValue = Maybe Value

type Address = Int

type ReturnType = AST.WType

type ParamsType = [AST.WType]

address :: Value -> Address
address (IArr addr) = addr
address (IPair addr) = addr
address _ = error "no address found"

data Value
  = IInt Integer
  | IBool Bool
  | IChar Char
  | IStr T.Text
  | IArr Address
  | IPair Address
  | IUnit -- for pair type erasure
  deriving (Eq, Ord)

instance Show Value where
  show (IInt i) = show i
  show (IBool b) = if b then "true" else "false"
  show (IChar c) = c : ""
  show (IStr t) = T.unpack t
  show (IArr addr) = "0x" <> showHex addr ""
  show (IPair addr) = "0x" <> showHex addr ""
  show IUnit = "(nil)"

data HeapValue
  = HArr [Value]
  | HPair Value Value
  deriving (Show)

showValue :: Value -> Interpreter T.Text
showValue (IArr addr) = do
  hArr <- lookupHeapArray addr (1,1)
  case hArr of
    chars@(IChar _ : _) -> mapM showValue chars <&> T.concat
    _ -> return (T.pack $ "0x" <> showHex addr "")
showValue (IPair addr) = return (T.pack $ "0x" <> showHex addr "")
showValue v = return (T.pack $ show v)

defaultAux :: Aux
defaultAux =
  Aux
    { vars = M.empty,
      params = M.empty,
      funcs = M.empty,
      returnValue = Nothing,
      heap = M.empty,
      freeAddresses = [0 ..]
    }

addHeapValue' :: HeapValue -> Interpreter Address
addHeapValue' v = do
  (addr : newFreeAddrs) <- gets freeAddresses
  modify (\aux@Aux {heap = h} -> aux {heap = M.insert addr v h, freeAddresses = newFreeAddrs})
  return addr

addHeapValue :: HeapValue -> Interpreter Value
addHeapValue v@(HArr _) = addHeapValue' v <&> IArr
addHeapValue v@(HPair _ _) = addHeapValue' v <&> IPair

modifyHeapPair :: Address -> HeapValue -> Interpreter ()
modifyHeapPair addr hval = modify (\aux@Aux {heap = h} -> aux {heap = M.insert addr hval h})

freeHeapValue :: Address -> AST.Position -> Interpreter ()
freeHeapValue addr pos = do
  isInHeap <- gets (M.member addr . heap)
  if isInHeap
    then modify (\aux@Aux {heap = h, freeAddresses = addrs} -> aux {heap = M.delete addr h, freeAddresses = addr : addrs})
    else throwError $ Runtime NullDereference pos

updateValueInHeap :: Address -> HeapValue -> AST.Position -> Interpreter ()
updateValueInHeap addr newHeapVal pos = do
  isInHeap <- gets (M.member addr . heap)
  if isInHeap
    then modify (\aux@Aux {heap = h} -> aux {heap = M.insert addr newHeapVal h})
    else throwError $ Runtime NullDereference pos

lookupHeap :: Address -> AST.Position -> Interpreter HeapValue
lookupHeap addr pos = do
  mVal <- gets (M.lookup addr . heap)
  case mVal of
    Just val -> return val
    Nothing -> throwError $ Runtime NullDereference pos

lookupHeapArray :: Address -> AST.Position ->  Interpreter [Value]
lookupHeapArray addr pos = do
  arr <- lookupHeap addr pos
  case arr of
    (HArr values) -> return values
    (HPair _ _) -> throwError $ IncompatibleTypes pos [arrayErrorType] pairErrorType

lookupHeapPair :: Address -> AST.Position ->  Interpreter (Value, Value)
lookupHeapPair addr pos = do
  arr <- lookupHeap addr pos
  case arr of
    (HPair l r) -> return (l, r)
    (HArr _) -> throwError $ IncompatibleTypes pos [pairErrorType] arrayErrorType
