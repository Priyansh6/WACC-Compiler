{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Utils (module Interpreter.Utils) where

import qualified AST
import Control.Monad.Except (ExceptT, liftIO, runExceptT, throwError)
import Control.Monad.State (MonadIO, StateT, execStateT, gets, modify)
import Data.Functor ((<&>))
import qualified Data.Map as M
import qualified Data.Text as T
import Numeric (showHex)
import Semantic.Errors (RuntimeError (..), SemanticError (..))

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

data Scope = Scope IsMain Int deriving (Eq)

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

type Variables = M.Map AST.Ident (Scope, Value)

type Parameters = M.Map AST.Ident Value

type Functions = M.Map AST.Ident AST.Func

type ReturnValue = Maybe Value

type Heap = M.Map Address HeapValue

type Address = Int

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

showValue :: Value -> Interpreter T.Text
showValue arr'@(IArr addr) = do
  hArr <- lookupHeap arr'
  case hArr of
    (HPair _ _) -> error "pair not valid array" -- never reaches
    (HArr chars@(IChar _ : _)) -> do
      strArr <- mapM showValue chars
      return $ T.concat strArr
    (HArr _) -> return (T.pack $ "0x" <> showHex addr "")
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

freeHeapValue :: Address -> AST.Position -> Interpreter ()
freeHeapValue addr pos = do
  isInHeap <- gets (M.member addr . heap)
  if isInHeap
    then modify (\aux@Aux {heap = h, freeAddresses = addrs} -> aux {heap = M.delete addr h, freeAddresses = addr : addrs})
    else throwError $ Runtime NullDereference pos

updateValueInHeap :: AST.Position -> Address -> HeapValue -> Interpreter ()
updateValueInHeap pos addr newHeapVal = do
  isInHeap <- gets (M.member addr . heap)
  if isInHeap
    then modify (\aux@Aux {heap = h} -> aux {heap = M.insert addr newHeapVal h})
    else throwError $ Runtime NullDereference pos

lookupHeap' :: Address -> Interpreter HeapValue
lookupHeap' addr =
  gets (M.lookup addr . heap)
    >>= ( \case
            Just val -> return val
            Nothing -> throwError $ Runtime NullDereference (1, 1)
        )

lookupHeap :: Value -> Interpreter HeapValue
lookupHeap (IPair addr) =
  lookupHeap' addr
    >>= ( \case
            val@(HPair _ _) -> return val
            _ -> error "invalid address dereference for pairs"
        )
lookupHeap (IArr addr) =
  lookupHeap' addr
    >>= ( \case
            val@(HArr _) -> return val
            _ -> error "invalid address dereference for arrays"
        )
lookupHeap _ = throwError $ Runtime NullDereference (1, 1)

lookupHeapArray :: Address -> Interpreter [Value]
lookupHeapArray addr = do
  arr <- lookupHeap (IArr addr)
  case arr of
    (HPair _ _) -> error "invalid array" -- never reaches
    (HArr values) -> return values

-- lookupHeapPair :: Address -> Interpreter ??????
