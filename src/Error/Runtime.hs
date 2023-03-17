module Error.Runtime (module Error.Runtime) where
import AST (Position)

data RuntimeError
  = IndexOutOfBounds Position
  | DivideByZero Position
  | NullDereference Position
  | IntegerOverflow Position
  | IntegerUnderflow Position
  deriving (Show, Eq)


runtimeMessage :: RuntimeError -> String
runtimeMessage runErr = case runErr of
  IndexOutOfBounds _ -> "Index out of bounds\n"
  DivideByZero _ -> "Dividing by zero\n"
  NullDereference _ -> "Can't dereference a null pointer\n"
  IntegerOverflow _ -> "Integer overflow\n"
  IntegerUnderflow _ -> "Integer underflow\n"

runtimePosition :: RuntimeError -> Position
runtimePosition (IndexOutOfBounds pos) = pos
runtimePosition (DivideByZero pos) = pos
runtimePosition (NullDereference pos) = pos
runtimePosition (IntegerOverflow pos) = pos
runtimePosition (IntegerUnderflow pos) = pos
