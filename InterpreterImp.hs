module InterpreterImp where
import Data.Char
import Data.Map
import ParserImp

type State = Map String Integer
data Interpreter a = Interpreter {eval :: State -> [(State, a)]} 

instance Functor (Interpreter) where
    fmap f p = pure f <*> p

instance Applicative (Interpreter) where
    pure = return
    intf <*> p = p >>= (\x -> intf >>= (\f -> return (f x)))

instance Monad (Interpreter) where
    return x = Interpreter (\s -> [(s, x)])
    i >>= f = Interpreter (\s -> case eval i s of
                                    [] -> []
                                    [(r, y)] -> eval (f y) r)

interpretAExpr :: AExpr -> Interpreter Integer
interpretAExpr (Var x) = getValue x
interpretAExpr (AVal  x) = return (x)
interpretAExpr (Plus l r) = do left <- interpretAExpr l
                               right <- interpretAExpr r
                               return (left + right)


interpretBExpr :: BExpr -> Interpreter Bool
interpretBExpr (BTrue) = return (True)
interpretBExpr (BFalse) = return (False)
interpretBExpr (And l r) = do left <- interpretBExpr l
                              if (left == False) then
                                     return False
                              else
                                    do right <- interpretBExpr r
                                       return (left && right)
interpretBExpr (Greater l r) = do left <- interpretAExpr l
                                  right <- interpretAExpr r
                                  return (left > right) 

updateValue :: (String, Integer) -> Interpreter Integer
updateValue (k,v) = Interpreter (\s -> let s1 = insert k v s in [(s1, 0)])

getValue :: String -> Interpreter Integer
getValue k = Interpreter (\s -> if member k s then [(s, s ! k)] else [])

interpretStmt :: Stmt -> Interpreter Integer
interpretStmt Void = return 0
interpretStmt (Equal var right) = do rvalue <- interpretAExpr right
                                     updateValue (var, rvalue)
interpretStmt x@(While bexpr stm) = do condition <- interpretBExpr bexpr
                                       if condition then
                                          do interpretStmt stm
                                             interpretStmt x
                                       else
                                           return 0

interpretStmt (If bexpr t f) = do condition <- interpretBExpr bexpr
                                  if condition then
                                     do interpretStmt t
                                  else
                                     do interpretStmt f

interpretStmt (Colon l r) = do interpretStmt l
                               interpretStmt r


