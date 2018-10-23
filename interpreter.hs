import Data.Char
import Data.Map

data AExpr = Var String | AVal Integer | Plus AExpr AExpr | AEncaps AExpr deriving Show
data BExpr = BTrue | BFalse | And BExpr BExpr | Greater AExpr AExpr | BEncaps BExpr deriving Show
data Stmt = Void | While BExpr Stmt | Equal String AExpr | If BExpr Stmt Stmt | Colon Stmt Stmt deriving Show
data Interpreter a = Interpreter {eval :: (Map String Integer) -> [((Map String Integer), a)]} 

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
interpretAExpr (Plus l r) = (interpretAExpr l) >>= (\x -> interpretAExpr r >>= (\y -> return (x + y)))


interpretBExpr :: BExpr -> Interpreter Bool
interpretBExpr (BTrue) = return (True)
interpretBExpr (BFalse) = return (False)
interpretBExpr (And l r) = (interpretBExpr l) >>= (\s -> if s == False then return False else interpretBExpr r >>= (\s2 -> return (s && s2)))
interpretBExpr (Greater l r) = interpretAExpr l >>= (\x -> interpretAExpr r >>= (\y -> return (x > y)))

updateValue :: (String, Integer) -> Interpreter Integer
updateValue (k,v) = Interpreter (\s -> let s1 = insert k v s in [(s1, 0)])

getValue :: String -> Interpreter Integer
getValue k = Interpreter (\s -> if member k s then [(s, s ! k)] else [])

interpretStmt :: Stmt -> Interpreter Integer
interpretStmt Void = return 0
interpretStmt (Equal var asg) = interpretAExpr asg >>= (\s -> updateValue (var, s))
interpretStmt x@(While bexpr stm) = interpretBExpr bexpr >>= (\b -> if b then interpretStmt stm >>= (\s -> interpretStmt x) else return 0)
interpretStmt (If bexpr t f) = interpretBExpr bexpr >>= (\b -> if b then interpretStmt t else interpretStmt f)
interpretStmt (Colon l r) = interpretStmt l >>= (\x -> interpretStmt r)

