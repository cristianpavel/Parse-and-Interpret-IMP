

import Data.Map

type State = (Map String Integer)
data Stmt = Void | While Stmt Stmt | Equal String Stmt | If Stmt Stmt Stmt | Colon Stmt Stmt 
                 | Plus Stmt Stmt | IfR Stmt Stmt | GreaterR Stmt | GreaterL Stmt | Var String
                 | EqualR String | ColonR Stmt | PlusL Stmt | PlusR Stmt | AndR Stmt | AVal Integer | BVal Bool | And Stmt Stmt | Greater Stmt Stmt deriving Show
type Cfg = ([Stmt], State)



step :: Cfg -> Cfg
step ([], s) = ([], s)
step (Void:t, s) = (t, s)
step ((Var x):t, s) = ((AVal (s ! x)):t, s)
step (((BVal True) `And` y):t, s) = (y:t, s)
step (((BVal False) `And` _):t, s) = ((BVal False):t, s)
step ((x `And` y):t, s) = (x:(AndR y):t, s)
step ((BVal b):(AndR y):t, s) = (((BVal b) `And` (AndR y)):t, s)
step (((AVal x) `Plus` (AVal y)):t, s) = ((AVal (x + y)):t, s)
step ((x `Plus` y):t, s) = (x:(PlusR y):t, s) 
step (x:(PlusR y):t, s) = (y:(PlusL x):t, s)
step ((AVal x):(PlusL y):t, s) = ((y `Plus` (AVal x)):t, s)
step (((AVal x) `Greater` (AVal y)):t, s) = ((BVal (x > y)):t, s) 
step ((x `Greater` y):t, s) = ((x:(GreaterR y):t), s)
step (x:(GreaterR y):t, s) = ((y:(GreaterL x):t), s)
step (x:(GreaterL y):t, s) = ((y `Greater` x):t, s)
step ((x@(While c b)):t, s) = ((If c (Colon b x) Void):t, s)
step ((If (BVal True) tr _):t, s) = (tr:t, s)
step ((If (BVal False) _ fls):t, s) = (fls:t, s)
step ((If c tr fls):t, s) = (c:(IfR tr fls):t, s)
step (c:(IfR tr fls):t,s) = ((If c tr fls):t, s) 
step ((Equal x (AVal y)):t, s) = (t, (insert x y s))
step ((Equal x assg):t, s) = (assg:(EqualR x):t,s)
step (x:(EqualR var):t, s) = ((Equal var x):t, s)
step ((Colon x y):t, s) = (x:(ColonR y):t, s)
step ((ColonR y):t, s) = (y:t, s)

stepStar :: Cfg -> State
stepStar ([], state) = state
stepStar x  = stepStar (step x)
