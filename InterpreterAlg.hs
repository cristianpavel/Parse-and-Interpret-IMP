{-# LANGUAGE DeriveFunctor #-}
import Data.Map
import ParserFix
import ParserImp

type State = (Map String Integer)
data ReturnType = B Bool | I Integer | Null
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


type Algebra f m a = (f (m a)) -> m a

cata :: (Functor f) => (f a -> a) -> (Fix f) -> a
cata alg = alg.(fmap (cata alg)).unFix


updateValue :: (String, Integer) -> Interpreter ReturnType 
updateValue (k,v) = Interpreter (\s -> let s1 = insert k v s in [(s1, (I 0))]) 
 
getValue :: String -> Interpreter ReturnType
getValue k = Interpreter (\s -> if member k s then [(s, (I (s ! k)))] else []) 


evall :: Algebra StmtF Interpreter ReturnType
evall VoidF = return Null
evall (BVal x) = return (B x)
evall (AndF x y) = do (B l) <- x
		      (B r) <- y
		      return (B (l && r))
evall (GreaterF x y) = do (I l) <- x
			  (I r) <- y
			  return (B (l > r))
evall (VarF s) = getValue s
evall (AValF x) = return (I x)
evall (PlusF x y) = do (I l) <- x
		       (I r) <- y	
		       return (I (l + r))	

evall x@(WhileF cond block) = do (B c) <- cond
			     	 if c then
			  		 do block
				    	    evall x
			      	 else
				 	return Null
evall (IfF cond t f) = do (B c) <- cond
			  if c then
				do t
			  else
				do f

evall (EqualF s assg) = do (I x) <- assg
			   updateValue (s, x)
		    

evall (ColonF x y) = do x
		        y

evaluateProgram :: String -> State
evaluateProgram x = fst $ head (eval (cata evall (snd $ head (apply parseProgF x))) empty)
