import Data.Char

data AExpr = Var String | AVal Integer | Plus AExpr AExpr | AEncaps AExpr deriving Show
data BExpr = BTrue | BFalse | And BExpr BExpr | Greater AExpr AExpr | BEncaps BExpr deriving Show
data Stmt = Void | While BExpr Stmt | Equal String AExpr | If BExpr Stmt Stmt | Colon Stmt Stmt deriving Show
data Parser a = Parser {apply :: String -> [(String, a)]}

whitespace = [' ', '\n', '\t']

parseChar :: (Char -> Bool) -> Parser Char
parseChar f = Parser (\s -> case s of
                                [] -> []
                                x:xs -> if f x then [(xs, x)] else [])



(.||.) :: (Parser a) -> (Parser a) -> (Parser a)
p1 .||. p2 = Parser (\s -> case apply p1 s of
                        [] -> apply p2 s
                        x -> x)

rreturn :: a -> (Parser a)
rreturn x = Parser (\s -> [(s, x)])

instance Functor Parser where
    fmap f p = (pure f) <*> p


instance Applicative Parser where
    pure = rreturn
    pf <*> p = Parser (\s -> case apply pf s of
                                [] -> []
                                [(r, f)] -> case apply p r of
                                                [] -> []
                                                [(r1, s)] -> [(r1, f s)])

plus p = (:) <$> p <*> star p
star p = (plus p) .||. (rreturn [])
maybeOnce p = ((:[]) <$>  p) .||. (rreturn [])

parseWs :: Parser String
parseWs = plus (parseChar (\x -> x `elem` whitespace))

parseAlpha = parseChar (isAlpha)
parseAlphaNum = parseChar (isAlphaNum)

parseDigit = parseChar (isDigit)

parseVar :: Parser AExpr
parseVar = (\x y -> Var (x++y)) <$> (plus (parseAlpha)) <*> (star (parseAlphaNum))

parseAVal :: Parser AExpr
parseAVal = (\x y -> (AVal (read (x++y):: Integer))) <$> maybeOnce (parseChar (== '-')) <*> plus (parseDigit)

parseCharInWs :: (Char -> Bool) -> Parser Char
parseCharInWs f = (\x y z -> y) <$> star (parseWs) <*> parseChar f <*> star parseWs

parseSum :: Parser AExpr
parseSum = (\x y -> Plus x (Prelude.foldr Plus (AVal 0) y))<$> (parseVar .||. parseAVal) <*> plus ((\x y -> y) <$> parseCharInWs (=='+') <*> parseVar .||. parseAVal)

parseEqual :: Parser Stmt
parseEqual = (\(Var x) y z-> Equal x z) <$> parseVar <*> parseCharInWs (=='=') <*> (parseSum .||. parseVar .||. parseAVal)



