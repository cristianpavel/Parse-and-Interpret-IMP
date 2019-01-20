module ParserImp where

import Data.Char
import Control.Applicative

data AExpr = Var String | AVal Integer | Plus AExpr AExpr | AEncaps AExpr deriving Show
data BExpr = BTrue | BFalse | And BExpr BExpr | Greater AExpr AExpr | BEncaps BExpr deriving Show
data Stmt = Void | While BExpr Stmt | Equal String AExpr | If BExpr Stmt Stmt | Colon Stmt Stmt deriving Show
data Parser a = Parser {apply :: String -> [(String, a)]}


whitespace = [' ', '\n', '\t']

parseChar :: (Char -> Bool) -> Parser Char
parseChar f = Parser (\s -> case s of
                                [] -> []
                                x:xs -> if f x then [(xs, x)] else [])



rreturn :: a -> (Parser a)
rreturn x = Parser (\s -> [(s, x)])

instance Functor Parser where
    fmap f p = (pure f) <*> p


instance Alternative Parser where
        empty = Parser (\s -> [])
        p1 <|> p2 = Parser (\s -> case apply p1 s of
                                        [] -> apply p2 s
                                        x -> x)

instance Applicative Parser where
    pure = rreturn
    pf <*> p = Parser (\s -> case apply pf s of
                                [] -> []
                                [(r, f)] -> case apply p r of
                                                [] -> []
                                                [(r1, s)] -> [(r1, f s)])

plus p = (:) <$> p <*> star p
star p = (plus p) <|> rreturn []
maybeOnce p = ((:[]) <$>  p) <|> rreturn []

parseWs :: Parser String
parseWs = plus (parseChar (\x -> x `elem` whitespace))

parseAlpha = parseChar (isAlpha)
parseAlphaNum = parseChar (isAlphaNum)

parseDigit = parseChar (isDigit)

parseVar :: Parser AExpr
parseVar = (\x y -> Var (x++y)) 
           <$> (plus (parseAlpha)) 
           <*> (star (parseAlphaNum))

parseAVal :: Parser AExpr
parseAVal = (\x y -> (AVal (read (x++y):: Integer))) 
            <$> maybeOnce (parseChar (== '-')) 
            <*> plus (parseDigit)

parseCharInWs :: (Char -> Bool) -> Parser Char
parseCharInWs f = (\x y z -> y) 
                  <$> star parseWs 
                  <*> parseChar f 
                  <*> star parseWs

parseOpen = parseCharInWs (=='(')
parseClosed = parseCharInWs (==')')

parseSum = (\x z y -> Plus x y)
           <$> (parseEnclosed parseAExpr <|> parseVar <|> parseAVal)
           <*> parseCharInWs (=='+')
           <*> parseAExpr

parseEnclosed p = (\x y z -> y)
                     <$> parseOpen
                     <*> p
                     <*> parseClosed

parseAExpr = parseSum <|> parseVar <|> parseAVal <|> parseEnclosed parseAExpr


parseEqual :: Parser Stmt
parseEqual = (\(Var x) y z-> Equal x z) 
             <$> parseVar 
             <*> parseCharInWs (=='=') 
             <*> parseAExpr


parseString :: String -> Parser String
parseString [] = rreturn []
parseString x = (\ws y ws2 -> y) 
                <$> star parseWs 
                <*> parseStringHelp x
                <*> star parseWs

parseStringHelp [] = rreturn []
parseStringHelp (x:xs) = (\x y -> (x:y)) 
                         <$> parseChar (== x) 
                         <*> parseStringHelp xs

parseTrue :: Parser BExpr
parseTrue = (\x -> BTrue) 
            <$> parseString "True"

parseFalse :: Parser BExpr
parseFalse = (\x -> BFalse) 
             <$> parseString "False"

parseGreater = (\x z y -> Greater x y)
               <$> parseAExpr
               <*> parseCharInWs (=='>')
               <*> parseAExpr

parseBool = parseTrue <|> parseFalse <|> parseGreater

parseAnd = (\x z y -> And x y)
           <$> (parseEnclosed parseBExpr <|> parseBool)
           <*> parseString "&&"
           <*> parseBExpr


parseBExpr = parseAnd <|> parseBool <|> parseEnclosed parseBExpr

parseStmt =  (parseEqual <|> parseWhile <|> parseIf)
parseColon = (\x z y -> Colon x y)
            <$> parseStmt
            <*> parseCharInWs (==';')
            <*> parseProg

parseWhile = (\x bexpr stmt -> While bexpr stmt)
             <$> parseString "while"
             <*> parseEnclosed parseBExpr
             <*> parseBlock

parseIf = (\x bexpr t f -> If bexpr t f)
          <$> parseString "if"
          <*> parseEnclosed parseBExpr
          <*> parseBlock
          <*> parseBlock


parseEnterBlock = parseCharInWs (=='{')
parseExitBlock = parseCharInWs (=='}')

parseBlock = (\x y z -> y)
             <$> parseEnterBlock
             <*> parseProg
             <*> parseExitBlock
             

parseProg = parseColon <|> parseStmt <|> pure Void




