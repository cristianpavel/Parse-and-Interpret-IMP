{-# LANGUAGE DeriveFunctor #-}

module ParserFix where
import ParserImp
import Control.Applicative

data StmtF a = VoidF | BVal Bool | AndF a a | GreaterF a a | VarF String | AValF Integer | PlusF a a | WhileF a a | EqualF String a | IfF a a a | ColonF a a deriving Functor 
data Fix f = Fix { unFix :: f (Fix f) }


parseVarF = (\x y -> Fix (VarF (x++y)))
           <$> (plus (parseAlpha))
           <*> (star (parseAlphaNum))

parseAValF = (\x y -> Fix (AValF (read (x++y):: Integer)))
            <$> maybeOnce (parseChar (== '-'))
            <*> plus (parseDigit)


parseSumF = (\x z y -> Fix (PlusF x y))
           <$> (parseEnclosed parseAExprF <|> parseVarF <|> parseAValF)
           <*> parseCharInWs (=='+')
           <*> parseAExprF

parseAExprF = parseSumF <|> parseVarF <|> parseAValF <|> parseEnclosed parseAExprF

parseEqualF = (\(Var x) y z-> Fix (EqualF x z))
             <$> parseVar
             <*> parseCharInWs (=='=')
             <*> parseAExprF


parseBVal = (\x -> Fix (BVal (read x::Bool)))
            <$> (parseString "True" <|> parseString "False")


parseGreaterF = (\x z y -> Fix (GreaterF x y))
               <$> parseAExprF
               <*> parseCharInWs (=='>')
               <*> parseAExprF

parseBoolF = parseBVal <|> parseGreaterF

parseAndF = (\x z y -> Fix (AndF x y))
           <$> (parseEnclosed parseBExprF <|> parseBoolF)
           <*> parseString "&&"
           <*> parseBExprF


parseBExprF = parseAndF <|> parseBoolF <|> parseEnclosed parseBExprF

parseStmtF =  (parseEqualF <|> parseWhileF <|> parseIfF)

parseColonF = (\x z y -> Fix (ColonF x y))
            <$> parseStmtF
            <*> parseCharInWs (==';')
            <*> parseProgF

parseWhileF = (\x bexpr stmt -> Fix (WhileF bexpr stmt))
             <$> parseString "while"
             <*> parseEnclosed parseBExprF
             <*> parseBlockF

parseIfF = (\x bexpr t f -> Fix (IfF bexpr t f))
          <$> parseString "if"
          <*> parseEnclosed parseBExprF
          <*> parseBlockF
          <*> parseBlockF



parseBlockF = (\x y z -> y)
             <$> parseEnterBlock
             <*> parseProgF
             <*> parseExitBlock


parseProgF = parseColonF <|> parseStmtF <|> pure (Fix VoidF)
