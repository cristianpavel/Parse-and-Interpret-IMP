import InterpreterImp
import ParserImp
import System.IO
import Data.Map

main :: IO ()
main = do fileName <- getLine
          file <- readFile fileName
          print (eval (interpretStmt (snd (head (apply parseProg file)))) empty)

          
