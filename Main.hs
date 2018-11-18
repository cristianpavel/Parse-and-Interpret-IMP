import InterpreterImp
import ParserImp
import System.IO
import Data.Map
import System.Environment

main :: IO ()
main = do args <- getArgs
          case args of
             [fileName] -> do file <- readFile fileName
                              print (eval (interpretStmt (snd (head (apply parseProg file)))) empty)
             _ -> putStrLn "Wrong number of args"
          
