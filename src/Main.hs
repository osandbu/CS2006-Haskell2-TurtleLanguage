-- | The 'Main' module contains the main method. Reads arguments from the command line, reads a script from the filename given as the first argument and executes the script with the remaining arguments as parameters passed into the script's main method.
module Main where

import IO (readFile, isDoesNotExistError)
import Lang
import LangParser (parseScript, expr, test)
import System (getArgs)
import Turtle (runTurtle)

-- | 'main' method which reads arguments from the command line, attempts to read and parse the first argument as a filename of a script and passes the remainding arguments as parameters to the script's 'main' method.
main :: IO ()
main = do args <- getArgs
          case args of
              []         -> putStrLn "Argument required: Script filename."
              fname : [] -> runScript fname []
              fname : xs -> runScript fname (parseArgs xs)

-- | 'parseArgs' converts a list of 'String's to 'Expr's.
parseArgs :: [String] -> [Expr]
parseArgs args = [test expr arg | arg <- args]

-- | 'runScript' runs a script with a given filename and using some given expressions as arguments.
runScript :: String -> [Expr] -> IO ()
runScript fname args = do content <- readFile' fname
                          case parseScript content of
                              Right defs -> do let runMain = Call "main" args
                                               case interp defs [] runMain of
                                                   Right cmd -> runTurtle cmd
                                                   Left msg  -> putStrLn msg
                              Left msg -> putStrLn msg

-- | 'readFile'' reads a file or reports throws an error if the file does not exist or some other IO error occurs.
readFile' :: String -> IO String
readFile' fname = catch (readFile fname) errHandler
    where
        errHandler e | isDoesNotExistError e = error "File does not exist."
                     | otherwise             = error "Unexpected IO error."
