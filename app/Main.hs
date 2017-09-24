module Main where
import           Repl.String
import           Set

main :: IO ()
main = runRepl defaultRepl { replEval = exec }
