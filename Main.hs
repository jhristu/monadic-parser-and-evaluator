import TypesAndTerms
import MonadicParser
import MonadicEvaluator

main :: IO ()
main = do program <- getContents
          let tm = parse program
          let val = eval tm
          putStrLn (show val)