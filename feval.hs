import System.IO
import Control.Applicative
import Control.Monad

import EF

showResult :: Either ParseError Result -> String
showResult (Right (Result (e, t))) = "  => " ++ show e ++ "\n    : " ++ show t
showResult (Right TypeMismatch) = "Error: Type Mismatch"
showResult (Right InconsistentTypes) = "Error: Inconsistent Types"
showResult (Left e) = "Error: " ++ show e

getLines :: IO ()
getLines = do
    l <- getLine
    if l == "\EOT" then return () else do
        putStrLn . showResult $ parseRun l
        getLines
        return ()

main = getLines

