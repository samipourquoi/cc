import Compiler

main :: IO ()
main = do
    putStrLn "hello world"
    return $ compile "hello"
    
