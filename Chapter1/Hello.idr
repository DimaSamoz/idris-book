module Main

main : IO ()
main = putStrLn "Hello, Idris world!"

mainHole : IO ()
mainHole = putStrLn ?greeting

mainConvert : IO ()
mainConvert = putStrLn (cast 'x')
