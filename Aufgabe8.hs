{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Parser
import Type
import Aufgabe2
import Aufgabe7

main :: IO ()
main = do
    putStrLn "Welcome!"
    putStrLn "Type \":h\" for help."
    loop (Prog []) dfs ""

loop :: Prog -> Strategy -> String -> IO ()
loop p s f = do
   putStr "?- "
   input <- getLine
   if length (words input) == 2 then cases p s f (head (words input)) (last (words input))
                        else if length (words input) == 1 then cases p s f (head (words input)) ""
                                                  else putStrLn "Sorry, Too many arguments. Try a different command" >> loop p s f

cases :: Prog -> Strategy -> String -> String -> String -> IO ()
cases p s f ":h" _ = do
                  putStrLn "Commands available from the prompt:"
                  putStrLn "   <goal>      Solves/proves the specified goal."
                  putStrLn "   :h          Shows this help message."
                  putStrLn "   :l <file>   Loads the specified file."
                  putStrLn "   :p          Prints the currently loaded program."
                  putStrLn "   :q          Exits the interactive environment."
                  putStrLn "   :r          Reloads the last loaded file."
                  putStrLn "   :s <strat>  Sets the specified search strategy"
                  putStrLn "               where <strat> is one of 'dfs' or 'bfs'."
                  putStrLn "   :t <goal>   Prints the SLD tree for the specified goal."
                  loop p s f
cases p s f ":l" file = do
                  fl <- parseFile file
                  case fl of
                     Left err -> putStrLn err >> loop p s f
                     Right pr -> putStrLn "Loaded." >> loop pr s file
cases (Prog ps) s f ":p" _ | null ps = putStrLn "No program loaded." >> loop (Prog ps) s f
                           | otherwise = do
                                 putStrLn "Loaded program:"
                                 putStrLn (pretty (Prog ps))
                                 loop (Prog ps) s f
cases _ _ _ ":q" _ = return ()
cases (Prog ps) s f ":r" _ | null ps = putStrLn "No program loaded." >> loop (Prog ps) s f
                           | otherwise = do
                                 fl <- parseFile f
                                 case fl of
                                    Left err -> putStrLn err >> loop (Prog ps) s f
                                    Right pr -> putStrLn "File reloaded." >> loop pr s f
cases p s f ":s" arg | arg == "bfs" = do
                        putStrLn "Strategy set to 'breadth-first search'."
                        loop p bfs f
                     | arg == "dfs" = do
                        putStrLn "Strategy set to 'depth-first search'."
                        loop p dfs f
                     | otherwise = do
                        putStrLn "Sorry, strategy not found. Try a different command"
                        loop p s f
cases p s f ":t" goal = do
                     case parse goal of
                        Left err -> putStrLn err
                        Right gl -> putStrLn (pretty (sld p gl))
                     loop p s f
cases p s f goal _ = do
                     case parse goal of
                        Left err -> putStrLn err
                        Right gl -> printGoals p gl s f
                     loop p s f

printGoals :: Prog -> Goal -> Strategy -> String -> IO () 
printGoals p g s f = printSubstList (solveWith p g s)
   where 
      printSubstList [] = putStrLn "No more solutions."
      printSubstList (x:xs) = do 
                     putStr (pretty x ++ " ")
                     input <- getLine
                     if input == ";" 
                        then printSubstList xs
                        else loop p s f
