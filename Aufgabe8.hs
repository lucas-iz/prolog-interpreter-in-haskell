{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Parser
import Type
import Aufgabe2
import Aufgabe7

main :: IO ()
main = do  
    putStrLn "Welcome!"
    putStrLn "Type \":h\" for help." 
    loop (Prog []) "dfs" ""

loop :: Prog -> String -> String -> IO ()
loop p s f = do
   -- program <- p
   putStr "?- "
   input <- getLine
   if length (words input) == 2 then cases p s f (head (words input)) (last (words input)) 
                        else if length (words input) == 1 then cases p s f (head (words input)) ""
                                                  else putStrLn "Sorry, Too many arguments. Try a different command" >> loop p s f

cases :: Prog -> String -> String -> String -> String -> IO ()
cases p s f ":h" _ = do
                  putStrLn "Commands available from the prompt:"
                  putStrLn "   <goal>      Solves/proves the specified goal."
                  putStrLn "   :h          Shows this help message."
                  putStrLn "   :l <file>   Loads the specified file."
                  putStrLn "   :p          Prints the currently loaded program."
                  putStrLn "   :q          Exits the interactive environment."
                  putStrLn "   :r          Reloads the last loaded file."
                  putStrLn "   :s <strat>  Sets the specified search strategy"
                  putStrLn "               where <strat> is one of 'dfs', 'bfs', or 'iddfs'."
                  putStrLn "   :t <goal>   Prints the SLD tree for the specified goal."
                  loop p s f
cases p s f ":l" file = do
                  fl <- parseFile file
                  case fl of
                     Left err -> putStrLn err >> loop p s f
                     Right pr -> putStrLn "New File loaded." >> loop pr s file
cases p s f ":p" _ = do
                  putStrLn (pretty p)
                  loop p s f
cases _ _ _ ":q" _ = return ()
cases p s f ":r" _ = do
                  fl <- parseFile f
                  case fl of
                     Left err -> putStrLn err >> loop p s f
                     Right pr -> putStrLn "File reloaded." >> loop pr s f
cases p s f ":s" arg | arg == "bfs" = do 
                        putStrLn "Strategy set to 'breadth-first search'."
                        -- Change strategy
                        loop p "bfs" f
                     | arg == "dfs" = do 
                        putStrLn "Strategy set to 'depth-first search'."
                        -- Change strategy
                        loop p "dfs" f
                     | arg == "iddfs" = do 
                        putStrLn "Strategy set to 'iterative deepening depth-first search'."
                        -- Change strategy
                        loop p "iddfs" f
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
                        Right gl -> putStrLn (concatMap pretty (dfs (sld p gl)))
                     loop p s f
cases p s f _ _ = loop p s f