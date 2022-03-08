-- import Parser
import Type

main :: IO ()
main = do  
    putStrLn "Welcome!"
    putStrLn "Type \":h\" for help." 
    loop (Prog [])

loop :: Prog -> IO ()
loop p = do
   -- program <- p
   putStr "?- "
   input <- getLine
   if length (words input) == 2 then cases p (head (words input)) (last (words input))
                        else if length (words input) == 1 then cases p (head (words input)) ""
                                                  else putStrLn "Sorry, Too many arguments. Try a different command" >> loop p

cases :: Prog -> String -> String -> IO ()
-- cases _ (Goal g) _    = do print (parse g)
cases p ":h" _ = do
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
   loop p
cases _ ":l" _ = return ()
cases _ ":p" _ = return ()
cases _ ":q" _ = return ()
cases _ ":r" _ = return ()
cases p ":s" arg 
            | arg == "bfs" = do 
                  putStrLn "Strategy set to 'breadth-first search'."
                  -- Change strategy
                  loop p
            | arg == "dfs" = do 
                  putStrLn "Strategy set to 'depth-first search'."
                  -- Change strategy
                  loop p
            | arg == "iddfs" = do 
                  putStrLn "Strategy set to 'iterative deepening depth-first search'."
                  -- Change strategy
                  loop p
            | otherwise = putStrLn "Sorry, strategy not found. Try a different command" >> loop p
cases _ ":t" _ = return ()
cases p _ _ = loop p