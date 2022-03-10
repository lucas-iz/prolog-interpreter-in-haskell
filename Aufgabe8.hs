import Parser
import Type

main :: IO ()
main = do  
    putStrLn "Welcome!"
    putStrLn "Type \":h\" for help." 
    loop (Prog []) (Goal []) "dfs"

loop :: Prog -> Goal -> String -> String -> IO ()
loop p g s f = do
   -- program <- p
   putStr "?- "
   input <- getLine
   if length (words input) == 2 then cases p g s f (head (words input)) (last (words input))
                        else if length (words input) == 1 then cases p g s f (head (words input)) ""
                                                  else putStrLn "Sorry, Too many arguments. Try a different command" >> loop p g s f

cases :: Prog -> Goal -> String -> String -> String -> IO ()
cases p g s f goal _ = if parse goal == (Goal _) then putStrLn (solveWith p goal s) else loop p g s f
cases p g s f ":h" _ = do
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
cases p g s f ":l" file = do
                  putStrLn "New File loaded."
                  loop (parse file) g s file
cases p g s f ":p" _ = putStrLn (pretty p)
cases p g s f ":q" _ = return ()
cases p g s f ":r" _ = do
                  putStrLn "File reloaded."
                  loop (parse f) g s f
cases p g s f ":s" arg 
            | arg == "bfs" = do 
                  putStrLn "Strategy set to 'breadth-first search'."
                  -- Change strategy
                  loop p g "bfs"
            | arg == "dfs" = do 
                  putStrLn "Strategy set to 'depth-first search'."
                  -- Change strategy
                  loop p g "dfs"
            | arg == "iddfs" = do 
                  putStrLn "Strategy set to 'iterative deepening depth-first search'."
                  -- Change strategy
                  loop p g "iddfs"
            | otherwise = putStrLn "Sorry, strategy not found. Try a different command" >> loop p g s f
cases p g s f ":t" goal = putStrLn (pretty (sld p goal))
cases p g s f _ _ = loop p g s f