import System.Environment (getArgs)

c_NUM_COLS :: Int
c_NUM_COLS = 12

splitOn :: Char -> String -> [String]
splitOn delim str = 
    snd $ foldr f ([], []) (delim : str)
        where 
            f c (cur, acc) = 
                if c == delim then ([], cur : acc)
                              else (c : cur, acc)

applyPattern :: [String] -> String
applyPattern args = 
    "Object { name = \"" ++ args!!0 ++ "\" , description = mempty, "
    ++ "speed = Values " ++ args!!1 ++ " " ++ args!!2 ++ " " ++ args!!3 ++ " " ++ args!!4
    ++ ", acceleration = " ++ args!!5
    ++ ", weight = " ++ args!!6
    ++ ", handling = Values " ++ args!!7 ++ " " ++ args!!8 ++ " " ++ args!!9 ++ " " ++ args!!10
    ++ ", traction = " ++ args!!11
    ++ ", miniTurbo = " ++ args!!12 ++ "}"

main :: IO ()
main = do
    args <- getArgs
    content <- readFile(args !! 0)
    let 
        lines = splitOn '\n' content
        separ = fmap (splitOn '\t') lines 
        res = fmap applyPattern separ
    putStrLn $ show res