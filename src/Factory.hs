module Factory 
    ( characters
    , kartBodies
    , bikeBodies
    , aTVBodies
    , allBodies
    , tires
    , gliders
    )
    where

import Type
import Control.Applicative (liftA3)

--Constants
c_NUM_COLS :: Int
c_NUM_COLS = 12

c_FILE_PREFIX :: String
c_FILE_PREFIX = "../data/"

c_FILE_POSTFIX :: String
c_FILE_POSTFIX = ".dat"

-- Can use Data.List.Split instead.
splitOn :: Char -> String -> [String]
splitOn delim str = 
    snd $ foldr f ([], []) (delim : str)
        where 
            f c (cur, acc) = 
                if c == delim then ([], cur : acc)
                              else (c : cur, acc)

parseItem :: [String] -> Object
parseItem args = 
    Object { name = args!!0
           , description = ""
           , speed = Values (read $ args!!1) (read $ args!!2) (read $ args!!3) (read $ args!!4)
           , acceleration = (read $ args!!5)
           , weight = (read $ args!!6)
           , handling = Values (read $ args!!7) (read $ args!!8) (read $ args!!9) (read $ args!!10)
           , traction = (read $ args!!11)
           , miniTurbo = (read $ args!!12)
       }


loadFromFile :: String -> IO [Object]
loadFromFile filename = do
    content <- readFile filename
    let
        args = fmap (splitOn '\t') $ lines content
    return $ fmap parseItem args

load :: String -> IO [Object]
load name = loadFromFile $ c_FILE_PREFIX ++ name ++ c_FILE_POSTFIX

characters :: IO [Object]
characters = load "characters"

kartBodies :: IO [Object]
kartBodies = load "kartBodies"

bikeBodies :: IO [Object]
bikeBodies = load "bikeBodies"

aTVBodies :: IO [Object]
aTVBodies = load "ATVBodies"

tires :: IO [Object]
tires = load "tires"

gliders :: IO [Object]
gliders = load "gliders"

allBodies :: IO [Object]
allBodies = liftA3 (\x y z -> x ++ y ++ z) kartBodies bikeBodies aTVBodies
