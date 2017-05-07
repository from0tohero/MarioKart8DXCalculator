module Search 
    ( search )
    where

import Type
import Factory
import Data.Monoid ((<>))

search spd acc wgt hdl grip = do
    cc <- characters
    ab <- allBodies
    tt <- tires
    gg <- gliders
    let candidates = [c <> b <> t <> g | c <- cc, b <- ab, t <- tt, g <- gg]
        model = createBasicModel spd acc wgt hdl grip
        createBasicModel spd acc wgt hdl grip =
                Object { name = ""
                      , description = ""
                      , speed = Values spd 0 0 0
                      , acceleration = acc
                      , weight = wgt
                      , handling = Values hdl 0 0 0
                      , traction = grip
                      , miniTurbo = 0
                     }
    return $ filter (model <) candidates
