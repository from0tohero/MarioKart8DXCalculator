{-# LANGUAGE ApplicativeDo #-}

module Search 
    ( search 
    , searchMain
    )
    where

import Type
import Factory as F
import Data.Monoid ((<>))

search :: Object -> [Object] -> [Object] -> [Object] -> [Object] -> [Object]
search standard characters bodies tires gliders =
    let candidates = [c <> b <> t <> g | c <- characters, b <- bodies, t <- tires, g <- gliders]
    in filter (standard <) candidates


searchMain spdG spdW spdA spdAG acc wgt hdlG hdlW hdlA hdlAG grip miniTurbo favorateChars favorateBodies favorateTires favorateGliders = do
    allCharacters <- F.characters
    allBodies <- F.allBodies
    allTires <- F.tires
    allGliders <- F.gliders
    let standard = Object { name = ""
                          , description = ""
                          , speed = Values spdG spdW spdA spdAG
                          , acceleration = acc
                          , weight = wgt
                          , handling = Values hdlG hdlW hdlA hdlAG
                          , traction = grip
                          , miniTurbo = miniTurbo
                         }
        characters = if (length favorateChars == 0) then allCharacters else favorateChars
        bodies = if (length favorateBodies == 0) then allBodies else favorateBodies
        tires = if (length favorateTires == 0) then allTires else favorateTires
        gliders = if (length favorateGliders == 0) then allGliders else favorateGliders
    
    return $ search standard characters bodies tires gliders
