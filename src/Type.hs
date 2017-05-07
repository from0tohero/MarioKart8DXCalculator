module Type where

import Data.Monoid ((<>))

data Values = Values { onGround :: Double
                     , underWater :: Double
                     , inSky :: Double
                     , antiGravity :: Double
                     }
                     deriving (Eq, Ord, Show)

data Object = Object { name         :: String
                     , description  :: String
                     , speed        :: Values
                     , acceleration :: Double
                     , weight       :: Double
                     , handling     :: Values
                     , traction     :: Double
                     , miniTurbo    :: Double
                     }
                     deriving (Show)

instance Eq Object where
    lhs == rhs =
           (speed lhs) == (speed rhs)
        && (acceleration lhs) == (acceleration rhs)
        && (weight lhs) == (weight rhs)
        && (handling lhs) == (handling rhs)
        && (traction lhs) == (traction rhs)
        && (miniTurbo lhs) == (miniTurbo rhs)

instance Ord Object where
    lhs <= rhs =
            (speed lhs) <= (speed rhs)
        && (acceleration lhs) <= (acceleration rhs)
        && (weight lhs) <= (weight rhs)
        && (handling lhs) <= (handling rhs)
        && (traction lhs) <= (traction rhs)
        && (miniTurbo lhs) <= (miniTurbo rhs)

instance Monoid Values where
    mempty
        = Values 0 0 0 0
    mappend (Values g w s a) (Values g' w' s' a')
        = Values (g + g') (w + w') (s + s') (a + a')

instance Monoid Object where
    mempty
        = Object { name         = "<No name> "
                 , description  = mempty
                 , speed        = mempty
                 , acceleration = 0
                 , weight       = 0
                 , handling     = mempty
                 , traction     = 0
                 , miniTurbo    = 0
                 }
    mappend (Object n d s a w h t m) (Object n' d' s' a' w' h' t' m')
        = Object (n ++ " + " ++ n') (d <> d') (s <> s') (a + a') (w + w') (h <> h') (t + t') (m + m')
