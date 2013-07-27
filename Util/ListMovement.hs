module Util.ListMovement
    ( ListMovement(..)
    ) where

import Model

import Web.PathPieces

import Data.Functor

data ListMovement = First | After ListId
    deriving (Show, Read, Eq)

instance PathPiece ListMovement where
    fromPathPiece t = if t == "0" then Just First
                      else After <$> fromPathPiece t
    toPathPiece First = "0"
    toPathPiece (After lid) = toPathPiece lid

