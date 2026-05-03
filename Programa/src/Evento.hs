module Evento where

data Evento = Evento {
    idEvento  :: Int,
    categoria :: String,
    valor     :: Double,
    timestamp :: Int
} deriving (Show, Eq)