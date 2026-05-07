module Evento where

data Evento = Evento {
    idEvento  :: Int,
    categoria :: String,
    valor     :: Double,
    timestamp :: Int,
    etiqueta  :: String
} deriving (Show, Eq)