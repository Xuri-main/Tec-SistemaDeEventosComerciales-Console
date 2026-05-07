module Evento where

{-
Estructura: Evento
Objetivo: Representar un evento con toda la información necesaria para analizarlo después.
Campos:
idEvento: Identificador unico del evento.
categoria: Tipo de evento, por ejemplo compra, visualizacion, apartado, devolucion o seguimiento.
valor: Monto o valor numérico asociado al evento.
timestamp: Fecha del evento en formato POSIX.
etiqueta: Texto adicional para clasificar o describir el evento.
Restricciones: Los campos deben respetar el tipo de dato definido. Show permite mostrar el evento y Eq permite compararlo.
-}
data Evento = Evento {
    idEvento  :: Int,
    categoria :: String,
    valor     :: Double,
    timestamp :: Int,
    etiqueta  :: String
} deriving (Show, Eq)