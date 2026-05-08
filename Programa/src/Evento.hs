{-
 - Archivo: Evento.hs
 - Descripcion: Define la estructura base de los datos (tipo Evento) y centraliza las constantes y herramientas globales del sistema.
 - Autores: Emilio Funes R. , Ginger Rodríguez G. & Jareck Levell C.
 -}

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


{-
Global: categorias
Objetivo: Guardar las categorías válidas que puede tener un evento.
Entrada: No recibe entrada.
Salida: Una lista de String con las categorías disponibles.
Restricciones: Los eventos generados solo usarán categorías que estén dentro de esta lista.
-}
categoriasGlobales :: [String]
categoriasGlobales = ["visualizacion", "apartado", "compra", "devolucion", "seguimiento"]

{-
Función: redondear2
Objetivo: Redondear un valor decimal a 2 decimales.
Entrada: Un número Double.
Salida: Un Double redondeado.
Restricciones: El valor debe ser numérico y puede tener pequeñas variaciones por el uso de Double.
-}
redondear2 :: Double -> Double
redondear2 v = fromIntegral (round (v * 100)) / 100.0

{-
Función: mostrar2Decimales
Objetivo: Convertir un número decimal en texto mostrando siempre 2 decimales.
Entrada: Un valor Double.
Salida: Un String con el número formateado.
Restricciones: Redondea el valor antes de mostrarlo. Si la parte decimal tiene un solo dígito, agrega un 0 para mantener el formato.
-}
mostrar2Decimales :: Double -> String
mostrar2Decimales v =
    let numeroTotal = round (v * 100) :: Integer
        parteEntera = numeroTotal `div` 100
        parteDecimal = abs (numeroTotal `mod` 100)
        textoDecimal = if parteDecimal < 10 then "0" ++ show parteDecimal else show parteDecimal
        
    in show parteEntera ++ "." ++ textoDecimal
