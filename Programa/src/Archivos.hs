{-
 - Archivo: Archivos.hs
 - Descripcion: Gestiona la persistencia de datos (lectura y escritura local) y la exportación de los reportes estadísticos en formato CSV.
 - Autores: Emilio Funes R. , Ginger Rodríguez G. & Jareck Levell C.
 -}

module Archivos where

import Evento
import System.IO
import System.Directory (doesFileExist)

-- FUNCIONES AUXILIAR: Une los atributos de un Evento en un solo texto separado por comas
eventoACsv :: Evento -> String
eventoACsv ev = show (idEvento ev) ++ "," ++
                categoria ev ++ "," ++
                mostrar2Decimales (valor ev) ++ "," ++  -- AQUI ESTA EL CAMBIO
                show (timestamp ev) ++ "," ++
                etiqueta ev

-- FUNCIONES AUXILIAR: Divide un texto en una lista de palabras cada vez que encuentra una coma.
separarPorComas :: String -> [String]
separarPorComas "" = [""]
separarPorComas (letra:restoLetras)
    | letra == ',' = "" : restoProcesado
    | otherwise    = (letra : head restoProcesado) : tail restoProcesado
    where restoProcesado = separarPorComas restoLetras

-- Convierte una lista de 5 textos
csvAEvento :: [String] -> Evento
csvAEvento [idStr, catStr, valStr, tsStr, etiqStr] = 
    Evento (read idStr) catStr (read valStr) (read tsStr) etiqStr
csvAEvento _ = Evento 0 "error" 0.0 0 "error"

-- PERSISTENCIA DE DATOS (BASE DE DATOS CSV)

guardarEventos :: [Evento] -> IO ()
guardarEventos eventos = do
    let lineasDeTexto = map eventoACsv eventos
    let contenidoFinal = unlines lineasDeTexto
    writeFile "eventos_db.csv" contenidoFinal
    putStrLn "[INFO] Datos guardados exitosamente en 'eventos_db.csv'."

-- Carga los eventos al iniciar
cargarEventos :: IO [Evento]
cargarEventos = do
    existe <- doesFileExist "eventos_db.csv"
    if existe
        then do
            contenido <- readFile "eventos_db.csv"
            let renglones = lines contenido

            let eventosCargados = map (\renglon -> csvAEvento (separarPorComas renglon)) renglones
            return eventosCargados
        else do
            return []


-- EXPORTAR REPORTES
exportarReporte :: String -> [String] -> IO ()
exportarReporte nombreArchivo lineasDelReporte = do
    let contenido = unlines lineasDelReporte
    writeFile nombreArchivo contenido
    putStrLn $ "[INFO] Reporte exportado exitosamente como '" ++ nombreArchivo ++ "'."