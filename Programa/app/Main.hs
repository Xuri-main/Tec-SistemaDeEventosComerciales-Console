module Main where

import System.IO (hFlush, stdout)
import Evento
import Generador
import Transformador

main :: IO ()
main = do
    putStrLn "[INFO] Iniciando Sistema de Eventos Comerciales..."
    menuPrincipal []

menuPrincipal :: [Evento] -> IO ()
menuPrincipal eventos = do
    putStrLn "\n========================================"
    putStrLn "            MENU PRINCIPAL"
    putStrLn "========================================"
    putStrLn "1. Gestionar Eventos"
    putStrLn "2. Transformacion de eventos"
    putStrLn "3. Analisis de datos"
    putStrLn "4. Analisis temporal"
    putStrLn "5. Busqueda"
    putStrLn "6. Estadisticas"
    putStrLn "7. Salir"
    putStrLn "========================================"
    putStr "Seleccione una opcion: "
    hFlush stdout 
    opcion <- getLine

    case opcion of
        "1" -> do
            menuGestionEventos eventos

        "2" -> do
            menuTransformacion eventos

        "3" -> do
            putStrLn "\n[INFO] Modulo en mantenimiento..."
            menuPrincipal eventos

        "4" -> do
            putStrLn "\n[INFO] Modulo en mantenimiento..."
            menuPrincipal eventos

        "5" -> do
            putStrLn "\n[INFO] Modulo en mantenimiento..."
            menuPrincipal eventos

        "6" -> do
            putStrLn "\n[INFO] Modulo en mantenimiento..."
            menuPrincipal eventos

        "7" -> do
            putStrLn "\n[INFO] Saliendo del sistema..."

        _ -> do
            putStrLn "\n[ERROR] Opcion no valida. Intente de nuevo."
            menuPrincipal eventos

menuGestionEventos :: [Evento] -> IO ()
menuGestionEventos eventos = do
    putStrLn "\n========================================"
    putStrLn "          GESTION DE EVENTOS"
    putStrLn "========================================"
    putStrLn "1. Cantidad de eventos"
    putStrLn "2. Generar eventos"
    putStrLn "3. Volver al menu principal"
    putStrLn "========================================"
    putStr "Seleccione una opcion: "
    hFlush stdout
    opcion <- getLine
    
    case opcion of
        "1" -> do
            putStrLn $ "\n[INFO] Cantidad de eventos cargados: " ++ show (length eventos)
            menuGestionEventos eventos

        "2" -> do
            putStrLn "\n[INFO] Generando eventos..."
            nuevosEventos <- generarEventos eventos
            putStrLn $ "[INFO] Se han generado " ++ show (length nuevosEventos) ++ " eventos nuevos."       
            let listaActualizada = eventos ++ nuevosEventos
            menuGestionEventos listaActualizada

        "3" -> do
            menuPrincipal eventos

        _ -> do
            putStrLn "\n[ERROR] Opcion no valida."
            menuGestionEventos eventos

menuTransformacion :: [Evento] -> IO ()
menuTransformacion eventos = do
    putStrLn "\n========================================"
    putStrLn "        TRANSFORMACION DE EVENTOS"
    putStrLn "========================================"
    putStrLn "1. Aplicar impuesto a compras (13%)"
    putStrLn "2. Etiquetar eventos de alto valor"
    putStrLn "3. Volver al menu principal"
    putStrLn "========================================"
    putStr "Seleccione una opcion: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "\n[INFO] Aplicando 13% de impuesto a las compras..."
            let eventosActualizados = aplicarImpuesto eventos
            putStrLn "[INFO] Impuestos aplicados con exito."
            menuTransformacion eventosActualizados

        "2" -> do
            putStrLn "\n[INFO] Analizando promedios y etiquetando eventos..."
            let eventosActualizados = etiquetarAltoValor eventos
            putStrLn "[INFO] Etiquetas de 'Alto Valor' aplicadas con exito."
            
            menuTransformacion eventosActualizados

        "3" -> do
            menuPrincipal eventos

        _ -> do
            putStrLn "\n[ERROR] Opcion no valida."
            menuTransformacion eventos