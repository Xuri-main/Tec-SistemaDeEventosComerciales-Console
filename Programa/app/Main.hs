module Main where

import System.IO (hFlush, stdout)
import Evento
import Generador
import Transformador
import Analizador
import Temporal
import Busqueda
import Archivos
import Estadisticas

main :: IO ()
main = do
    putStrLn "[INFO] Iniciando Sistema de Eventos Comerciales..."
    putStrLn "[INFO] Buscando base de datos anterior..."
    eventosAnteriores <- cargarEventos
    putStrLn $ "[INFO] Se cargaron " ++ show (length eventosAnteriores) ++ " eventos en memoria."
    menuPrincipal eventosAnteriores

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
            menuAnalisis eventos

        "4" -> do
            menuTemporal eventos

        "5" -> do
            menuBusqueda eventos

        "6" -> do
            menuEstadisticas eventos

        "7" -> do
            putStrLn "\n[INFO] Guardando base de datos..."
            guardarEventos eventos
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

menuAnalisis :: [Evento] -> IO ()
menuAnalisis eventos = do
    putStrLn "\n========================================"
    putStrLn "          ANALISIS DE DATOS"
    putStrLn "========================================"
    putStrLn "1. Categoria mas frecuente"
    putStrLn "2. Total de ventas (compras globales)"
    putStrLn "3. Total de ventas mensuales y anuales"
    putStrLn "4. Promedio de ventas por categoria por anio"
    putStrLn "5. Volver al menu principal"
    putStrLn "========================================"
    putStr "Seleccione una opcion: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            let frecuente = categoriaMasFrecuente eventos
            putStrLn $ "\n[INFO] La categoria mas frecuente es: " ++ frecuente
            menuAnalisis eventos

        "2" -> do
            let total = totalDeVentas eventos
            putStrLn $ "\n[INFO] El total de todas las compras es: " ++ show total
            menuAnalisis eventos

        "3" -> do
            putStrLn "\n[INFO] Total de ventas por (Anio, Mes):"
            mapM_ print (ventasPorMesYAnio eventos)
            menuAnalisis eventos

        "4" -> do
            putStrLn "\n[INFO] Promedio por (Categoria, Anio):"
            mapM_ print (promedioPorCategoriaYAnio eventos)
            menuAnalisis eventos

        "5" -> do
            menuPrincipal eventos

        _ -> do
            putStrLn "\n[ERROR] Opcion no valida."
            menuAnalisis eventos

menuTemporal :: [Evento] -> IO ()
menuTemporal eventos = do
    putStrLn "\n========================================"
    putStrLn "          ANALISIS TEMPORAL"
    putStrLn "========================================"
    putStrLn "1. Mes con mayor venta y dia mas activo"
    putStrLn "2. Evento mas antiguo y reciente"
    putStrLn "3. Resumen de ventas por intervalo"
    putStrLn "4. Volver al menu principal"
    putStrLn "========================================"
    putStr "Seleccione una opcion: "
    hFlush stdout
    opcion <- getLine

    if null eventos && opcion `elem` ["1", "2", "3"]
        then do
            putStrLn "\n[ERROR] No hay eventos cargados. Vaya a 'Gestionar Eventos' y genere datos primero."
            menuTemporal eventos
        else case opcion of
            "1" -> do
                let ((anio, mes), totalVendido) = mesMayorVenta eventos
                let diaActivo = diaMasActivo eventos
                putStrLn $ "\n[INFO] Mes con MAYOR VENTA: " ++ show mes ++ "/" ++ show anio ++ " (Total: $" ++ show totalVendido ++ ")"
                putStrLn $ "[INFO] Dia mas activo (mas cantidad de eventos): " ++ diaActivo
                menuTemporal eventos

            "2" -> do
                let (antiguo, reciente) = extremosTemporales eventos
                putStrLn "\n[INFO] --- Evento mas ANTIGUO ---"
                print antiguo
                putStrLn "\n[INFO] --- Evento mas RECIENTE ---"
                print reciente
                menuTemporal eventos

            "3" -> do
                putStrLn "\n[INFO] Modulo en mantenimiento..."
                menuTemporal eventos

            "4" -> do
                menuPrincipal eventos

            _ -> do
                putStrLn "\n[ERROR] Opcion no valida."
                menuTemporal eventos

menuBusqueda :: [Evento] -> IO ()
menuBusqueda eventos = do
    putStrLn "\n========================================"
    putStrLn "          BUSQUEDA ESPECIFICA"
    putStrLn "========================================"
    putStrLn "1. Buscar ventas por rango de fechas"
    putStrLn "2. Volver al menu principal"
    putStrLn "========================================"
    putStr "Seleccione una opcion: "
    hFlush stdout

    opcion <- getLine
    
    if null eventos && opcion == "1"
        then do
            putStrLn "\n[ERROR] No hay eventos cargados para buscar."
            menuBusqueda eventos
        else case opcion of
            "1" -> do
                putStrLn "\n--- INGRESE LA FECHA DE INICIO ---"
                putStr "Anio (ej. 2026): "
                hFlush stdout
                anio1Str <- getLine
                putStr "Mes (1-12): "
                hFlush stdout
                mes1Str <- getLine
                putStr "Dia (1-31): "
                hFlush stdout
                dia1Str <- getLine

                putStrLn "\n--- INGRESE LA FECHA DE FIN ---"
                putStr "Anio (ej. 2026): "
                hFlush stdout
                anio2Str <- getLine
                putStr "Mes (1-12): "
                hFlush stdout
                mes2Str <- getLine
                putStr "Dia (1-31): "
                hFlush stdout
                dia2Str <- getLine

                let fInicio = (read anio1Str :: Integer, read mes1Str :: Int, read dia1Str :: Int)
                let fFin    = (read anio2Str :: Integer, read mes2Str :: Int, read dia2Str :: Int)

                putStrLn "\n[INFO] Buscando..."
                let resultados = buscarVentasPorRango fInicio fFin eventos
                
                putStrLn $ "[INFO] Se encontraron " ++ show (length resultados) ++ " ventas en ese rango:\n"
                
                if null resultados 
                    then putStrLn "No hubo ventas en las fechas indicadas."
                    else mapM_ print resultados
                
                menuBusqueda eventos

            "2" -> do
                menuPrincipal eventos

            _ -> do
                putStrLn "\n[ERROR] Opcion no valida."
                menuBusqueda eventos

menuEstadisticas :: [Evento] -> IO ()
menuEstadisticas eventos = do
    putStrLn "\n========================================"
    putStrLn "              ESTADISTICAS"
    putStrLn "========================================"
    putStrLn "1. Categoria mas vendida"
    putStrLn "2. Categoria con menor participacion"
    putStrLn "3. Resumen general"
    putStrLn "4. Volver al menu principal"
    putStrLn "========================================"
    putStr "Seleccione una opcion: "
    hFlush stdout
    opcion <- getLine
    if null eventos && opcion `elem` ["1", "2", "3"]
        then do
            putStrLn "\n[ERROR] No hay eventos cargados para calcular estadisticas."
            menuEstadisticas eventos
        else case opcion of
            "1" -> do
                putStrLn "\n[INFO] Calculando Categoria mas vendida..."
                let lineasA = reporteA eventos
                mapM_ putStrLn (drop 1 lineasA) 
                
                -- Guardamos el archivo físico
                exportarReporte "Estadistica_A.csv" lineasA
                menuEstadisticas eventos

            "2" -> do
                putStrLn "\n[INFO] Calculando Categoria con menor participacion..."
                let lineasB = reporteB eventos
                mapM_ putStrLn (drop 1 lineasB)
                
                exportarReporte "Estadistica_B.csv" lineasB
                menuEstadisticas eventos

            "3" -> do
                putStrLn "\n[INFO] Generando Resumen General..."
                let lineasC = reporteC eventos
                putStrLn "[INFO] Vista previa generada correctamente."
                
                exportarReporte "Estadistica_C.csv" lineasC
                menuEstadisticas eventos

            "4" -> do
                menuPrincipal eventos

            _ -> do
                putStrLn "\n[ERROR] Opcion no valida."
                menuEstadisticas eventos
