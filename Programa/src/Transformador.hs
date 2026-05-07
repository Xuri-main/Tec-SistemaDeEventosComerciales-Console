module Transformador where

import Evento


-- 1. APLICAR IMPUESTO
aplicarImpuesto :: [Evento] -> [Evento]
aplicarImpuesto [] = []
aplicarImpuesto (ev:resto) =
    if categoria ev == "compra"
        then ev { valor = valor ev * 1.13 } : aplicarImpuesto resto

        else ev : aplicarImpuesto resto



-- 2. ETIQUETAR ALTO VALOR

-- Función auxiliar
promedioPorCategoria :: String -> [Evento] -> Double
promedioPorCategoria cat todosLosEventos =
    let eventosDeCategoria = filter (\e -> categoria e == cat) todosLosEventos
        sumaTotal = sum (map valor eventosDeCategoria)
        cantidad = length eventosDeCategoria

    in if cantidad == 0 
       then 0 
       else sumaTotal / fromIntegral cantidad


-- Función principal
etiquetarAltoValor :: [Evento] -> [Evento]
etiquetarAltoValor todosLosEventos = etiquetarAux todosLosEventos todosLosEventos
  where
    etiquetarAux _ [] = []
    etiquetarAux listaCompleta (ev:resto) =
        let promedio = promedioPorCategoria (categoria ev) listaCompleta
        in if valor ev > promedio
           then ev { etiqueta = "Alto Valor" } : etiquetarAux listaCompleta resto
           else ev : etiquetarAux listaCompleta resto