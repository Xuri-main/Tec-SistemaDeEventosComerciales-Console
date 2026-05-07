module Transformador where

import Evento


{-
Función: aplicarImpuesto
Objetivo: Aplicar un impuesto del 13% al valor de los eventos de compra.
Entrada: Una lista de eventos.
Salida: Una lista de eventos actualizada.
Restricciones: Solo modifica eventos con categoría "compra". Los demás eventos se mantienen igual.
-}
aplicarImpuesto :: [Evento] -> [Evento]
aplicarImpuesto [] = []
aplicarImpuesto (ev:resto) =
    if categoria ev == "compra"
        then ev { valor = valor ev * 1.13 } : aplicarImpuesto resto

        else ev : aplicarImpuesto resto



-- 2. ETIQUETAR ALTO VALOR

{-
Función: promedioPorCategoria
Objetivo: Calcular el promedio de valor de una categoría específica.
Entrada: Una categoría y una lista de eventos.
Salida: Un Double con el promedio de esa categoría.
Restricciones: Si no hay eventos de esa categoría, devuelve 0 para evitar división entre cero.
-}
promedioPorCategoria :: String -> [Evento] -> Double
promedioPorCategoria cat todosLosEventos =
    let eventosDeCategoria = filter (\e -> categoria e == cat) todosLosEventos
        sumaTotal = sum (map valor eventosDeCategoria)
        cantidad = length eventosDeCategoria

    in if cantidad == 0 
       then 0 
       else sumaTotal / fromIntegral cantidad


{-
Función: etiquetarAltoValor
Objetivo: Etiquetar como "Alto Valor" los eventos cuyo valor supera el promedio de su categoría.
Entrada: Una lista de eventos.
Salida: Una lista de eventos donde algunos pueden tener la etiqueta "Alto Valor".
Restricciones: El promedio se calcula usando todos los eventos de la misma categoría. Si el valor no supera el promedio, el evento queda igual.
-}
etiquetarAltoValor :: [Evento] -> [Evento]
etiquetarAltoValor todosLosEventos = etiquetarAux todosLosEventos todosLosEventos
  where
    etiquetarAux _ [] = []
    etiquetarAux listaCompleta (ev:resto) =
        let promedio = promedioPorCategoria (categoria ev) listaCompleta
        in if valor ev > promedio
           then ev { etiqueta = "Alto Valor" } : etiquetarAux listaCompleta resto
           else ev : etiquetarAux listaCompleta resto