module Mturing(Alfabeto,Cuadruplas, Accion(Dcha,Izq,Escribir,Ninguna), MT, calcula, tabla, calculo) where
import Data.Char
--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

--Renombramientos y definiciones de tipos.
type Simbolo = Char 
type Estado = Int 
type Alfabeto = [Simbolo] 
type Cinta = [Simbolo]

data Accion = Izq | Dcha | Escribir Char |  Ninguna
    deriving(Show,Eq)

type Cuadruplas = [(Estado, Simbolo, Accion, Estado)]

--Ejemplo de tabla de estados.
tabla :: Cuadruplas
tabla = [(1,'#',Dcha,2), (2,'1',Dcha,3), (3,'1',Dcha,2), (3,'#', Escribir '#',3)]

--Funcion que te dice que resultado obtienes con un estado y simbolo
delta :: Cuadruplas -> (Estado,Simbolo) -> (Accion,Estado)
delta [] (en,sn) = (Ninguna, -1)
delta ((e0,s,a,ea):xs) (en, sn) = if e0 == en && s == sn then (a,ea) else delta xs (en,sn)

-- MT y Configuracion

type MT = (Alfabeto,Cuadruplas)

data Configuracion = C Estado (Cinta,Simbolo,Cinta)

--Instancia del show

mostrarCaracterEspacios :: [Char] -> Char -> String 
mostrarCaracterEspacios [] e = " " ++ [e] ++ "\n"
mostrarCaracterEspacios (x:xs) e = " " ++ mostrarCaracterEspacios xs e

instance Show Configuracion where
   show (C e (cIzq,s,cDcha)) = cIzq ++ [s] ++ cDcha ++ "\n" ++ mostrarCaracterEspacios cIzq '^' ++ mostrarCaracterEspacios cIzq (intToDigit e)

--Funcion que actualiza la cinta según la acción y la cinta de entrada. Si la acción es ninguna se crea esa cinta especial para determinar que no hay acción posible.

actualizaCinta :: Accion -> (Cinta,Simbolo,Cinta) -> (Cinta,Simbolo,Cinta)
actualizaCinta a (cIzq,s,cDcha)
    | a == Dcha && null cDcha = (cIzq++[s], ' ', [])
    | a == Izq  && null cIzq = ([], ' ', cDcha++[s])
    | a == Dcha = (cIzq++[s], head cDcha, tail cDcha)
    | a == Izq = (init cIzq, last cIzq, [s]++cDcha)
    | a == Ninguna = ([],'?',[])
actualizaCinta (Escribir c) (cIzq,s,cDcha) = if c == '#' then (cIzq, s,cDcha) else (cIzq,c,cDcha)

--Función que determina los pasos de calculo que hay qeu seguir de una configuración hasta la final si es posible, si no es posible devuelve los pasos que ha podido dar.
--Si ha podido dar alguno.

pasosCalculo :: MT -> Configuracion -> [Configuracion]
pasosCalculo (a,xs) (C e (cIzq,s,cDcha))  
    | sN == ' ' = [C e (cIzq,s,cDcha)] ++ [(C estadoN (cIzqN,sN,cDchaN))]
    | sV == ' ' = [C e (cIzq,s,cDcha)] ++ [(C estadoV (cIzqV,sV,cDchaV))]
    | accion /= Ninguna = [C e (cIzq,s,cDcha)] ++ pasosCalculo (a,xs) (C estadoN (cIzqN,sN,cDchaN))
    | accionV /= Ninguna = [C e (cIzq,s,cDcha)] ++ pasosCalculo (a,xs) (C estadoV (cIzqV,sV,cDchaV))
    | otherwise  = []
    where (accionV, estadoV) = delta xs (e,'#')
          (accion,estadoN) = delta xs (e,s)
          (cIzqN,sN,cDchaN) = actualizaCinta accion (cIzq,s,cDcha)
          (cIzqV, sV, cDchaV) = actualizaCinta accionV (cIzq,s,cDcha)

--Realiza el calculo completo con una MT y un string de entrada.

calculo :: MT -> String -> [Configuracion]
calculo (a,cuads) cad = pasosCalculo (a,cuads) (C 1 ([], ' ', cad))

--Funcion auxiliar para coger la cinta de una configuracion.
cogerCinta :: Configuracion -> Cinta
cogerCinta (C e (i,s,d)) = i++[s]

--Funcion que determina cual es la cinta de la configuración final.
calcula :: MT -> String -> Cinta 
calcula (a,cuads) cad  = if not (null xs) then cogerCinta(last xs) else []
    where xs = calculo (a,cuads) cad

    


















--calculo :: MT -> String -> [Configuracion]
--calculo (a,xs,ea) [] = []
--calculo (alf,xs,ea) (c:cs) = if a == Ninguna then [] else [C e s cIzq cDcha] ++ calculo (alf,xs,e) cs
 --   where (cIzq, s, cDcha) = actualizaCinta a ([],c,cs)
    --      (a,e) = delta xs (ea,c) 
          
--calcula :: MT -> String -> Cinta 
---calcula (a,xs,ea) cad = cIzq ++ [s] ++ cDcha 
 --   where (C e s cIzq cDcha) = head (calculo (a,xs,ea) cad)