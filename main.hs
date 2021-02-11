import Mturing (Alfabeto, Cuadruplas, Accion(Dcha,Izq,Escribir,Ninguna), MT, calcula, tabla, calculo )

--Al final no he utilizado esta función, pero la dejo hecha, se requería en el ejercicio.
aceptaPalabra :: MT -> IO()
aceptaPalabra (a,cuads) = putStrLn "Introduzca una cadena a procesar" >>  getLine >>= \cad -> print (calcula (a,cuads) cad)

--Funcion que parsea todas las cuadras.
devuelveCuads :: [String] -> Cuadruplas 
devuelveCuads []  = [] 
devuelveCuads (x:xs) =  [(ea,s,accion,en)] ++ devuelveCuads xs
        where (ea, s, accion, en) = parseaCuadra x

--Funcion que parsea una cuadra.
parseaCuadra :: String -> (Int,Char, Accion, Int)
parseaCuadra xs 
    | a == "@d" =  (read [head e], head s, Dcha , read [head en])
    | a == "@i" = (read [head e], head s, Izq , read [head en])
    | take 2 a == "@s" = (read [head e], head s, Escribir (last a) , read [head en])
    where 
        en = cs !! 3
        a = cs !! 2
        s = cs !! 1 
        e = head cs
        cs = words xs

--Funcion que imprime el calculo de una palabra con una MT
imprimeCalculo :: Cuadruplas -> Alfabeto -> String -> IO ()
imprimeCalculo cuads alf cad = print (calculo (alf,cuads) cad) >> if aceptada then  print "PALABRA ACEPTADA" else print "PALABRA NO ACEPTADA"
    where aceptada = last (calcula (alf,cuads) cad) == ' '

--Funcion que imprime el resultado del calculo, es decir, hasta donde ha podido llegar la máquina de turing.
imprimeCalcula :: Cuadruplas -> Alfabeto -> String -> IO ()
imprimeCalcula cuads alf cad = print (calcula (alf,cuads) cad) 

-- Imprime el calculo completo con s y con n solo la funcion calcula.
main :: IO () 
main = do 
        putStr "Nombre del fichero con la Maquina de Turing: "
        nombreF <- getLine 
        contenidoF <- readFile nombreF
        let cuads = devuelveCuads (lines contenidoF)
        putStr "Alfabeto de la MT (en forma de String): " 
        alfabeto <- getLine 
        putStr "Palabra de entrada: "
        palabra <- getLine 
        putStr "Ver el calculo completo (s/n): "
        respuesta <- getLine 
        case respuesta of 
            "s" -> do 
                    imprimeCalculo cuads alfabeto palabra
            "n" -> do 
                    imprimeCalcula cuads alfabeto palabra
        