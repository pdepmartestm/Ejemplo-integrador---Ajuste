import Text.Show.Functions()

data Empleado = Empleado { 
    nombre :: String, 
    rol :: String, 
    salario :: Int
    } deriving Show

data Propuesta = Propuesta { 
    nuevoRol :: String, 
    funcionSalarial :: Int -> Int
    } deriving Show


---- Parte 1

-- Punto 1
aplicarPropuesta :: Empleado -> Propuesta -> Empleado
aplicarPropuesta unaPersona unaPropuesta = unaPersona{ rol = nuevoRol unaPropuesta, 
                                                       salario = (funcionSalarial unaPropuesta) (salario unaPersona) }

salarioPostPropuesta :: Empleado -> Propuesta -> Int
salarioPostPropuesta unaPersona = salario.aplicarPropuesta unaPersona
--salarioPostPropuesta unaPersona unaPropuesta = (salario.aplicarPropuesta unaPersona) unaPropuesta
--salarioPostPropuesta unaPersona unaPropuesta =  salario (aplicarPropuesta unaPersona unaPropuesta)

propuestaIlegal :: Empleado -> Propuesta -> Bool
propuestaIlegal unaPersona unaPropuesta =  salario unaPersona  > salarioPostPropuesta unaPersona unaPropuesta

-- Punto 2
ilegalParaUno :: Empleado -> Empleado -> Propuesta -> Bool
ilegalParaUno unEmpleado otroEmpleado unaPropuesta = 
    (propuestaIlegal unEmpleado unaPropuesta) && 
    not (propuestaIlegal otroEmpleado unaPropuesta)

propuestaDudosa' :: Empleado -> Empleado -> Propuesta -> Bool
propuestaDudosa' unEmpleado otroEmpleado unaPropuesta = 
    (ilegalParaUno unEmpleado otroEmpleado unaPropuesta) || 
    (ilegalParaUno otroEmpleado unEmpleado unaPropuesta)

propuestaDudosa :: [Empleado] -> Propuesta -> Bool
propuestaDudosa empleados propuesta = 
    any (flip propuestaIlegal propuesta) empleados &&
    any (not.flip propuestaIlegal propuesta) empleados

-- Punto 3
ordenarSegun :: Ord a1 => (a2 -> a1) -> [a2] -> [a2]
ordenarSegun _ [] = []
ordenarSegun funcion (x:xs) = (ordenarSegun funcion [a | a <- xs, (funcion a) >= (funcion x)]) ++ [x] ++ (ordenarSegun funcion [a | a <- xs, (funcion a) < (funcion x)])

ordenarSegun' _ [] = []
ordenarSegun' _ [x] = [x]
ordenarSegun' funcion (x:xs) = ordenarSegun' funcion losMayores ++ [x] ++ ordenarSegun' funcion losMenores
    where losMenores = filter (\a -> funcion a < funcion x) xs
          losMayores = filter (\a -> funcion a >= funcion x) xs

mejorPropuesta :: [Propuesta] -> Empleado -> Propuesta
mejorPropuesta listaDePropuestas unEmpleado = head (ordenarSegun (salarioPostPropuesta unEmpleado) listaDePropuestas)

mejorPropuesta' propuestas empleado = foldl1 (maxSegun (salarioPostPropuesta empleado)) propuestas

maxSegun :: Ord a1 => (a2 -> a1) -> a2 -> a2 -> a2
maxSegun funcion x y
  | funcion x > funcion y = x 
  | otherwise = y

ganaConMejorPropuesta :: [Propuesta] -> Empleado -> Int
ganaConMejorPropuesta propuestas unEmpleado = salarioPostPropuesta unEmpleado (mejorPropuesta propuestas unEmpleado)

-- Solución sin orden ordenarSegun, pero
-- ganaConMejorPropuesta propuestas unEmpleado =  maximum (map (salarioPostPropuesta unEmpleado) propuestas)

-- Punto 4
guitaAhorrada :: Propuesta -> Empleado -> Int
guitaAhorrada unaPropuesta unaPersona = salario unaPersona - salarioPostPropuesta unaPersona unaPropuesta

nosAhorramosGuita :: [Empleado] -> Propuesta -> Int
nosAhorramosGuita listaDeEmpleados unaPropuesta = sum ( (map (guitaAhorrada unaPropuesta)) listaDeEmpleados)

nosAhorramosGuita' :: Propuesta ->[Empleado] ->  Int
nosAhorramosGuita' propuesta = sum.map (guitaAhorrada propuesta)

-- Punto 5 - Transformaciones
-- Transformación A
conLosOjosCerrados :: [Empleado] -> [Empleado]
conLosOjosCerrados empleados = tomarIngeniero empleados:(soloEmpleados empleados)
conLosOjosCerrados' empleados = tomarIngeniero empleados:filter (esDeRol "Backend developer") empleados
conLosOjosCerrados'' empleados = tomarIngeniero empleados:filter (("Backend developer"==).rol) empleados
conLosOjosCerrados''' empleados = tomarIngeniero empleados:filter (\e -> rol e == "Backend developer") empleados
-- Qué pasa si no hay "ingeniero capo master"?

tomarIngeniero :: [Empleado] -> Empleado
tomarIngeniero  = head.(filter (esDeRol  "Ingeniero Capo Master" ))
tomarIngeniero' empleados = head (filter (esDeRol "Ingeniero Capo Master" ) empleados)

soloEmpleados :: [Empleado] -> [Empleado]
soloEmpleados = filter (esDeRol "Backend developer")

esDeRol :: String -> Empleado -> Bool
esDeRol tipo e =  ((== tipo).rol) e
esDeRol' tipo e = rol e == tipo

-- Transformación B
reducirSalario :: Int -> Empleado -> Empleado
reducirSalario numero empleado = empleado{salario = salario empleado - numero}

reduccionViolenta :: Int -> [Empleado] -> [Empleado]
reduccionViolenta numero empleados = map (reducirSalario numero) empleados

-- Transformación C
aplicarMejorPropuesta :: [Propuesta] -> Empleado -> Empleado
aplicarMejorPropuesta propuestas empleado = aplicarPropuesta empleado (mejorPropuesta propuestas empleado)

propuestaGeneral :: [Propuesta] -> [Empleado] -> [Empleado]
propuestaGeneral propuestas empleados = map (aplicarMejorPropuesta propuestas) empleados

-- Transformación D
sueldosTotales :: [Empleado] -> Int
sueldosTotales empleados = sum (map salario empleados)

promedio :: [Empleado] -> Int
promedio empleados = div (sueldosTotales empleados)  (length empleados)

menosQueElPromedio :: [Empleado] -> Empleado -> Bool
menosQueElPromedio empleados empleado = salario empleado < promedio empleados

soloLosQueCobranPoco :: [Empleado] -> [Empleado]
soloLosQueCobranPoco empleados = filter (menosQueElPromedio empleados) empleados

soloLosQueCobranPoco' empleados = filter ((<=prom).salario) empleados
    where prom = sum (map salario empleados) `div` length empleados


---- Parte 2
data Subsidiaria = Subsidiaria { 
    presupuesto :: Int, 
    empleados :: [Empleado]
    } deriving Show

juan,pedro,lisa :: Empleado
juan = Empleado "Juan" "Frontend" 23000
pedro = Empleado "Pedro" "Analista" 44000
lisa = Empleado "Lisa" "Ingeniero Capo Master" 51000

propuestaMaster :: Propuesta
propuestaMaster = Propuesta "Ingeniero Capo Master" (+10)

propuestaMax = Propuesta "Hasta mil" (max 1000)

empleado1 :: Empleado
empleado1 = Empleado "A" "a" 150000

empleado2 :: Empleado
empleado2 = Empleado "B" "b" 200000

carnero :: Empleado
carnero = Empleado "Carnero" "Carnero" 200000

unaListaDeEmpleados :: [Empleado]
unaListaDeEmpleados = [empleado1, empleado2]

subsidiariaEjemplo :: Subsidiaria
subsidiariaEjemplo = Subsidiaria 100000 unaListaDeEmpleados

subsidiariaCarnera :: Subsidiaria
subsidiariaCarnera = Subsidiaria 1000000 [empleado1, carnero]

-- Punto 1
totalSalarios :: Subsidiaria -> Int
totalSalarios subsidiaria = sueldosTotales (empleados subsidiaria)

leAlcanza :: Subsidiaria -> Bool
leAlcanza subsidiaria = presupuesto subsidiaria > totalSalarios subsidiaria
leAlcanza' (Subsidiaria presupuesto empleados) = presupuesto > sueldosTotales empleados 

-- aplicarEnSerie [(+1), (+2), (*4)] 40
-- 172
-- ($) (+1) 40 ------> 41
-- ($) (+2) 41 ------> 43
-- ($) (*4) 43 ------> 172
aplicarEnSerie :: [a -> a] -> a -> a
aplicarEnSerie funciones valorInicial = foldr ($) valorInicial (reverse funciones)

puedePagar :: [Subsidiaria -> Subsidiaria] -> Subsidiaria -> Bool
puedePagar transformaciones subsidiaria = leAlcanza (aplicarEnSerie transformaciones subsidiaria)

puedePagar' transformaciones = leAlcanza.foldr1 (.) transformaciones

--puedePagar [reduccionViolenta 100, conLosOjosCerrados] (Subsidiaria 1000 [juan,pedro,lisa])

-- Punto 2
puntosCarneros :: Subsidiaria -> Int
puntosCarneros subsidiaria | any (esDeRol "Carnero") (empleados subsidiaria) = 0
                            | otherwise = 10


subsidiariaAchicadaEn :: Int -> Subsidiaria -> Subsidiaria
subsidiariaAchicadaEn numero subsidiaria = subsidiaria { empleados = reduccionViolenta numero (empleados subsidiaria) } 


puntosPresupuesto :: Subsidiaria -> Int
puntosPresupuesto subsidiaria | leAlcanza subsidiaria = 0
                              | otherwise = 1 + puntosPresupuesto (subsidiariaAchicadaEn 5000 subsidiaria)


coeficienteDeHuelgosidad :: Subsidiaria -> Int
coeficienteDeHuelgosidad subsidiaria = puntosCarneros subsidiaria + puntosPresupuesto subsidiaria


ordenarSubsidiarias :: [Subsidiaria] -> [Subsidiaria]
ordenarSubsidiarias subsidiarias = ordenarSegun coeficienteDeHuelgosidad subsidiarias
