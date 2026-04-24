module Practica04 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

type Literal = Prop
type Clausula = [Literal]

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

--Definicion de los tipos para la practica
type Interpretacion = [( String , Bool ) ]
type Estado = ( Interpretacion , [Clausula])
data ArbolDPLL = Node Estado ArbolDPLL | Branch Estado ArbolDPLL ArbolDPLL | Void deriving Show

--IMPLEMENTACION PARTE 1
--Ejercicio 1
conflict :: Estado -> Bool
conflict (_, clausulas) = verificarVacia clausulas

--Ejercicio 2
success :: Estado -> Bool
success (_, clausulas) = arregloVacio clausulas

--Ejercicio 3
unit :: Estado -> Estado
unit (interp, clausulas) = 
    let (unidad, resto) = extraerUnitaria clausulas []
    in case unidad of
        [] -> (interp, clausulas)
        [lit] -> 
            let nombre = nombreV lit
            in if tieneV nombre interp
               then (interp, resto)
               else (asignarV lit interp, resto)
        _ -> (interp, clausulas)

--Ejercicio 4
elim :: Estado -> Estado
elim = undefined

--Ejercicio 5
red :: Estado -> Estado
red = undefined


--Ejercicio 6
sep :: Literal -> Estado -> (Estado, Estado)
sep = undefined

--IMPLEMENTACION PARTE 2


--Ejercicio 1
heuristicsLiteral :: [Clausula] -> Literal
heuristicsLiteral = undefined

--EJERCICIO 2
dpll :: [Clausula] -> Interpretacion
dpll = undefined

--EXTRA
dpll2 :: Prop -> Interpretacion
dpll2 = undefined

-- Funciones auxiliares

verificarVacia :: [Clausula] -> Bool
verificarVacia [] = False
verificarVacia (c:cs) = if clausulaVacia c then True else verificarVacia cs 

clausulaVacia :: Clausula -> Bool
clausulaVacia [] = True
clausulaVacia _ = False

arregloVacio :: [Clausula] -> Bool
arregloVacio [] = True
arregloVacio _ = False

extraerUnitaria :: [Clausula] -> [Clausula] -> (Clausula, [Clausula])
extraerUnitaria [] previas = ([], previas)
extraerUnitaria (c:cs) previas =
    if longitud c == 1
    then (c, previas ++ cs)
    else extraerUnitaria cs (previas ++ [c])

longitud :: [a] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

nombreV :: Literal -> String
nombreV (Var px) = px
nombreV (Not (Var px)) = px
nombreV _ = ""

tieneV :: String -> Interpretacion -> Bool
tieneV _ [] = False
tieneV x ((y, _) : ys) = if x == y then True else tieneV x ys

asignarV :: Literal -> Interpretacion -> Interpretacion
asignarV (Var px) i = (px, True) : i
asignarV (Not (Var px)) i = (px, False) : i
asignarV _ i = i

