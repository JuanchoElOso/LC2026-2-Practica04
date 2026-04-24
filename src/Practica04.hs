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
conflict = undefined

--Ejercicio 2
success :: Estado -> Bool
success = undefined

--Ejercicio 3
unit :: Estado -> Estado
unit = unit :: Estado -> Estado
unit (interp, clausulas) = 
    let (unidad, resto) = extraerUnitaria clausulas [] 


    extraerUnitaria :: [Clausula] -> [Clausula] -> (Clausula, [Clausula])
extraerUnitaria [] previas = ([], previas)

--Ejercicio 4.
elim :: Estado -> Estado
elim (interp, clausulas) = (interp, limpiarCiertas interp clausulas)

limpiarCiertas :: Interpretacion -> [Clausula] -> [Clausula]
limpiarCiertas _ [] = []
limpiarCiertas interp (c:cs) =
    if clausulaVerdadera interp c
    then limpiarCiertas interp cs
    else c : limpiarCiertas interp cs

clausulaVerdadera :: Interpretacion -> Clausula -> Bool
clausulaVerdadera _ [] = False
clausulaVerdadera interp (l:ls) =
    if esLiteralVerdadero interp l
    then True
    else clausulaVerdadera interp ls

esLiteralVerdadero :: Interpretacion -> Literal -> Bool
esLiteralVerdadero interp (Var px) = tieneVcierto px interp
esLiteralVerdadero interp (Not (Var px)) = tieneVfalso px interp
esLiteralVerdadero _ _ = False

tieneVcierto :: String -> Interpretacion -> Bool
tieneVcierto _ [] = False
tieneVcierto px ((x, v):is) = if px == x && v == True then True else tieneVcierto px is

tieneVfalso :: String -> Interpretacion -> Bool
tieneVfalso _ [] = False
tieneVfalso px ((x, v):is) = if px == x && v == False then True else tieneVfalso px is

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