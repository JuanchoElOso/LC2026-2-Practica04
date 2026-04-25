module Practica04 where

-- Sintaxis de la lógica proposicional
data Prop
  = Var String
  | Cons Bool
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  | Impl Prop Prop
  | Syss Prop Prop
  deriving (Eq)

instance Show Prop where
  show (Cons True) = "⊤"
  show (Cons False) = "⊥"
  show (Var px) = px
  show (Not px) = "¬" ++ show px
  show (Or px qx) = "(" ++ show px ++ " ∨ " ++ show qx ++ ")"
  show (And px qx) = "(" ++ show px ++ " ∧ " ++ show qx ++ ")"
  show (Impl px qx) = "(" ++ show px ++ " → " ++ show qx ++ ")"
  show (Syss px qx) = "(" ++ show px ++ " ↔ " ++ show qx ++ ")"

type Literal = Prop
type Clausula = [Literal]

-- Variables de ejemplo
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

-- Definicion de los tipos para la practica
type Interpretacion = [(String, Bool)]
type Estado = (Interpretacion, [Clausula])

data ArbolDPLL = Node Estado ArbolDPLL | Branch Estado ArbolDPLL ArbolDPLL | Void deriving (Show)

-- Ejercicio 1: Conflict
conflict :: Estado -> Bool
conflict (_, clausulas) = verificarVacia clausulas

verificarVacia :: [Clausula] -> Bool
verificarVacia [] = False
verificarVacia (c:cs) = if esClausulaVacia c then True else verificarVacia cs

esClausulaVacia :: Clausula -> Bool
esClausulaVacia [] = True
esClausulaVacia _ = False

-- Ejercicio 2: Success
success :: Estado -> Bool
success (_, clausulas) = arregloVacio clausulas

arregloVacio :: [Clausula] -> Bool
arregloVacio [] = True
arregloVacio _ = False

-- Ejercicio 3: Unit (y funciones auxiliares)
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

-- Ejercicio 4: Elim (y funciones auxiliares)
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

-- Ejercicio 5: Red (y funciones auxiliares)
red :: Estado -> Estado
red (interp, clausulas) = (interp, limpiarFalsas interp clausulas)

limpiarFalsas :: Interpretacion -> [Clausula] -> [Clausula]
limpiarFalsas _ [] = []
limpiarFalsas interp (c:cs) = 
    quitarFalsos interp c : limpiarFalsas interp cs

quitarFalsos :: Interpretacion -> Clausula -> Clausula
quitarFalsos _ [] = []
quitarFalsos interp (l:ls) =
    if esLiteralFalso interp l
    then quitarFalsos interp ls
    else l : quitarFalsos interp ls

esLiteralFalso :: Interpretacion -> Literal -> Bool
esLiteralFalso interp (Var px) = tieneVfalso px interp
esLiteralFalso interp (Not (Var px)) = tieneVcierto px interp
esLiteralFalso _ _ = False

-- Ejercicio 6: Sep
sep :: Literal -> Estado -> (Estado, Estado)
sep l (interp, clausulas) = 
    let nombre = nombreV l
    in (((nombre, True):interp, clausulas), ((nombre, False):interp, clausulas))
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