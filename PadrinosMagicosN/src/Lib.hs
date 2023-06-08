import Text.Show.Functions ()

type Nombre = String
type Habilidad = String
type Deseo = Chico->Chico 
type Condicion = Chico ->Bool

data Chico = Chico {

    nombre::Nombre,
    edad:: Int  ,
    habilidades:: [Habilidad],
    deseos:: [Deseo]
    
}deriving Show 

data Chica = Chica {

    nombreChica:: Nombre,
    condicion:: Condicion

}deriving Show 

aprenderHabilidades:: [Habilidad]-> Chico -> Chico
aprenderHabilidades habilidadess unChico = unChico{habilidades = habilidades unChico ++ habilidadess } 

serGroseroEnNeedForSpeed::Chico->Chico
serGroseroEnNeedForSpeed unChico = unChico{habilidades = habilidades unChico ++ agregarJuegos }

agregarJuegos::[Habilidad]
agregarJuegos = map (\unNumero -> "jugar need for speed " ++ show unNumero) [1..]

serMayor :: Chico-> Chico
serMayor unChico = unChico {edad = 18}

wanda:: Chico->Chico
wanda unChico = cambiarEdad (+1).cumplirDeseo $ unChico

cambiarEdad::(Int ->Int)->Chico->Chico
cambiarEdad funtion unChico = unChico{edad =funtion.edad $ unChico}

cumplirDeseo:: Chico->Chico
cumplirDeseo unChico = head (deseos unChico) unChico 


cosmo :: Chico->Chico
cosmo unChico = cambiarEdad  (flip div  2) unChico

muffinMagico:: Chico -> Chico
muffinMagico unChico = foldr1 (.) (deseos unChico) unChico
 

tieneHabilidad :: Habilidad -> Chico ->Bool
tieneHabilidad unaHabilidad unChico = elem unaHabilidad (habilidades unChico)

esSuperMaduro:: Chico ->Bool
esSuperMaduro unChico = esMayordeEdad unChico && tieneHabilidad "saber manejar" unChico


esMayordeEdad :: Chico->Bool
esMayordeEdad unChico = (>2).edad $ unChico




timmy::Chico
timmy= Chico { nombre = "Timmy",edad= 15 , habilidades = ["saber nadar",  "saber multiplicar"],deseos= [serMayor]}

mario::Chico
mario= Chico { nombre = "Mario",edad= 15 , habilidades = ["ser violinista", "ser un super modelo"],deseos= []}

trixie::Chica
trixie = Chica { nombreChica= "Trixie Tang",condicion = noesTimmy}

vicky::Chica
vicky = Chica {nombreChica = "Vicky",condicion = tieneHabilidad "ser un super modelo"}

dana::Chica 
dana = Chica { nombreChica= "Dana",condicion = tieneHabilidad "saber cocinar"}

noesTimmy::Condicion
noesTimmy unChico = nombre unChico /= "Timmy"


quienConquistarA:: Chica ->[Chico]->Nombre
quienConquistarA unaChica (x:xs) = cumpleCondion unaChica x xs 



cumpleCondion:: Chica -> Chico ->[Chico]->Nombre
cumpleCondion unaChica unChico lista
  | (condicion unaChica) unChico = nombre unChico 
  |  null lista                  = conElUltimo unChico
  | otherwise                    = quienConquistarA unaChica lista

conElUltimo:: Chico->Nombre
conElUltimo unChico = nombre unChico

{-
infracctoresDeDaRules:: [Chico]->[Nombre]
infracctoresDeDaRules lista = map nombre (filter tieneDeseoProhibido lista)

tieneDeseoProhibido :: Chico->Bool
tieneDeseoProhibido unChico = esDeseoProhibido.take 5 .habilidades $ unChico 

--esDeseoProhibido:: [Habilidad]->Bool
--esDeseoProhibido unasHabilidades = any prohibido unasHabilidades

--prohibido:: Habilidad -> 
--prohibido = "matar" || "enamorarse" || "dominar el mundo"

--nom::Chico ->Nombre
--nom unChico = nombre unChico 

-}
