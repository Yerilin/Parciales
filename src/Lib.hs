import Text.Show.Functions ()
import Data.Char

type Nombre = String
type Habilidad = String
type Objeto = Barbaro ->Barbaro


data Barbaro = Barbaro{

    nombre::Nombre,
    fuerza::Int,
    habilidades::[Habilidad],
    objetos:: [Objeto]
}deriving Show

--Punto 1

espadas:: Int->Objeto
espadas unPeso unBarbaro = unBarbaro{fuerza= 2*unPeso + fuerza unBarbaro}

amuletosMisticos :: Habilidad -> Objeto
amuletosMisticos unaHabilidad unBarbaro = unBarbaro{habilidades = unaHabilidad : habilidades unBarbaro}
varitasDefectuosas:: Objeto
varitasDefectuosas unBarbaro = unBarbaro {habilidades = ["hacerMagia"]}

agregarunaHabilidad:: Habilidad -> Objeto
agregarunaHabilidad unaHabilidad unBarbaro = unBarbaro {habilidades = unaHabilidad : habilidades unBarbaro}

ardilla:: Objeto
ardilla unBarbaro = unBarbaro


cuerda :: Objeto -> Objeto -> Objeto
cuerda objeto1 objeto2 unBarbaro = objeto1.objeto2 $ unBarbaro

--Punto 2 

megafono:: Objeto 
megafono unBarbaro = unBarbaro{habilidades =  [(ponerEnMayusculas.concat) $ habilidades unBarbaro ] }

ponerEnMayusculas:: Habilidad ->Habilidad
ponerEnMayusculas unaPalabra =  map toUpper unaPalabra

megafonoBarbarico:: Objeto
megafonoBarbarico unBarbaro = cuerda ardilla megafono unBarbaro 


barbaro1::Barbaro
barbaro1 = Barbaro{nombre = "Barbarin",fuerza = 103 , habilidades=["tejer","lavarRopa","tejer","cocinar"],objetos=[ardilla,amuletosMisticos "bailar",espadas 2]}

roco::Barbaro
roco = Barbaro{nombre = "Roco",fuerza = 123 , habilidades=["nadar","patinar","saltar"],objetos=[cuerda ardilla varitasDefectuosas, espadas 5]}

type Evento = Barbaro -> Bool
type Prueba = Barbaro -> Bool
{-
data Aventura = Aventura{

   eventos :: [Evento]
} deriving Show 
-}
type Aventura = [Evento]

--aventura1::Aventura
--aventura1= Aventura {eventos = [invasionDeSuciosDuendes]}

--Punto 3 
invasionDeSuciosDuendes::Evento
invasionDeSuciosDuendes unBarbaro = tieneHabilidad "Escribir Poesia Atroz".habilidades $ unBarbaro


tieneHabilidad::Habilidad ->[Habilidad]->Bool
tieneHabilidad unaHabilidad lista = any (==unaHabilidad) lista 

cremalleraDelTiempo::Evento 
cremalleraDelTiempo unBarbaro = esFaffyoAstro .nombre $ unBarbaro

esFaffyoAstro::Nombre ->Bool
esFaffyoAstro unNombre = unNombre == "Faffy" || unNombre=="Astro"
--{-
ritualDeFechorias :: Evento
ritualDeFechorias unBarbaro = saqueo unBarbaro || gritoDeGuerra unBarbaro || caligrafia unBarbaro

saqueo::Prueba 
saqueo unBarbaro =( tieneHabilidad "robar". habilidades $ unBarbaro ) && fuerza unBarbaro > 80 

gritoDeGuerra::Prueba
gritoDeGuerra unBarbaro = (poderDeGrito.habilidades $ unBarbaro) > ((*4).cantidadDeObjetos $ unBarbaro)

poderDeGrito:: [Habilidad]->Int
poderDeGrito habilidades = sum .map length $ habilidades

cantidadDeObjetos::Barbaro -> Int 
cantidadDeObjetos unBarbaro = length.objetos $ unBarbaro

caligrafia:: Prueba 
caligrafia unBarbaro= ( cantidadDeVocalesMayoresA 3 . habilidades $ unBarbaro  ) && (comienzaConMayuscula .habilidades $ unBarbaro)

cantidadDeVocalesMayoresA::Int->[Habilidad]->Bool
cantidadDeVocalesMayoresA unNumero habilidadess = (length.filter esVocal.concat  $ habilidadess  )> unNumero

esVocal :: Char -> Bool
esVocal unCaracter = elem unCaracter "aeiouáéíóúAEIOUÁÉÍÓÚ"


comienzaConMayuscula:: [Habilidad]->Bool
comienzaConMayuscula unaLista = esMayuscula.map head $ unaLista 

esMayuscula ::String -> Bool
esMayuscula unaLetra = all mayusculas unaLetra

mayusculas:: Char ->Bool
mayusculas unaLetra = elem unaLetra "ABCDEFGHIJKLÑMOPQRSTUVWXYZÁBCDÉFGHÍJKLNÑMOPQRSTÚVWXYZ"

{-
sobrevivientes:: [Barbaro]-> Aventura -> [Barbaro]
sobrevivientes barbaros aventura =  filter (cumpleTodoslosEventos  aventura )  barbaros 

cumpleTodoslosEventos:: Aventura -> Barbaro ->Bool
cumpleTodoslosEventos unaAventura unBarbaro = cumple (length unAventura) 

-}

--Punto 4

estaRepetido:: Habilidad -> [Habilidad]->Bool
estaRepetido unaHabilidad habilidadess = elem unaHabilidad habilidadess


sinRepetidos::[Habilidad]->[Habilidad]
sinRepetidos [] = []
sinRepetidos (cabeza : cola)
  | (estaRepetido cabeza cola  ) = sinRepetidos cola
  | otherwise                    = cabeza : sinRepetidos cola 
  