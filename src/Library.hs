module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero
data Mago = Mago {
    nombre::String,
    horrocruxes:: [Horrocrux]
} deriving Show

data Horrocrux = Horrocrux {
    denominacion::String,
    mago::Mago
} deriving Show

diadema = Horrocrux {
    denominacion = "Ravenclow",
    mago = srTenebroso
}

diario = Horrocrux {
    denominacion = "Diario de Tom Riddle",
    mago = srTenebroso
}

harry = Horrocrux {
    denominacion = "Harry Postre",
    mago = srTenebroso
}
srTenebroso = Mago {
    nombre = "Voldemort",
    horrocruxes = [diadema, diario, harry]
}
destru = [diadema, diario, harry]
destruir:: Horrocrux -> Mago
destruir horrocrux = (mago horrocrux){horrocruxes=sinhorrocrux (mago horrocrux) horrocrux}
sinhorrocrux::Mago->Horrocrux->[Horrocrux]
sinhorrocrux mago horro = filter (compararhorro horro) (horrocruxes mago)
compararhorro::Horrocrux->Horrocrux->Bool
compararhorro horro1 horro2 = denominacion horro1 /= denominacion horro2

finalFeliz:: [Horrocrux] -> Bool
finalFeliz horro = null(horrocruxes(foldl (matar) srTenebroso horro))
matar::Mago->Horrocrux->Mago
matar  mago horro = mago{horrocruxes=sinhorrocrux mago horro}

{-finalFeliz [] =  False
finalFeliz (x:xs) | null(horrocruxes (destruir x)) = True
                  | otherwise = finalFeliz xs
-}