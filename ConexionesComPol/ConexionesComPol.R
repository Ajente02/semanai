###################################################################################################
## ConexionesComPol - Genera una gráfica de conexiones entre medios                              ##
##                     de comunicación y partidos políticos.                                     ##
## Tiempo aproximado de carga: 3 horas 40 minutos.                                               ##
## @autor: Alejandro Guzmán                                                                      ##
## @fuente: https://github.com/Ajente02/semanai/blob/master/ConexionesComPol/ConexionesComPol.R  ##
###################################################################################################

## Limitaciones a considerar:
## - La API de Twitter limita las solicitudes a 180 llamados cada 15 minutos.
##   Por lo tanto, se irá bloqueando por intérvalos de tiempo, haciendo el proceso
##   de recolección de datos bastante lento.
## - La API de Twitter solo guarda los tweets de los últimos 6-9 días.
##   Por lo tanto, la búsqueda se limitará a una muestra muy reducida de datos.



# FASE 1: INSTALACIÓN
# Instalar/cargar las librerías.
if("twitteR" %in% installed.packages() != TRUE) { install.packages("twitteR") }
if("network" %in% installed.packages() != TRUE) { install.packages("network") }
if("igraph" %in% installed.packages() != TRUE) { install.packages("igraph") }
if("sna" %in% installed.packages() != TRUE) { install.packages("sna") }
if("GGally" %in% installed.packages() != TRUE) { install.packages("GGally") }
if("ggplot2" %in% installed.packages() != TRUE) { install.packages("ggplot2") }
library("twitteR")
library("network")
library("igraph")
library("sna")
library("GGally")
library("ggplot2")

# Iniciar sesión en la API de Twitter.
API_key <- "qRKjCyzf0awqeIBYC3LnJOLcN"
API_secret <- "0fxOmcyXs6x5AyaoYtBnTPuKOKQLA7cA1JihH5riS6W568NY9S"
Access_token <- "780869649734914048-d26jSEjYm5jFtrsIiM9XIoPyBYOoLqE"
Access_token_secret <- "2yN8WvLalfaq6GhKcVouGMhS7lY7gqIhJO3rSzghdxmuC"
setup_twitter_oauth(API_key, API_secret, Access_token, Access_token_secret)



# FASE 2: OBTENCIÓN DE DATOS
# Crear una función para obtener el número de @mentions de una cuenta:
menciones <- function(x, usuario = x, color = "Medio") {
  busqueda <- function(x, medio) {
    termino <- sub("medio", medio, "x medio", fixed=TRUE)
    if(gsub("from:", "@", termino, fixed=T) == sub("@", "x @", x, fixed=T)) {
      return(0) } else { 
        termino <- sub("x", x, termino, fixed=T)
        num_menciones <- try(nrow(twListToDF(searchTwitter(termino, n=75))))
        ifelse(class(num_menciones) != "integer",
               return(0),
               num_menciones)
      }
  }
  c(Acento = busqueda(x, "AcentoDiario"),
    El_Caribe = busqueda(x, "ElCaribeRD"),
    El_Día = busqueda(x, "ElDia_do"),
    El_Nacional = busqueda(x, "ElNacional_RD"),
    El_Nuevo_Diario = busqueda(x, "ElNuevoDiarioRD"),
    Diario_Libre = busqueda(x, "Diario_Libre"),
    Hoy = busqueda(x, "PeriodicoHoy"),
    Listín_Diario = busqueda(x, "ListinDiario"),
    Z101 = busqueda(x, "Z101Digital"),
    Zol = busqueda(x, "ZolFM1065"),
    CDN = busqueda(x, "CDN37"),
    Teleantillas = busqueda(x, "InformativosTA"),
    Grupo_SIN = busqueda(x, "SIN24Horas"),
    Telemicro = busqueda(x, "NTelemicro5"),
    Telesistema = busqueda(x, "TelenoticiasRD"),
    PLD = busqueda(x, "from:PLDenlinea OR from:PLDalDia OR from:BDiputadosPLD OR from:JuventudPLD"),
    PRM = busqueda(x, "from:PRModerno_ OR from:PRM_Oficial OR from:El_PRM OR from:JuventudPRM"),
    PRSC = busqueda(x, "from:PartidoPRSC OR from:PrensaPRSC OR from:DiputadosPRSCPRSC OR from:elPRSC OR from:JuventudPRSC"),
    PRD = busqueda(x, "from:MiPRD OR from:MiJRD OR from:JMiguelistaPRD"),
    ALPAÍS = busqueda(x, "from:AlianzaPaisRD OR from:MPatriaParaTodo OR from:JAlianzaPais"),
    APD = busqueda(x, "from:APD_web OR from:ODemocratica OR from:DemocraciaND"),
    FNP = busqueda(x, "from:FNPoficial OR from:FNProgresista OR from:PoloSoberanoRD OR JuventudFNP"),
    PQDC = busqueda(x, "from:PQDCenlinea"),
    Leonel_Fernández = busqueda(x, "from:LeonelFernandez"),
    Danilo_Medina = busqueda(x, "from:DaniloMedina"),
    Margarita_Cedeño_de_Fernández = busqueda(x, "from:MargaritaCdF"),
    Reinaldo_Pared_Pérez = busqueda(x, "from:ReinaldoPared"),
    Lucía_Medina = busqueda(x, "from:YomairaMedinaS"),
    Abel_Martínez = busqueda(x, "from:AbelMartinezD"),
    Cristina_Lizardo = busqueda(x, "from:CristinaLizardo"),
    Gustavo_Montalvo = busqueda(x, "from:GMontalvoFranco"),
    Hipólito_Mejía = busqueda(x, "from:LlegoPapa"),
    Luis_Abinader = busqueda(x, "from:LuisAbinader"),
    Orlando_Jorge_Mera = busqueda(x, "from:OrlandoJM"),
    Jesús_Chu_Vásquez = busqueda(x, "from:ChuVasquez"),
    Geanilda_Vásquez = busqueda(x, "from:GeanildaOficial OR from:GeanildaVasquez from:GeanildaV"),
    Alfredo_Pacheco = busqueda(x, "from:PachecoAlfredoo"),
    David_Collado = busqueda(x, "from:DavidColladoM"),
    Milagros_Ortiz_Bosch = busqueda(x, "from:OrtizBosch"),
    Quique_Antún_Batlle = busqueda(x, "from:QuiqueAntun"),
    Ramón_Rogelio_Genao = busqueda(x, "from:RogelioGenao"),
    Willis_Rogelio_Genao = busqueda(x, "from:WRGenao"),
    Ito_Bisonó = busqueda(x, "from:ItoBisono"),
    Miguel_Vargas_Maldonado = busqueda(x, "from:MiguelVargasM"),
    Fello_Suberví = busqueda(x, "from:FelloSuberviPRD"),
    Víctor_Gómez_Casanova = busqueda(x, "from:VGomezCasanova"),
    Guido_Gómez_Mazara = busqueda(x, "from:GGomezMazara"),
    Guillermo_Moreno = busqueda(x, "from:MorenoGuillermo"),
    Fidelio_Despradel = busqueda(x, "from:FDespradelR"),
    Guadalupe_Valdez = busqueda(x, "from:GuadalupeValdez"),
    Aura_Celeste_Fernández = busqueda(x, "from:AuraCelesteFR"),
    Minou_Tavárez_Mirabal = busqueda(x, "from:MinouTavarezM OR from:MinouPresidenta"),
    Max_Puig = busqueda(x, "from:Max_Puig"),
    Carlos_de_la_Peña = busqueda(x, "from:alazurda"),
    Vinicito_Castillo_Semán = busqueda(x, "from:SenadorVinicio"),
    Pelegrín_Castillo_Semán = busqueda(x, "from:PelgrinC"),
    Elías_Wessin_Chávez = busqueda(x, "from:WessinChavez OR from:ewessin16"),
    tamaño = as.integer(5*log(getUser(usuario)$followersCount,base=25)/7.5),
    color = color)
}

# Obtener el número de @mentions de las cuentas que queremos analizar:
  # Medios de comunicación - 15
    #Prensa - 8
Acento <- menciones("@AcentoDiario")
El_Caribe <- menciones("@ElCaribeRD")
El_Día <- menciones("@ElDia_do")
El_Nacional <- menciones("@ElNacional_RD")
El_Nuevo_Diario <- menciones("@ElNuevoDiarioRD")
Diario_Libre <- menciones("@Diario_Libre")
Hoy <- menciones("@PeriodicoHoy")
Listín_Diario <- menciones("@ListinDiario")
    #Radio - 2
Z101 <- menciones("@Z101Digital")
Zol <- menciones("@ZolFM1065")
    #Televisión - 5
CDN <- menciones("@CDN37")
Teleantillas <- menciones("@InformativosTA")
Grupo_SIN <- menciones("@SIN24Horas")
Telemicro <- menciones("@NTelemicro5")
Telesistema <- menciones("@TelenoticiasRD")
  # Partidos políticos - 8
PLD <- menciones("PLD OR @PLDenlinea OR @PLDalDia OR @BDiputadosPLD OR @JuventudPLD", "PLDenlinea", "PLD")
PRM <- menciones("PRM OR @PRModerno_ OR @PRM_Oficial OR @El_PRM OR @JuventudPRM", "El_PRM", "PRM")
PRSC <- menciones("PRSC OR @PartidoPRSC OR @PrensaPRSC OR @DiputadosPRSCPRSC OR @elPRSC OR @JuventudPRSC", "PartidoPRSC", "PRSC")
PRD <- menciones("PRD or @MiPRD OR @MiJRD OR @JMiguelistaPRD", "MiPRD", "PRD")
ALPAÍS <- menciones("ALPAIS OR Alianza Pais OR @AlianzaPaisRD OR @MPatriaParaTodo OR @JAlianzaPais", "AlianzaPaisRD", "ALPAÍS")
APD <- menciones("APD OR OD OR Opcion Democratica OR @APD_web OR @ODemocratica OR @DemocraciaND", "ODemocratica", "APD-OD")
FNP <- menciones("FNP OR @FNPoficial OR @FNProgresista OR @PoloSoberanoRD OR JuventudFNP", "FNPoficial", "FNP")
PQDC <- menciones("PQDC OR @PQDCenlinea", "PQDCenlinea", "PQDC")
  # Políticos - 34
    #PLD - 8
Leonel_Fernández <- menciones("Leonel OR Leonel Fernandez OR @LeonelFernandez", "LeonelFernandez", "PLD")
Danilo_Medina <- menciones("Danilo OR Danilo Medina OR @DaniloMedina", "DaniloMedina", "PLD")
Margarita_Cedeño_de_Fernández <- menciones("Margarita OR Cedeño OR @MargaritaCdF", "MargaritaCdF", "PLD")
Reinaldo_Pared_Pérez <- menciones("Reinaldo OR Pared Perez OR @ReinaldoPared", "ReinaldoPared", "PLD")
Lucía_Medina <- menciones("Lucia Medina OR @YomairaMedinaS", "YomairaMedinaS", "PLD")
Abel_Martínez <- menciones("Abel OR Abel Martinez OR @AbelMartinezD", "AbelMartinezD", "PLD")
Cristina_Lizardo <- menciones("Cristina Lizardo OR @CristinaLizardo", "CristinaLizardo", "PLD")
Gustavo_Montalvo <- menciones("Montalvo OR Gustavo Montalvo OR @GMontalvoFranco", "GMontalvoFranco", "PLD")
    #PRM - 8
Hipólito_Mejía <- menciones("Hipolito OR Mejia OR Hipolito Mejia OR @LlegoPapa", "LlegoPapa", "PRM")
Luis_Abinader <- menciones("Abinader OR Luis Abinader OR @LuisAbinader", "LuisAbinader", "PRM")
Orlando_Jorge_Mera <- menciones("Jorge Mera OR Orlando Jorge Mera OR @OrlandoJM", "OrlandoJM", "PRM")
Jesús_Chu_Vásquez <- menciones("Jesus Vasquez OR Chu Vasquez OR @ChuVasquez", "ChuVasquez", "PRM")
Geanilda_Vásquez <- menciones("Geanilda OR @GeanildaOficial OR @GeanildaVasquez @GeanildaV", "GeanildaOficial", "PRM")
Alfredo_Pacheco <- menciones("Pacheco OR Alfredo Pacheco OR @PachecoAlfredoo", "PachecoAlfredoo", "PRM")
David_Collado <- menciones("Collado OR David Collado OR @DavidColladoM", "DavidColladoM", "PRM")
Milagros_Ortiz_Bosch <- menciones("Milagros OR Milagros Ortiz OR Ortiz Bosch OR @OrtizBosch", "OrtizBosch", "PRM")
    #PRSC - 4
Quique_Antún_Batlle <- menciones("Quique Antún OR @QuiqueAntun", "QuiqueAntun", "PRSC")
Ramón_Rogelio_Genao <- menciones("Ramón Rogelio OR Rogelio Genao OR @RogelioGenao", "RogelioGenao", "PRSC")
Willis_Rogelio_Genao <- menciones("Willis Genao OR @WRGenao", "WRGenao", "PRSC")
Ito_Bisonó <- menciones("Bisonó OR Ito Bisonó OR @ItoBisono", "ItoBisono", "PRSC")
    #PRD - 4
Miguel_Vargas_Maldonado <- menciones("Miguel Vargas OR Vargas Maldonado OR @MiguelVargasM", "MiguelVargasM", "PRD")
Fello_Suberví <- menciones("Fello OR Fello Suberví OR @FelloSuberviPRD", "FelloSuberviPRD", "PRD")
Víctor_Gómez_Casanova <- menciones("Víctor Gómez OR Gómez Casanova OR @VGomezCasanova", "VGomezCasanova", "PRD")
Guido_Gómez_Mazara <- menciones("Gómez Mazara OR Guido Gómez OR @GGomezMazara", "GGomezMazara", "PRD")
    #ALPAÍS - 4
Guillermo_Moreno <- menciones("Guillermo OR Guillermo Moreno OR @MorenoGuillermo", "MorenoGuillermo", "ALPAÍS")
Fidelio_Despradel <- menciones("Fidelio OR Despradel OR @FDespradelR", "FDespradelR", "ALPAÍS")
Guadalupe_Valdez <- menciones("Guadalupe OR Valdez OR @GuadalupeValdez", "GuadalupeValdez", "ALPAÍS")
Aura_Celeste_Fernández <- menciones("Aura Celeste OR @AuraCelesteFR", "AuraCelesteFR", "ALPAÍS")
    #APD - 3
Minou_Tavárez_Mirabal <- menciones("Minou OR Tavárez Mirabal OR Mirabal @MinouTavarezM OR @MinouPresidenta", "MinouTavarezM", "APD")
Max_Puig <- menciones("Max Puig OR @Max_Puig", "Max_Puig", "APD")
Carlos_de_la_Peña <- menciones("Carlos de Peña OR de la Peña OR @alazurda", "alazurda", "APD")
    #FNP - 2
Vinicito_Castillo_Semán <- menciones("Vinicio OR Vinicito OR Castillo Semán OR @SenadorVinicio", "SenadorVinicio", "FNP")
Pelegrín_Castillo_Semán <- menciones("Pelegrín OR Castillo Semán OR @PelgrinC", "PelegrinC", "FNP")
    #PQDC - 1
Elías_Wessin_Chávez <- menciones("Wessin OR Elias Wessin OR @WessinChavez OR @ewessin16", "PQDCenlinea", "PQDC")

# OPCIONAL: Guardar los datos obtenidos para futuras referencias o modificaciones.
save.image(".RData")



# FASE 3: ORGANIZACIÓN Y ANÁLISIS DE DATOS
# Crear un 'data frame' y una matriz con los datos obtenidos.
datos <- data.frame(row.names = c("Acento", "El Caribe", "El Día", "El Nacional", "El Nuevo Diario",
                    "Diario Libre", "Hoy", "Listín Diario", "Z 101.5FM", "Zol 106.5FM",
                    "CDN 37", "Teleantillas 2", "Grupo SIN 7", "Telemicro 5", "Telesistema 11",
                    "PLD", "PRM", "PRSC", "PRD", "ALPAÍS", "APD", "FNP", "PQDC", "Leonel Fernández",
                    "Danilo Medina", "Margarita Cedeno", "Reinaldo Pared Pérez", "Lucía Medina",
                    "Abel Martínez", "Cristina Lizardo", "Gustavo Montalvo", "Hipólito Mejía",
                    "Luis Abinader", "Orlando Jorge Mera", "Jesús 'Chu' Vásquez", "Geanilda Vásquez",
                    "Alfredo Pacheco", "David Collado", "Milagros Ortiz Bosch", "Quique Antún Batlle",
                    "Ramón Rogelio Genao", "Willis Rogelio Genao", "Ito Bisonó", "Miguel Vargas Maldonado",
                    "Fello Suberví", "Víctor Gómez Casanova", "Guido Gómez Mazara", "Guillermo Moreno",
                    "Fidelio Despradel", "Guadalupe Valdez", "Aura Celeste Fernández", "Minou Tavárez Mirabal",
                    "Max Puig","Carlos de la Peña", "Elías Wessin Chávez", "Vinicito Castillo Semán", "Pelegrin Castillo Semán" ),
                    "Acento" = Acento[1:57],  "El Caribe" = El_Caribe[1:57],  "El Día" = El_Día[1:57], 
                    "El Nacional" = El_Nacional[1:57],  "El Nuevo Diario" = El_Nuevo_Diario[1:57],  "Diario Libre" = Diario_Libre[1:57], 
                    "Hoy" = Hoy[1:57],  "Listín Diario" = Listín_Diario[1:57],  "Z 101.5FM" = Z101[1:57], 
                    "Zol 106.5FM" = Zol[1:57],  "CDN 37" = CDN[1:57],  "Teleantillas 2" = Teleantillas[1:57], 
                    "Grupo SIN 7" = Grupo_SIN[1:57],  "Telemicro 5" = Telemicro[1:57],  "Telesistema 11" = Telesistema[1:57], 
                    "PLD" = PLD[1:57], "PRM" = PRM[1:57], "PRSC" = PRSC[1:57], "PRD" = PRD[1:57],
                    "ALPAÍS" = ALPAÍS[1:57], "APD" = APD[1:57], "FNP" = FNP[1:57], "PQDC" = PQDC[1:57],
                    "Leonel Fernández" = Leonel_Fernández[1:57], "Danilo Medina" = Danilo_Medina[1:57],
                    "Margarita Cedeño de Fernández" = Margarita_Cedeño_de_Fernández[1:57],
                    "Reinaldo Pared Pérez" = Reinaldo_Pared_Pérez[1:57], "Lucía Medina" = Lucía_Medina[1:57],
                    "Abel Martínez" = Abel_Martínez[1:57], "Cristina Lizardo" = Cristina_Lizardo[1:57],
                    "Gustavo Montalvo" = Gustavo_Montalvo[1:57], "Hipólito_Mejía" = Hipólito_Mejía[1:57],
                    "Luis_Abinader" = Luis_Abinader[1:57], "Orlando Jorge Mera" = Orlando_Jorge_Mera[1:57],
                    "Jesús_'Chu'_Vásquez" = Jesús_Chu_Vásquez[1:57], "Geanilda Vásquez" = Geanilda_Vásquez[1:57],
                    "Alfredo_Pacheco" = Alfredo_Pacheco[1:57], "David_Collado" = David_Collado[1:57],
                    "Milagros Ortiz Bosch" = Milagros_Ortiz_Bosch[1:57], "Quique Antún Batlle" = Quique_Antún_Battle[1:57],
                    "Ramón Rogelio Genao" = Ramón_Rogelio_Genao[1:57], "Willis Rogelio Genao" = Willis_Rogelio_Genao[1:57],
                    "Ito Bisonó" = Ito_Bisonó[1:57], "Miguel Vargas Maldonado" = Miguel_Vargas_Maldonado[1:57],
                    "Fello Suberví" = Fello_Suberví[1:57], "Víctor Gómez Casanova" = Víctor_Gómez_Casanova[1:57],
                    "Guido Gómez Mazara" = Guido_Gómez_Mazara[1:57], "Guillermo Moreno" = Guillermo_Moreno[1:57],
                    "Fidelio Despradel" = Fidelio_Despradel[1:57], "Guadalupe Valdez" = Guadalupe_Valdez[1:57],
                    "Aura Celeste Fernández" = Aura_Celeste_Fernández[1:57], "Minou Tavárez Mirabal" = Minou_Tavárez_Mirabal[1:57],
                    "Max Puig" = Max_Puig[1:57], "Carlos de la Peña" = Carlos_de_la_Peña[1:57],
                    "Elías Wessin Chávez" = Elías_Wessin_Chávez[1:57], "Vinicito Castillo Semán" = Vinicito_Castillo_Semán[1:57],
                    "Pelegrín Castillo Semán" = Pelegrín_Castillo_Semán[1:57])
d <- as.matrix.data.frame(data, rownames.force = TRUE)



# Crear también un 'data frame' y una matriz para todos los valores de tamaño y color.
valoresTyC <- data.frame(row.names = c("tamaño", "color"),
                         "Acento" = Acento[58:59],  "El Caribe" = El_Caribe[58:59],  "El Día" = El_Día[58:59], 
                         "El Nacional" = El_Nacional[58:59],  "El Nuevo Diario" = El_Nuevo_Diario[58:59],  "Diario Libre" = Diario_Libre[58:59], 
                         "Hoy" = Hoy[58:59],  "Listín Diario" = Listín_Diario[58:59],  "Z 101.5FM" = Z101[58:59], 
                         "Zol 106.5FM" = Zol[58:59],  "CDN 37" = CDN[58:59],  "Teleantillas 2" = Teleantillas[58:59], 
                         "Grupo SIN 7" = Grupo_SIN[58:59],  "Telemicro 5" = Telemicro[58:59],  "Telesistema 11" = Telesistema[58:59], 
                         "PLD" = PLD[58:59], "PRM" = PRM[58:59], "PRSC" = PRSC[58:59], "PRD" = PRD[58:59],
                         "ALPAÍS" = ALPAÍS[58:59], "APD" = APD[58:59], "FNP" = FNP[58:59], "PQDC" = PQDC[58:59],
                         "Leonel Fernández" = Leonel_Fernández[58:59], "Danilo Medina" = Danilo_Medina[58:59],
                         "Margarita Cedeño de Fernández" = Margarita_Cedeño_de_Fernández[58:59],
                         "Reinaldo Pared Pérez" = Reinaldo_Pared_Pérez[58:59], "Lucía Medina" = Lucía_Medina[58:59],
                         "Abel Martínez" = Abel_Martínez[58:59], "Cristina Lizardo" = Cristina_Lizardo[58:59],
                         "Gustavo Montalvo" = Gustavo_Montalvo[58:59], "Hipólito_Mejía" = Hipólito_Mejía[58:59],
                         "Luis_Abinader" = Luis_Abinader[58:59], "Orlando Jorge Mera" = Orlando_Jorge_Mera[58:59],
                         "Jesús_'Chu'_Vásquez" = Jesús_Chu_Vásquez[58:59], "Geanilda Vásquez" = Geanilda_Vásquez[58:59],
                         "Alfredo_Pacheco" = Alfredo_Pacheco[58:59], "David_Collado" = David_Collado[58:59],
                         "Milagros Ortiz Bosch" = Milagros_Ortiz_Bosch[58:59], "Quique Antún Batlle" = Quique_Antún_Battle[58:59],
                         "Ramón Rogelio Genao" = Ramón_Rogelio_Genao[58:59], "Willis Rogelio Genao" = Willis_Rogelio_Genao[58:59],
                         "Ito Bisonó" = Ito_Bisonó[58:59], "Miguel Vargas Maldonado" = Miguel_Vargas_Maldonado[58:59],
                         "Fello Suberví" = Fello_Suberví[58:59], "Víctor Gómez Casanova" = Víctor_Gómez_Casanova[58:59],
                         "Guido Gómez Mazara" = Guido_Gómez_Mazara[58:59], "Guillermo Moreno" = Guillermo_Moreno[58:59],
                         "Fidelio Despradel" = Fidelio_Despradel[58:59], "Guadalupe Valdez" = Guadalupe_Valdez[58:59],
                         "Aura Celeste Fernández" = Aura_Celeste_Fernández[58:59], "Minou Tavárez Mirabal" = Minou_Tavárez_Mirabal[58:59],
                         "Max Puig" = Max_Puig[58:59], "Carlos de la Peña" = Carlos_de_la_Peña[58:59],
                         "Elías Wessin Chávez" = Elías_Wessin_Chávez[58:59], "Vinicito Castillo Semán" = Vinicito_Castillo_Semán[58:59],
                         "Pelegrín Castillo Semán" = Pelegrín_Castillo_Semán[58:59])
v <- as.matrix.data.frame(valoresTyC, rownames.force = TRUE)

# Crear un objeto 'network' tomando nuestra matriz de datos.
red <- network(datos, matrix.type = "adjacency", ignore.eval = FALSE, names.eval = "longitud")

# Y una paleta de colores basado en los grupos que tenemos.
paleta <- c("Medio" = "gray80", "PLD" = "magenta4", "PRM" = "lightskyblue",
            "PRSC" = "red2", "PRD" = "white", "ALPAÍS" = "lightseagreen",
            "APD" = "sienna4", "FNP" = "navy", "PQDC"= "yellow1")



# FASE 4: GRAFICAR
# Construir una gráfica con ggnet2().
gráfica <- ggnet2(red,
                  mode = "circle", # Me gustan "circle", "kamadakawai" y "target"
                  layout.exp = 0,
                  alpha = 1,
                  shape = 19,
                  size = valoresTyC$tamaño,
                  color = valoresTyC$color,
                  color.palette = paleta,
                  color.legend = "Organización (medios y partidos)",
                  size.legend = "Influencia (del 1 al 10)",
                  label = TRUE,
                  label.size = 2.5,
                  legend.size = 5) + geom_point(alpha = 0.25,
                                                size = 0.95*valoresTyC$tamaño,
                                                shape = 19)

# Graficar.
gráfica

# Generar un archivo .png para guardar la gráfica. ¡Y listo!
png("Conexiones_entre_medios_y_partidos.png", width=2400, height=3400, units="px")
dev.off()