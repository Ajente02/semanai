###############################################################################
## ConeccionesComPol - Genera una gráfica de conexiones entre medios         ##
##                     de comunicación y partidos políticos.                 ##
## @autor: Alejandro Guzmán                                                  ##
## @fuente: https://github.com/Ajente02/semanai/                             ##
###############################################################################


# Primero instalar/cargar las librerías e iniciar sesión en la API de Twitter.
if("twitteR" %in% installed.packages() != T){ install.packages("twitteR") }
if("tm" %in% installed.packages() != T){ install.packages("tm") }
if("wordcloud" %in% installed.packages() != T){ install.packages("wordcloud") }
library("twitteR")
library("tm")
library("wordcloud")

API_key <- "qRKjCyzf0awqeIBYC3LnJOLcN"
API_secret <- "0fxOmcyXs6x5AyaoYtBnTPuKOKQLA7cA1JihH5riS6W568NY9S"
Access_token <- "780869649734914048-d26jSEjYm5jFtrsIiM9XIoPyBYOoLqE"
Access_token_secret <- "2yN8WvLalfaq6GhKcVouGMhS7lY7gqIhJO3rSzghdxmuC"

setup_twitter_oauth(API_key, API_secret, Access_token, Access_token_secret)


# Luego, obtener la data (los tweets) de los grupos que queremos analizar
tweetsPLD <- c(userTimeline("PLDenlinea", n=1000),
               userTimeline("PLDalDia", n=1000),
               userTimeline("JuventudPLD", n=200),
               userTimeline("LeonelFernandez", n=200),
               userTimeline("DaniloMedina", n=200),
               userTimeline("ReinaldoPared", n=200),
               userTimeline("MargaritaCdF", n=200))

tweetsPRM <- c(userTimeline("PRModerno_", n=1000),
               userTimeline("PRM_Oficial", n=1000),
               userTimeline("JuventudPRM", n=200),
               userTimeline("OrlandoJM", n=200),
               userTimeline("LuisAbinader", n=200),
               userTimeline("LlegoPapa", n=200),
               userTimeline("El_PRM", n=200))

tweetsFNP <- c(userTimeline("FNPoficial", n=1000),
               userTimeline("FNProgresista", n=1000),
               userTimeline("JuventudFNP", n=500),
               userTimeline("SenadorVinicio", n=500))

tweetsALPAIS <- c(userTimeline("AlianzaPaisRD", n=1000),
                  userTimeline("MPatriaParaTodo", n=1000),
                  userTimeline("JAlianzaPais", n=250),
                  userTimeline("MorenoGuillermo", n=250),
                  userTimeline("FDespradelR", n=250),
                  userTimeline("GuadalupeValdez", n=250))


