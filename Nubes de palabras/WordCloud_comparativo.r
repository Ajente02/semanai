###############################################################################
## WordCloud Comparativa - Compara términos usados en Twitter por            ##
##                         grupos coyunturales en una nube de palabras.      ##
## @autor: Gastón Sánchez.                                                   ##
## @fuente: https://sites.google.com/site/miningtwitter/                     ##
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


# A continuación, convertir la data en 'data frame' y, de paso,
# eliminar caracteres no compatibles con ASCII
convertirTaDF <- function(x) {
  texto_crudo <- twListToDF(x)$text
  texto_convertido <- iconv(texto_crudo, to="ascii") # puede fallar
  texto_compatible <- texto_convertido[!is.na(texto_convertido)] # puede fallar
  return(texto_compatible)
}

textoPLD <- convertirTaDF(tweetsPLD)
textoPRM <- convertirTaDF(tweetsPRM)
textoFNP <- convertirTaDF(tweetsFNP)
textoALPAIS <- convertirTaDF(tweetsALPAIS)


# Ahora debemos limpiar el texto de datos espurios (hashtags, cuentas, URLs...)
limpiarDF = function(x){
  x <- tolower(x) # Estandarizar el texto en minúsculas
  x <- gsub("#\\w+", "", x) # Eliminar hashtags
  x <- gsub("@\\w+", "", x) # Eliminar menciones a cuentas
  x <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", x) # Elimina RTs
  x <- gsub("[[:punct:]]", "", x) # Eliminar signos de punctuación
  x <- gsub("[[:digit:]]", "", x) # Eliminar números
  x <- gsub("http\\w+", "", x) # Eliminar URLs
  return(x)
}

PLDlimpio <- limpiarDF(textoPLD)
PRMlimpio <- limpiarDF(textoPRM)
FNPlimpio <- limpiarDF(textoFNP)
ALPAISlimpio <- limpiarDF(textoALPAIS)


# El siguiente paso será unir los textos de cada grupo en un solo vector
PLD <- paste(PLDlimpio, collapse=" ")
PRM <- paste(PRMlimpio, collapse=" ")
FNP <- paste(FNPlimpio, collapse=" ")
ALPAIS <- paste(ALPAISlimpio, collapse=" ")

partidos <- c(PLD, PRM, FNP, ALPAIS)

# Seguido, eliminar palabras indeseadas (conjunciones, artículos...) y claves
partidos <- removeWords(partidos,
                        c(stopwords("spanish"), "pld", "prm", "fnp", "alpais"))

# Finalmente, con toda la data limpia y ordenada, pasamos a crear un 'corpus' y
# una matriz de términos (tdm) ordenada según los grupos
corpus <- Corpus(VectorSource(partidos)) # Crear 'corpus'
tdm <- TermDocumentMatrix(corpus) # Crear tdm
tdm <- as.matrix(tdm) # Convertir tdm de 'data frame' a matriz
colnames(tdm) <- c("PLD", "PRM", "FNP", "ALPAÍS")

# Graficamos la nube de palabras y... ¡Listo!
comparison.cloud(tdm, random.order=FALSE, scale=c(2.5,0.1),
                 colors=c("#8B008B", "#2DA4FB", "#00008B", "#009999"),
                 title.size=1.15, max.words=500)

# (OPCIONAL) Guardar la imagen en formato PNG:
png("WordCloud_comparativo.png", width=3600, height=2400, units="px", res=600)
comparison.cloud(tdm, random.order=FALSE, scale=c(2.5,0.1),
                 colors=c("#8B008B", "#2DA4FB", "#00008B", "#009999"),
                 title.size=1.15, max.words=500)
dev.off()