###############################################################################
## Esto es >95% código de Gastón Sanchez. El código original y mucho más lo  ##
## pueden encontrar en https://sites.google.com/site/miningtwitter/ Un súper ##
## sitio si apenas estan empezando a minar y visualizar texto con R.         ##
###############################################################################


# Primero hay que cargar las librerías e iniciar sesión en la API de Twitter.
library("twitteR")
library("tm")
library("wordcloud")

API_key <- "qRKjCyzf0awqeIBYC3LnJOLcN"
API_secret <- "0fxOmcyXs6x5AyaoYtBnTPuKOKQLA7cA1JihH5riS6W568NY9S"
Access_token <- "780869649734914048-d26jSEjYm5jFtrsIiM9XIoPyBYOoLqE"
Access_token_secret <- "2yN8WvLalfaq6GhKcVouGMhS7lY7gqIhJO3rSzghdxmuC"

setup_twitter_oauth(API_key, API_secret, Access_token, Access_token_secret)

# Luego, obtener la data (los tweets) que queremos analizar
miBusqueda <- searchTwitter("@MorenoGuillermo", n = 200)

# Eliminar caracteres que no son UTF-8 -- (¿por qué?)
rawTweets <- twListToDF(miBusqueda)$text
#convTweets <- iconv(rawTweets, to = "ascii")
#tweets <- (convTweets[!is.na(convTweets)])

# Crear un 'corpus'
corpus <- Corpus(VectorSource(rawTweets), #(tweets),
                 readerControl = list(language = "es-419"))

# Remover URLs, cuentas, hashtags y emojis
corpus <- tm_map(corpus,content_transformer(function(x) gsub("http\\S","",x,ignore.case=TRUE,perl=TRUE)))
corpus <- tm_map(corpus,content_transformer(function(x) gsub("@\\S*","",x,ignore.case=TRUE,perl=TRUE)))
corpus <- tm_map(corpus,content_transformer(function(x) gsub("@\\S*","",x,ignore.case=TRUE,perl=TRUE)))

# Crear una matriz de términos (tdm) aplicando algunas transformaciones al texto
tdm <- TermDocumentMatrix(corpus,
                          control = list(language = "spanish", removePunctuation = TRUE,
                                         stopwords = c(stopwords("spanish")),
                                         removeNumbers = TRUE, tolower = TRUE))
# Definir nuestra tdm como una matriz para que R la reconozca como tal
m = as.matrix(tdm)
# Obtener la frecuencia de los términos en orden decreciente
word_freqs = sort(rowSums(m), decreasing=TRUE)
# Crear un 'data frame' con los términos y sus frecuencias
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# Plottear con WordCount
wordcloud(dm$word, dm$freq, random.order=TRUE, colors=brewer.pal(8, "RdGy"))

# Guardar la imagen en formato PNG
png("wordCloud.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "RdGy"))
dev.off()