##### Unicode fixing #####
fixUnicode <- function (x){
  x<-stri_escape_unicode(x)
  x<-gsub("\\u00c3\\u00a1", "\\u00e1", x, fixed = TRUE) # á
  x<-gsub("\\u00c3\\u00a9", "\\u00e9", x, fixed = TRUE) # é
  x<-gsub("\\u00c3\\u00ad", "\\u00ed", x, fixed = TRUE) # í
  x<-gsub("\\u00c3\\u00b3", "\\u00f3", x, fixed = TRUE) # ó
  x<-gsub("\\u00c3\\u00ba", "\\u00fa", x, fixed = TRUE) # ú
  x<-gsub("\\u00c3\\u00b1", "\\u00f1", x, fixed = TRUE) # ñ
  x<-gsub("\\u00c2\\u00bf", "\\u00bf", x, fixed = TRUE) # ¿
  x<-gsub("\\u00c2\\u00a1", "\\u00a1", x, fixed = TRUE) # ¡
  x<-gsub("\\u00e2\\u20ac\\u0153", "\\u201c", x, fixed = TRUE) # <<
  x<-gsub("\\u00e2\\u20ac\\u009d", "\\u201d", x, fixed = TRUE) # >>
  x<-gsub("\\u00c3\\u00bc", "\\u00fc", x, fixed = TRUE) # ü
  x<-gsub("\\u00e2\\u20ac\\u2122", "'", x, fixed = TRUE) # ü
  x<-stri_unescape_unicode(x)
  return(x)
}