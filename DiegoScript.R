setwd('D:/UVG/CODING/Semestre7/MineriaDeDatos/HDT/HDT2')
movies<-read.csv("./movies.csv")
# Preprocesamiento
View(movies)
# casteo de women y men amount
movies$castWomenAmount<-as.integer(movies$castWomenAmount)
movies$castMenAmount<-as.integer(movies$castMenAmount)
movies<-movies[complete.cases(movies),]

summary(movies)
columns<-c("popularity","budget","revenue","runtime","genresAmount","productionCoAmount","voteCount", "voteAvg", "actorsAmount", "castMenAmount", "castWomenAmount")
# Normalizacion
movies[, columns]<-scale(movies[,columns])