library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el número de clusters óptimo
library(factoextra) #Para hacer gráficos bonitos de clustering
library(hopkins) #Para revisar si vale la pena hacer agrupamiento
library(GGally) #Para hacer el conjunto de graficos
library(FeatureImpCluster) #Para revisar la importancia de las variables en los grupos.
library(pheatmap) #Para hacer mapa de calor

setwd('C:/Users/bcarr/OneDrive/Desktop/UVG/7mo semestre/Minería de Datos/Hoja de Trabajo 2')
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

#¿Hacemos agrupamiento?
set.seed(123)
hopkins(movies[,columns])

movies_dist<- dist(movies[,columns])
fviz_dist(movies_dist, show_labels = F)