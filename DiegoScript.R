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
setwd('D:/UVG/CODING/Semestre7/MineriaDeDatos/HDT/HDT2')
#setwd('/media/sf_D_DRIVE/UVG/CODING/Semestre7/MineriaDeDatos/HDT/HDT2')
getwd()
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

# Determinacion de la cantidad de grupos

## Metodo de codo
### Tipo 1
wss=0
for (i in 1:20) 
  wss[i] <- sum(kmeans(movies[,columns], centers=i)$withinss)

plot(1:20, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

### Tipo 2
fviz_nbclust(movies[columns], kmeans, method = "wss") +
  labs(subtitle = "Metodo de Codo")

## Metodo de la silueta
fviz_nbclust(movies[columns], kmeans, method = "silhouette") +
  labs(subtitle = "Metodo de la silueta")

## Metodo de la brecha
fviz_nbclust(movies[columns], kmeans,
             nstart = 25, method = "gap_stat", nboot = 50, verbose = F)+
  labs(subtitle = "Metodo de la brecha")
## Resumen de diversas estadísticas
nb <- NbClust(movies[columns], distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")