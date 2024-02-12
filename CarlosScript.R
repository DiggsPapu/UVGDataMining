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
library(ggrepel)
#setwd('D:/UVG/CODING/Semestre7/MineriaDeDatos/HDT/HDT2')
#setwd('/media/sf_D_DRIVE/UVG/CODING/Semestre7/MineriaDeDatos/HDT/HDT2')
setwd('/Users/carlos/Documents/GitHub/UVGDataMining')
getwd()
movies<-read.csv("movies.csv")
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

# Clustering
## KMeans
km<-kmeans(movies[,columns],2,iter.max =100)
km
### Ploteo
plotcluster(movies[,columns],km$cluster) #grafica la ubicaciÃ³n de los clusters
fviz_cluster(km, data = movies[,columns],geom = "point", ellipse.type = "norm")
### Cardinalidad de los grupos
km$size
### Variabilidad intragrupo
km$withinss
### Cardinalidad vs variabilidad intragrupo-Detección de grupos anómalos
m<-data.frame(withinss=km$withinss, size=km$size)
ggplot(m, aes(size,withinss))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="cardinalidad (size)",y="magnitud (whithinss)")+
  geom_text_repel(label=rownames(m))

#Clustering Jerarquico
movies_dist <- dist(movies[, columns])
hc <- hclust(movies_dist, method = "ward.D2")
plot(hc, cex=0.5, axes=FALSE)
rect.hclust(hc, k=2)

fviz_dend(hc, k=2, rect = TRUE, cex = 0.5)

groups <- cutree(hc, k=2)
movies$gruposHC <- groups

#tamanio de los grupos
table(movies$gruposHC)
#medida de variables
by(movies[, columns], movies$gruposHC, colMeans)




