library(nortest)

setwd("C:/Users/bcarr/OneDrive/Desktop/UVG/7mo semestre/Minería de Datos/Hoja de Trabajo 1")
movies<-read.csv("movies.csv")
View(movies)

#4.1. ¿Cuáles son las 10 películas que contaron con más presupuesto?
top_budget<-head(movies[order(-movies$budget),c("budget","title")],10)
View(top_budget)

#4.2. ¿Cuáles son las 10 películas que más ingresos tuvieron?
top_revenue<-head(movies[order(-movies$revenue),c("revenue","title")],10)
View(top_revenue)

#4.3. ¿Cuál es la película que más votos tuvo?
movie_voteCount<-head(movies[order(-movies$voteCount),c("voteCount","title")],1)
View(movie_voteCount)

#4.4. ¿Cuál es la peor película de acuerdo a los votos de todos los usuarios?
movie_voteAvg<-head(movies[order(movies$voteAvg),c("voteAvg","title")],1)
View(movie_voteAvg)

#4.5. ¿Cuántas películas se hicieron en cada año? ¿En qué año se hicieron más 
#películas? Haga un gráfico de barras
movies$releaseYear<-substr(movies$releaseDate, 1,4)
movies_perYear<-aggregate(movies[,"releaseYear"],list(movies$releaseYear),length)
names(movies_perYear)<-c("releaseYear","amountMovies")
movies_perYear<-movies_perYear[order(-movies_perYear$amountMovies),]
View(movies_perYear)

#Grafico de barras
top_movies_perYear<-head(movies_perYear,10)
barplot(top_movies_perYear$amountMovies,names.arg = top_movies_perYear$releaseYear,
        main = "Cantidad de peliculas por año", col=c("red","blue","yellow","orange","green","cyan","purple","brown","black","skyblue"),
        xlab="releaseYear",ylab = "amountMovies",ylim = c(0,850))
