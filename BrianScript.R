library(nortest)

setwd("C:/Users/bcarr/OneDrive/Desktop/UVG/7mo semestre/Minería de Datos/Hoja de Trabajo 1")
movies<-read.csv("movies.csv")
View(movies)

#3. Investigue si las variables cuantitativas siguen una distribución normal y haga una
#tabla de frecuencias de las variables cualitativas. Explique todos los resultados.
#Variables cuantitativas
#popularity
#Grafica
popularity<-as.numeric(movies[complete.cases(movies$popularity),"popularity"])
qqnorm(popularity, col="blue")
qqline(popularity, col="red")
#Parametrica
lillie.test(popularity)

#budget
#Grafica
budget<-as.numeric(movies[complete.cases(movies$budget),"budget"])
qqnorm(budget, col="blue")
qqline(budget, col="red")
#Parametrica
lillie.test(budget)

#revenue
#Grafica
revenue<-as.numeric(movies[complete.cases(movies$revenue),"revenue"])
qqnorm(revenue, col="blue")
qqline(revenue, col="red")
#Parametrica
lillie.test(revenue)

#runtime
#Grafica
runtime<-as.numeric(movies[complete.cases(movies$runtime),"runtime"])
qqnorm(runtime, col="blue")
qqline(runtime, col="red")
#Parametrica
lillie.test(runtime)

#genresAmount
#Grafica
genresAmount<-as.numeric(movies[complete.cases(movies$genresAmount),"genresAmount"])
qqnorm(genresAmount, col="blue")
qqline(genresAmount, col="red")
#Parametrica
lillie.test(genresAmount)

#productionCoAmount
#Grafica
productionCoAmount<-as.numeric(movies[complete.cases(movies$productionCoAmount),"productionCoAmount"])
qqnorm(productionCoAmount, col="blue")
qqline(productionCoAmount, col="red")
#Parametrica
lillie.test(productionCoAmount)

#productionCountriesAmount
#Grafica
productionCountriesAmount<-as.numeric(movies[complete.cases(movies$productionCountriesAmount),"productionCountriesAmount"])
qqnorm(productionCountriesAmount, col="blue")
qqline(productionCountriesAmount, col="red")
#Parametrica
lillie.test(productionCountriesAmount)

#voteCount
#Grafica
voteCount<-as.numeric(movies[complete.cases(movies$voteCount),"voteCount"])
qqnorm(voteCount, col="blue")
qqline(voteCount, col="red")
#Parametrica
lillie.test(voteCount)

#voteAvg
#Grafica
voteAvg<-as.numeric(movies[complete.cases(movies$voteAvg),"voteAvg"])
qqnorm(voteAvg, col="blue")
qqline(voteAvg, col="red")
#Parametrica
lillie.test(voteAvg)

#actorsPopularity
#Grafica. Se formo una sola lista con todas las popularidades en todas
#las peliculas
allPopularities <- unlist(strsplit(as.character(movies$actorsPopularity), '\\|'))
allPopularities <- as.numeric(allPopularities)
qqnorm(allPopularities, col="blue")
qqline(allPopularities, col="red")
#Parametrica
lillie.test(allPopularities)

#actorsAmount
#Grafica
actorsAmount<-as.numeric(movies[complete.cases(movies$actorsAmount),"actorsAmount"])
qqnorm(actorsAmount, col="blue")
qqline(actorsAmount, col="red")
#Parametrica
lillie.test(actorsAmount)

#castWomenAmount
#Grafica
castWomenAmount<-as.numeric(movies[complete.cases(movies$castWomenAmount),"castWomenAmount"])
qqnorm(castWomenAmount, col="blue")
qqline(castWomenAmount, col="red")
#Parametrica
lillie.test(castWomenAmount)

#castMenAmount
#Grafica
castMenAmount<-as.numeric(movies[complete.cases(movies$castMenAmount),"castMenAmount"])
qqnorm(castMenAmount, col="blue")
qqline(castMenAmount, col="red")
#Parametrica
lillie.test(castMenAmount)

#Variables cualitativas
#id
View(table(movies$id))
#original_title
View(table(movies$originalTitle))
#originalLanguage
View(table(movies$originalLanguage))
#title
View(table(movies$title))
#homePage
View(table(movies$homePage))
#video
View(table(movies$video))
#director
View(table(movies$director))
#genres
View(table(movies$genres))
#productionCompany
View(table(movies$productionCompany))
#productionCompanyCountry
View(table(movies$productionCompanyCountry))
#productionCountry
View(table(movies$productionCountry))
#releaseDate
View(table(movies$releaseDate))
#actors
View(table(movies$actors))
#actorsCharacter
View(table(movies$actorsCharacter))

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
