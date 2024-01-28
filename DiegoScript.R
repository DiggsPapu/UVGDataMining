setwd('/media/sf_D_DRIVE/UVG/CODING/Semestre7/MineriaDeDatos/DataMining')
library(nortest)
movies<-read.csv("./movies.csv")
View(movies)


# EXPLORACION DE DATOS
str(movies)
# al menos un valor de las columnas es nulo
atLeastOnenull <- movies[!complete.cases(movies),]
nrow(atLeastOnenull)

#id null
nrow(movies[is.na(movies$id),])
# budget null
nrow(movies[is.na(movies$budget),])
# genres null
nrow(movies[is.na(movies$genres),])
# homePage null
nrow(movies[is.na(movies$homePage),])
# productionCompany null
nrow(movies[is.na(movies$productionCompany),])
# productionCompanyCountry null
nrow(movies[is.na(movies$productionCompanyCountry),])
# productionCountry null
nrow(movies[is.na(movies$productionCountry),])
# revenue null
nrow(movies[is.na(movies$revenue),])
# runtime null
nrow(movies[is.na(movies$runtime),])
# video null
nrow(movies[is.na(movies$video),])
# director null
nrow(movies[is.na(movies$director),])
# actors null
nrow(movies[is.na(movies$actors),])
# actorsPopularity null
nrow(movies[is.na(movies$actorsPopularity),])
# actorsCharacter null
nrow(movies[is.na(movies$actorsCharacter),])
# originalTitle null
nrow(movies[is.na(movies$originalTitle),])
# title null
nrow(movies[is.na(movies$title),])
# originalLanguage null
nrow(movies[is.na(movies$originalLanguage),])
# popularity null
nrow(movies[is.na(movies$popularity),])
# releaseDate null
nrow(movies[is.na(movies$releaseDate),])
# voteAvg null
nrow(movies[is.na(movies$voteAvg),])
# voteCount null
nrow(movies[is.na(movies$voteCount),])
# genresAmount null
nrow(movies[is.na(movies$genresAmount),])
# productionCoAmount null
nrow(movies[is.na(movies$productionCoAmount),])
# productionCountriesAmount null
nrow(movies[is.na(movies$productionCountriesAmount),])
# actorsAmount null
nrow(movies[is.na(movies$actorsAmount),])
# castWomenAmount null
nrow(movies[is.na(movies$castWomenAmount),])
# castMenAmount null
nrow(movies[is.na(movies$castMenAmount),])
# hgamos la exploracion de datos con un paquete
install.packages("DataExplorer")
library(DataExplorer)


# estructura de la data
plot_str(movies)
introduce(movies)
plot_intro(movies)
plot_missing(movies)
plot_histogram(movies)
plot_correlation(movies)


#creating Report of Data
create_report(
  movies,
  output_file = "movies_report.html",
  output_dir = getwd(),
  config = configure_report(),
  report_title = "Movies Data Report"
)
budget_withoutNA<-movies[budget_withoutNA<-movies[budget_withoutNA<-movies[complete.cases(movies$budget),]
nrow(budget_withoutNA)
rm (budget_withoutNA)

budget_withoutNA<-movies[complete.cases(movies$budget),]
nrow(budget_withoutNA)

movies.count(id, budget)
peligrosos_completos<-peligrosos[complete.cases(peligrosos$perdidas),]

rm()