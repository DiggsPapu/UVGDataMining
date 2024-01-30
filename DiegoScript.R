## Asignar el working directory
setwd('/media/sf_D_DRIVE/UVG/CODING/Semestre7/MineriaDeDatos/DataMining')
# Aniadir librerias
library(tidyr)
library(DataExplorer)
library(nortest)
library(dplyr)
library(ggplot2)
library(reshape2)


## asignacion del dataset a trabajar
movies<-read.csv("./movies.csv")
View(movies)
# EXPLORACION DE DATOS
## Mostrar la estructura de los datos
str(movies)
## Estructura de la data
plot_str(movies)
introduce(movies)
plot_intro(movies)
plot_missing(movies)
plot_histogram(movies)
plot_correlation(movies)
## Crear un reporte de la data.
create_report(
  movies,
  output_file = "movies_report.html",
  output_dir = getwd(),
  config = configure_report(),
  report_title = "Movies Data Report"
)
# 4.11 ¿Cómo se correlacionan los presupuestos con los ingresos? ¿Los altos presupuestos significan altos ingresos? Haga los gráficos que necesite, histograma, diagrama de dispersión
## Crear el diagrama de dispersion de presupuesto contra ingresos
plot(xlab="Budget",ylab="Revenue",main=paste("Plot of", "Budget vs Revenue"),
     movies$budget, movies$revenue)
### Calcular los coeficientes para la regresion lineal
fit <- lm(movies$revenue ~ movies$budget)
slope <- coef(fit)[2]
intercept <- coef(fit)
### Aniadir la linea de tendencia al grafico
abline(intercept, slope, col = "red")
## Calculo del coeficiente de correlacion para todos los datos
cor(movies$budget, movies$revenue)
## diagrama de dispersion para peliculas con altos presupuestos
plot(xlab="Budget >Q80",ylab="Revenue >Q80",main=paste("Plot of", "Budget vs Revenue in >Q80"),
     movies$budget[movies$budget>quantile(movies$budget, probs = 0.8)], movies$revenue[movies$budget>quantile(movies$budget, probs=0.8)])
### Calcular los coeficientes de corrleacion para los presupuestos altos, ultimo quintil
fit <- lm(movies$revenue[movies$budget>quantile(movies$budget, probs=0.8)] ~ movies$budget[movies$budget>quantile(movies$budget, probs = 0.8)])
slope <- coef(fit)[2]
intercept <- coef(fit)
### Aniadir la linea de tendencia a la grafica
abline(intercept, slope, col = "red")
## Correlacion para el ultimo quintil
cor(movies$budget[movies$budget>quantile(movies$budget, probs = 0.8)], movies$revenue[movies$budget>quantile(movies$budget, probs=0.8)])
# 4.12 ¿Se asocian ciertos meses de lanzamiento con mejores ingresos?
## Crear una tabla con valores de agregacion, mediana, media y desviacion estandar
View(aggregate(movies$revenue, by = list(month = substr(movies$releaseDate, 6, 7)), FUN = function(x) c(
  mean = mean(x), median = median(x), sd = sd(x) )))
# Crear un diagrama de caja y bigotes para cada mes
boxplot(list(
  c(movies$revenue[grepl("-01-",movies$releaseDate)],rep(median(movies$revenue[grepl("-01-",movies$releaseDate)]), 1079-nrow(movies[grepl("-01-",movies$releaseDate),]))),
  c(movies$revenue[grepl("-02-",movies$releaseDate)],rep(median(movies$revenue[grepl("-02-",movies$releaseDate)]), 1079-nrow(movies[grepl("-02-",movies$releaseDate),]))),
  c(movies$revenue[grepl("-03-",movies$releaseDate)],rep(median(movies$revenue[grepl("-03-",movies$releaseDate)]), 1079-nrow(movies[grepl("-03-",movies$releaseDate),]))),
  c(movies$revenue[grepl("-04-",movies$releaseDate)],rep(median(movies$revenue[grepl("-04-",movies$releaseDate)]), 1079-nrow(movies[grepl("-04-",movies$releaseDate),]))),
  c(movies$revenue[grepl("-05-",movies$releaseDate)],rep(median(movies$revenue[grepl("-05-",movies$releaseDate)]), 1079-nrow(movies[grepl("-05-",movies$releaseDate),]))),
  c(movies$revenue[grepl("-06-",movies$releaseDate)],rep(median(movies$revenue[grepl("-06-",movies$releaseDate)]), 1079-nrow(movies[grepl("-06-",movies$releaseDate),]))),
  c(movies$revenue[grepl("-07-",movies$releaseDate)],rep(median(movies$revenue[grepl("-07-",movies$releaseDate)]), 1079-nrow(movies[grepl("-07-",movies$releaseDate),]))),
  c(movies$revenue[grepl("-08-",movies$releaseDate)],rep(median(movies$revenue[grepl("-08-",movies$releaseDate)]), 1079-nrow(movies[grepl("-08-",movies$releaseDate),]))),
  c(movies$revenue[grepl("-09-",movies$releaseDate)],rep(median(movies$revenue[grepl("-09-",movies$releaseDate)]), 1079-nrow(movies[grepl("-09-",movies$releaseDate),]))),
  c(movies$revenue[grepl("-10-",movies$releaseDate)],rep(median(movies$revenue[grepl("-10-",movies$releaseDate)]), 1079-nrow(movies[grepl("-10-",movies$releaseDate),]))),
  c(movies$revenue[grepl("-11-",movies$releaseDate)],rep(median(movies$revenue[grepl("-11-",movies$releaseDate)]), 1079-nrow(movies[grepl("-11-",movies$releaseDate),]))),
  c(movies$revenue[grepl("-12-",movies$releaseDate)],rep(median(movies$revenue[grepl("-12-",movies$releaseDate)]), 1079-nrow(movies[grepl("-12-",movies$releaseDate),])))
), main = "Revenue by Month", xlab = "Month", ylab = "Revenue")
# 4.13 ¿En qué meses se han visto los lanzamientos con mejores ingresos?¿Cuántas películas, en promedio, se han lanzado por mes?
## Meses con los mejores ingresos segun el lanzamiento
View(aggregate(movies$revenue, by = list(month = substr(movies$releaseDate, 6, 7)), FUN = function(x) c(max = max(x) )))
## Promedio de peliculas lanzadas por mes
View(colMeans(
  table(
    format(as.Date(movies$releaseDate), "%m"),format(as.Date(movies$releaseDate), "%Y"))
))
# 4.14 ¿Cómo se correlacionan las calificaciones con el éxito comercial?
plot(movies[,c("voteAvg", "revenue")])
plot(movies[,c("voteAvg", "revenue")])
plot(movies[,c("revenue", "voteAvg")])
plot(movies[,c("revenue", "popularity")])
## coeficientes de correlacion
cor(movies$revenue, movies$popularity)
cor(movies$revenue-movies$budget, movies$voteAvg)
## diagrama de dispersion ganancias vs votos
plot(movies$voteAvg, movies$revenue-movies$budget, main="Movies Profit vs Movies Vote", xlab = "Average Vote", ylab = "Profit")
fit <- lm(movies$revenue-movies$budget ~ movies$voteAvg)
slope <- coef(fit)[2]
intercept <- coef(fit)
abline(intercept, slope, col = "red")
boxplot(movies$budget)
shapiro.test(movies$revenue-movies$budget)
## pruebas de normalidad
lillie.test(movies$voteAvg)
lillie.test(movies$revenue-movies$budget)
## diagrama de dispersion ganancias vs popularidad
plot(movies$popularity, movies$revenue-movies$budget, main="Movies Profit vs Popularity", xlab = "Popularity", ylab = "Profit")
cor(movies$revenue-movies$budget, movies$popularity)
# 4.15 ¿A qué género principal pertenecen las películas más largas?
## Obtener los generos unicos
n_distinct(movies$genres)
## Son 2346 generos dado que hay peliculas que pueden pertenecer a mas de un genero, separados por |
## Separar los generos
moviesSeparatedGenres <- separate_rows(movies, genres, sep = "\\|")
## 20 generos de peliculas
n_distinct(moviesSeparatedGenres$genres)
unique(moviesSeparatedGenres$genres)
## Obtener la mediana segun el genero
moviesSeparatedGenres %>% group_by(genres) %>% summarize(runtime_median = median(runtime, na.rm = TRUE))
## mediana de tiempo que dura una pelicula por genero, generar grafico de barras
ggplot(moviesSeparatedGenres %>% group_by(genres) %>% summarize(runtime_median = median(runtime, na.rm = TRUE)), aes(x = genres, y = runtime_median)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Median Runtime by Genre", x = "Genres", y = "Median Runtime")
## media de tiempo que dura una pelicula por genero
ggplot(moviesSeparatedGenres %>% group_by(genres) %>% summarize(runtime_mean = mean(runtime, na.rm = TRUE)), aes(x = genres, y = runtime_mean)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Mean Runtime by Genre", x = "Genres", y = "Mean Runtime")
## desviacion estandar del tiempo que dura una pelicula por genero
ggplot(moviesSeparatedGenres %>% group_by(genres) %>% summarize(runtime_sd = sd(runtime, na.rm = TRUE)), aes(x = genres, y = runtime_sd)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Standard Deviation Runtime by Genre", x = "Genres", y = "Standard Deviation Runtime")
## Media en especifico del genero de historia
mean(moviesSeparatedGenres$runtime[moviesSeparatedGenres$genres=="History"])
median(moviesSeparatedGenres$runtime[moviesSeparatedGenres$genres=="History"])
sd(moviesSeparatedGenres$runtime[moviesSeparatedGenres$genres=="History"])