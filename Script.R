setwd('/media/sf_D_DRIVE/UVG/CODING/Semestre7/MineriaDeDatos/DataMining')
# Aniadir librerias
library(tidyr)
library(DataExplorer)
library(nortest)
library(dplyr)
library(ggplot2)
library(reshape2)
library(nortest)
library(dplyr)
library(ggplot2)
library(lubridate)

movies<-read.csv("./movies.csv")
View(movies)
# 
View(movies$id)
View(movies$popularity)
View(movies$budget)
View(movies$revenue)
View(movies$originalTitle)
View(movies$originalLanguage)
View(movies$title)
View(movies$homePage)
View(movies$video)
View(movies$director)
View(movies$runtime)
View(movies$genres)
View(movies$genresAmount)
View(movies$productionCompany)
View(movies$productionCoAmount)
View(movies$productionCompanyCountry)
View(movies$productionCountry)
View(movies$productionCountriesAmount)
View(movies$releaseDate)
View(movies$voteCount)
View(movies$voteAvg)
View(movies$actors)
View(movies$actorsPopularity)
View(movies$actorsCharacter)
View(movies$actorsAmount)
View(movies$castWomenAmount)
View(movies$castMenAmount)

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

#4.6. ¿Cuál es el género principal de las 20 películas más recientes? ¿Cuál es el género principal que predomina en el conjunto de datos? Represéntelo usando un gráfico


#formato de fecha
movies$releaseDate <- ymd(movies$releaseDate)  

#20 películas más recientes
recent_movies <- movies %>% 
  arrange(desc(releaseDate)) %>% 
  slice(1:20)

#Género principal de cada película
recent_movies$main_genre <- sapply(strsplit(as.character(recent_movies$genres), "\\|"), `[`, 1)

# Calcular la frecuencia de cada género principal entre las 20 películas más recientes
genre_frequency <- as.data.frame(table(recent_movies$main_genre))
names(genre_frequency) <- c("main_genre", "Freq")
genre_frequency <- genre_frequency %>% 
  arrange(desc(Freq))

#Grafica
ggplot(genre_frequency, aes(x=reorder(main_genre, Freq), y=Freq)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title="Frecuencia de Género Principal en las 20 Películas Más Recientes",
       x="Género Principal", y="Frecuencia") +
  theme_minimal()



#4.7. ¿Las películas de qué género principal obtuvieron mayores ganancias?


# Calcular las ganancias para cada película
movies$profit <- movies$revenue - movies$budget

# Extraer el género principal de cada película
movies$main_genre <- sapply(strsplit(as.character(movies$genres), "\\|"), `[`, 1)

# Agrupar por género principal y sumar las ganancias
genre_profits <- movies %>%
  group_by(main_genre) %>%
  summarise(total_profit = sum(profit, na.rm = TRUE)) %>%
  arrange(desc(total_profit))

# Identificar el género con mayores ganancias
top_genre <- head(genre_profits, 1)
View(top_genre)



#4.8. ¿La cantidad de actores influye en los ingresos de las películas?¿se han hecho películas con más actores en los últimos años?

# Filtrar las películas donde la columna 'actors' no es 'FALSE' y no está vacía
movies <- movies %>%
  filter(actors != "FALSE", actors != "")

# Contar la cantidad de actores en la columna 'actors'
movies$actorsCount <- sapply(strsplit(as.character(movies$actors), "\\|"), function(x) length(x))

# Calcular el coeficiente de correlación
correlation <- cor.test(movies$actorsCount, movies$revenue, use = "complete.obs")

# Mostrar el resultado del test de correlación
print(correlation)

# Grafica
ggplot(movies, aes(x = actorsCount, y = revenue)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Relación entre Cantidad de Actores y los Ingresos de las Películas",
       x = "Cantidad de Actores", y = "Ingresos") +
  theme_minimal()


movies$releaseDate <- as.Date(movies$releaseDate, format = "%Y-%m-%d")
movies$releaseYear <- year(movies$releaseDate)

# Crear una nueva columna para la década
movies$decade <- (movies$releaseYear %/% 10) * 10

# Agrupar por década y calcular el promedio de la cantidad de actores
actors_trend_by_decade <- movies %>%
  group_by(decade) %>%
  summarise(average_actors = mean(actorsAmount, na.rm = TRUE)) 

# Grafica
ggplot(actors_trend_by_decade, aes(x = decade, y = average_actors)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(movies$decade), max(movies$decade), by = 10)) +
  labs(title = "Tendencia de Cantidad de Actores por Década",
       x = "Década", y = "Promedio de Cantidad de Actores") +
  theme_minimal()



#4.9. ¿Es posible que la cantidad de hombres y mujeres en el reparto influya en lapopularidad y los ingresos de las películas?

# Eliminar filas con NA y convertirlos a datos numericos
movies <- na.omit(movies)
movies$castMenAmount <- as.numeric(movies$castMenAmount)
movies$castWomenAmount <- as.numeric(movies$castWomenAmount)
movies$popularity <- as.numeric(movies$popularity)
movies$revenue <- as.numeric(movies$revenue)

# Calcular las correlaciones
cor_men_popularity <- cor.test(movies$castMenAmount, movies$popularity, use = "complete.obs")
cor_women_popularity <- cor.test(movies$castWomenAmount, movies$popularity, use = "complete.obs")
cor_men_revenue <- cor.test(movies$castMenAmount, movies$revenue, use = "complete.obs")
cor_women_revenue <- cor.test(movies$castWomenAmount, movies$revenue, use = "complete.obs")

# Imprimir los resultados de las correlaciones
print(cor_men_popularity)
print(cor_women_popularity)
print(cor_men_revenue)
print(cor_women_revenue)

# Graficos

ggplot(movies, aes(x = castMenAmount, y = popularity)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Relación entre Cantidad de Actores Hombres y Popularidad",
       x = "Cantidad de Actores Hombres", y = "Popularidad") +
  theme_minimal()

ggplot(movies, aes(x = castWomenAmount, y = popularity)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre Cantidad de Actrices Mujeres y Popularidad",
       x = "Cantidad de Actrices Mujeres", y = "Popularidad") +
  theme_minimal()

ggplot(movies, aes(x = castMenAmount, y = revenue)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Relación entre Cantidad de Actores Hombres y Ingresos",
       x = "Cantidad de Actores Hombres", y = "Ingresos") +
  theme_minimal()

ggplot(movies, aes(x = castWomenAmount, y = revenue)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre Cantidad de Actrices Mujeres y Ingresos",
       x = "Cantidad de Actrices Mujeres", y = "Ingresos") +
  theme_minimal()  


#4.10. ¿Quiénes son los directores que hicieron las 20 películas mejor calificadas?

# Ordenar las películas por calificación y tomar las 20 primeras
top_20_movies <- movies %>%
  arrange(desc(voteAvg)) %>%
  slice(1:20) %>%
  select(title, director, voteAvg)

# Ver los directores de estas películas
View(top_20_movies)

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

#5.2 Pregunta extra, ¿qué tanto afecta la presencia o no de videos publicitarios y/o promocionales en la popularidad de la pelicula?

movies %>% 
  group_by(video) %>% 
  summarize(average_popularity = mean(popularity, na.rm = TRUE)) %>% 
  ggplot(aes(x = video, y = average_popularity)) +
  geom_bar(stat = "identity") +
  labs(title = "Promedio de Popularidad en Películas con y sin Videos Promocionales", x = "Videos Promocionales", y = "Promedio de Popularidad")



#5.3 Pregunta extra, ¿qué tanto afecta la presencia o no de videos publicitarios y/o promocionales en la popularidad de la pelicula?

movies %>% 
  group_by(video) %>% 
  summarize(average_popularity = mean(popularity, na.rm = TRUE)) %>% 
  ggplot(aes(x = video, y = average_popularity)) +
  geom_bar(stat = "identity") +
  labs(title = "Promedio de Popularidad en Películas con y sin Videos Promocionales", x = "Videos Promocionales", y = "Promedio de Popularidad")



#5.4 Pregunta extra, ¿qué tanto influye la cantidad de generos en los ingresos que genera la pelicula?


movies %>% 
  mutate(num_genres = str_count(genres, pattern = ",") + 1) %>% 
  group_by(num_genres) %>% 
  summarize(average_revenue = mean(revenue, na.rm = TRUE), 
            average_popularity = mean(popularity, na.rm = TRUE)) %>% 
  ggplot(aes(x = num_genres)) +
  geom_line(aes(y = average_revenue, colour = "Ingresos")) +
  geom_line(aes(y = average_popularity, colour = "Popularidad")) +
  labs(title = "Ingresos y Popularidad según la Cantidad de Géneros de la Película", x = "Número de Géneros", y = "Promedio")

#5.5 Pregunta extra, ¿Que géneros o combinaciones de estos son más comunes en las películas?

movies %>%
  group_by(genres) %>%
  summarize(frequency = n()) %>%
  top_n(10, frequency) %>%
  ggplot(aes(x = reorder(genres, frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "green") +
  coord_flip() +
  labs(title = "Top 10 Generos o Combinaciones de Géneros Más Comunes en Películas", 
       x = "Combinaciones de Géneros", 
       y = "Frecuencia") +
  theme_minimal()

#5.6 Pregunta extra, ¿Existe una tendencia en la duracion de las peliculas a lo largo de los años?

movies %>%
  mutate(year = year(ymd(releaseDate))) %>%
  group_by(year) %>%
  summarize(average_runtime = mean(runtime, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = average_runtime)) +
  geom_line(color = "blue") +
  labs(title = "Tendencia de la Duración de las Películas a lo Largo de los Años", 
       x = "Año", 
       y = "Duración Promedio (minutos)") +
  theme_minimal()