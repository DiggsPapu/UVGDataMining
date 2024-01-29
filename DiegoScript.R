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

budget_withoutNA<-movies[complete.cases(movies$budget),]
nrow(budget_withoutNA)

movies.count(id, budget)
peligrosos_completos<-peligrosos[complete.cases(peligrosos$perdidas),]
#¿Cómo se correlacionan los presupuestos con los ingresos? ¿Los altos presupuestos significan altos ingresos? Haga los gráficos que necesite, histograma, diagrama de dispersión
# Create the scatter plot
plot(xlab="Budget",ylab="Revenue",main=paste("Plot of", "Budget vs Revenue"),
     movies$budget, movies$revenue)
# Calculate the linear regression coefficients
fit <- lm(movies$revenue ~ movies$budget)
slope <- coef(fit)[2]
intercept <- coef(fit)
# Add the tendency line to the plot
abline(intercept, slope, col = "red")
# coeficiente de correlacion
cor(movies$budget, movies$revenue)
max(movies$budget)
mean(movies$budget)
sd(movies$budget)
median(movies$budget, na.rm = TRUE)
# diagrama de dispersion para peliculas con altos presupuestos
cor(movies$budget[movies$budget > median(movies$budget, na.rm = TRUE) + sd(movies$budget)], movies$revenue[movies$budget > median(movies$budget, na.rm = TRUE) + sd(movies$budget)])
cor(movies$budget[movies$budget>quantile(movies$budget, probs = 0.8)], movies$revenue[movies$budget>quantile(movies$budget, probs=0.8)])
plot(movies$budget[movies$budget > median(movies$budget, na.rm = TRUE) + sd(movies$budget)], movies$revenue[movies$budget > median(movies$budget, na.rm = TRUE) + sd(movies$budget)])

# Create the scatter plot
plot(xlab="Budget >Q80",ylab="Revenue >Q80",main=paste("Plot of", "Budget vs Revenue in >Q80"),
     movies$budget[movies$budget>quantile(movies$budget, probs = 0.8)], movies$revenue[movies$budget>quantile(movies$budget, probs=0.8)])
# Calculate the linear regression coefficients
fit <- lm(movies$revenue[movies$budget>quantile(movies$budget, probs=0.8)] ~ movies$budget[movies$budget>quantile(movies$budget, probs = 0.8)])
slope <- coef(fit)[2]
intercept <- coef(fit)
# Add the tendency line to the plot
abline(intercept, slope, col = "red")

boxplot(movies$budget)

boxplot(movies$budget[movies$budget > median(movies$budget, na.rm = TRUE) + sd(movies$budget)])

quantile(movies$budget, probs = 0.8)
# create two boxplots
boxplot(movies$budget[movies$budget > median(movies$budget, na.rm = TRUE) + sd(movies$budget)], movies$budget, names = c("Budget > Median + SD", "All Budgets"))

boxplot(movies$budget[movies$budget > quantile(movies$budget, probs = 0.8)], movies$budget, names = c("Budget last quantile", "All Budgets"))
hist(movies$budget)
hist(movies$budget[movies$budget > median(movies$budget, na.rm = TRUE) + sd(movies$budget)], main=paste("Histogram of", "Budgets in the last quartile"),xlab = "Budgets in the last quartile")

# ¿Se asocian ciertos meses de lanzamiento con mejores ingresos?
movies$releaseDate
install.packages("dplyr")
library(dplyr)
movies$releaseDate[movies$releaseDate %like% "-01-"]
movies$releaseDate[movies$releaseDate%like%"-02-"]
movies$releaseDate[movies$releaseDate%like%"-03-"]
movies$releaseDate[movies$releaseDate%like%"-04-"]
movies$releaseDate[movies$releaseDate%like%"-05-"]
movies$releaseDate[movies$releaseDate%like%"-06-"]
movies$releaseDate[movies$releaseDate%like%"-07-"]
movies$releaseDate[movies$releaseDate%like%"-08-"]
movies$releaseDate[movies$releaseDate%like%"-09-"]
movies$releaseDate[movies$releaseDate%like%"-10-"]
movies$releaseDate[movies$releaseDate%like%"-11-"]
movies$releaseDate[movies$releaseDate%like%"-12-"]

boxplot(
  movies$revenue[movies$releaseDate%like%"-01-"],
  movies$revenue[movies$releaseDate%like%"-02-"],
  movies$revenue[movies$releaseDate%like%"-03-"],
  movies$revenue[movies$releaseDate%like%"-04-"],
  movies$revenue[movies$releaseDate%like%"-05-"],
  movies$revenue[movies$releaseDate%like%"-06-"],
  movies$revenue[movies$releaseDate%like%"-07-"],
  movies$revenue[movies$releaseDate%like%"-08-"],
  movies$revenue[movies$releaseDate%like%"-09-"],
  movies$revenue[movies$releaseDate%like%"-10-"],
  movies$revenue[movies$releaseDate%like%"-11-"],
  movies$revenue[movies$releaseDate%like%"-12-"],
  names = c("Enero", "Febrero","Marzo","Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  )

mean(movies$revenue[movies$releaseDate%like%"-01-"])
revenueMonth<-tibble(
  month=c("Enero", "Febrero","Marzo","Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
  value = c(
      movies$revenue[grepl("-01-",movies$releaseDate)],
      movies$revenue[grepl("-02-",movies$releaseDate)],
      movies$revenue[grepl("-03-",movies$releaseDate)],
      movies$revenue[grepl("-04-",movies$releaseDate)],
      movies$revenue[grepl("-05-",movies$releaseDate)],
      movies$revenue[grepl("-06-",movies$releaseDate)],
      movies$revenue[grepl("-07-",movies$releaseDate)],
      movies$revenue[grepl("-08-",movies$releaseDate)],
      movies$revenue[grepl("-09-",movies$releaseDate)],
      movies$revenue[grepl("-10-",movies$releaseDate)],
      movies$revenue[grepl("-11-",movies$releaseDate)],
      movies$revenue[grepl("-12-",movies$releaseDate)]
  )
)

c(movies$revenue[grepl("-01-",movies$releaseDate)],rep(median(movies$revenue[grepl("-01-",movies$releaseDate)]), 1079-nrow(movies[grepl("-01-",movies$releaseDate),])))
c(movies$revenue[grepl("-02-",movies$releaseDate)],rep(median(movies$revenue[grepl("-02-",movies$releaseDate)]), 1079-nrow(movies[grepl("-02-",movies$releaseDate),])))
c(movies$revenue[grepl("-03-",movies$releaseDate)],rep(median(movies$revenue[grepl("-03-",movies$releaseDate)]), 1079-nrow(movies[grepl("-03-",movies$releaseDate),])))
c(movies$revenue[grepl("-04-",movies$releaseDate)],rep(median(movies$revenue[grepl("-04-",movies$releaseDate)]), 1079-nrow(movies[grepl("-04-",movies$releaseDate),])))
c(movies$revenue[grepl("-05-",movies$releaseDate)],rep(median(movies$revenue[grepl("-05-",movies$releaseDate)]), 1079-nrow(movies[grepl("-05-",movies$releaseDate),])))
c(movies$revenue[grepl("-06-",movies$releaseDate)],rep(median(movies$revenue[grepl("-06-",movies$releaseDate)]), 1079-nrow(movies[grepl("-06-",movies$releaseDate),])))
c(movies$revenue[grepl("-07-",movies$releaseDate)],rep(median(movies$revenue[grepl("-07-",movies$releaseDate)]), 1079-nrow(movies[grepl("-07-",movies$releaseDate),])))
c(movies$revenue[grepl("-08-",movies$releaseDate)],rep(median(movies$revenue[grepl("-08-",movies$releaseDate)]), 1079-nrow(movies[grepl("-08-",movies$releaseDate),])))
c(movies$revenue[grepl("-09-",movies$releaseDate)],rep(median(movies$revenue[grepl("-09-",movies$releaseDate)]), 1079-nrow(movies[grepl("-09-",movies$releaseDate),])))
c(movies$revenue[grepl("-10-",movies$releaseDate)],rep(median(movies$revenue[grepl("-10-",movies$releaseDate)]), 1079-nrow(movies[grepl("-10-",movies$releaseDate),])))
c(movies$revenue[grepl("-11-",movies$releaseDate)],rep(median(movies$revenue[grepl("-11-",movies$releaseDate)]), 1079-nrow(movies[grepl("-11-",movies$releaseDate),])))
c(movies$revenue[grepl("-12-",movies$releaseDate)],rep(median(movies$revenue[grepl("-12-",movies$releaseDate)]), 1079-nrow(movies[grepl("-12-",movies$releaseDate),])))
medianMonth<-tibble(
  month=c("Enero", "Febrero","Marzo","Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
  value = c(
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
  )
)

# Create the data
data <- list(
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
)
# enero
mean(movies$revenue[grepl("-01-",movies$releaseDate)])
median(movies$revenue[grepl("-01-",movies$releaseDate)])
sd(movies$revenue[grepl("-01-",movies$releaseDate)])
# Febrero
mean(movies$revenue[grepl("-02-",movies$releaseDate)])
median(movies$revenue[grepl("-02-",movies$releaseDate)])
sd(movies$revenue[grepl("-02-",movies$releaseDate)])
# Marzo
mean(movies$revenue[grepl("-03-",movies$releaseDate)])
median(movies$revenue[grepl("-03-",movies$releaseDate)])
sd(movies$revenue[grepl("-03-",movies$releaseDate)])
# Abril
mean(movies$revenue[grepl("-04-",movies$releaseDate)])
median(movies$revenue[grepl("-04-",movies$releaseDate)])
sd(movies$revenue[grepl("-04-",movies$releaseDate)])

# Crear una tabla con valores de agregacion, mediana, media y desviacion estandar
View(aggregate(movies$revenue, by = list(month = substr(movies$releaseDate, 6, 7)), FUN = function(x) c(
  #mean = mean(x), median = median(x), sd = sd(x)
  #, 
  max = max(x)
                                                                                                        )))
# cuantas peliculas en promedio se han lanzado por mes
as.Date(movies$releaseDate)
# Convertir la columna releaseDate en un objeto de fecha
movies$releaseDate <- as.Date(movies$releaseDate)

# Crear una nueva columna con el mes de lanzamiento
movies$month <- format(movies$releaseDate, "%m")

# Calcular el número promedio de películas lanzadas por mes
mean(table(movies$month))
View(aggregate(movies$month, by = list(movies$month), FUN = function(x) c(
  mean = mean(x), median = median(x), sd = sd(x), max = max(x)
)))

View(aggregate(movies$month, by = list(month = format(as.Date(movies$releaseDate), "%m")), FUN = function(x) c(
  mean = mean(x), median = median(x), sd = sd(x), max = max(x)
)))
# diciembre
mean(movies$revenue[grepl("-12-",movies$releaseDate)])
median(movies$revenue[grepl("-12-",movies$releaseDate)])
sd(movies$revenue[grepl("-12-",movies$releaseDate)])
# Create the boxplot
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

# Create the boxplot
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

length(c(movies$revenue[grepl("-12-",movies$releaseDate)],rep(median(movies$revenue[grepl("-01-",movies$releaseDate)]), 1079-nrow(movies[grepl("-12-",movies$releaseDate),]))))
nrow(movies[grepl("-01-",movies$releaseDate),])
nrow(movies[grepl("-02-",movies$releaseDate),])
nrow(movies[grepl("-03-",movies$releaseDate),])
nrow(movies[grepl("-04-",movies$releaseDate),])
nrow(movies[grepl("-05-",movies$releaseDate),])
nrow(movies[grepl("-06-",movies$releaseDate),])
nrow(movies[grepl("-07-",movies$releaseDate),])
nrow(movies[grepl("-08-",movies$releaseDate),])
nrow(movies[grepl("-09-",movies$releaseDate),])
nrow(movies[grepl("-10-",movies$releaseDate),])
nrow(movies[grepl("-11-",movies$releaseDate),])
nrow(movies[grepl("-12-",movies$releaseDate),])
medianMonth<-tibble(
  month=c("Enero", "Febrero","Marzo","Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
  value = c(median(movies$revenue[movies$releaseDate%like%"-01-"]),
            median(movies$revenue[movies$releaseDate%like%"-02-"]),
            median(movies$revenue[movies$releaseDate%like%"-03-"]),
            median(movies$revenue[movies$releaseDate%like%"-04-"]),
            median(movies$revenue[movies$releaseDate%like%"-05-"]),
            median(movies$revenue[movies$releaseDate%like%"-06-"]),
            median(movies$revenue[movies$releaseDate%like%"-07-"]),
            median(movies$revenue[movies$releaseDate%like%"-08-"]),
            median(movies$revenue[movies$releaseDate%like%"-09-"]),
            median(movies$revenue[movies$releaseDate%like%"-10-"]),
            median(movies$revenue[movies$releaseDate%like%"-11-"]),
            median(movies$revenue[movies$releaseDate%like%"-12-"]))
  )
)
medianMonth<-tibble(
  month=c("Enero", "Febrero","Marzo","Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
  value = c(median(movies$revenue[movies$releaseDate%like%"-01-"]),
            median(movies$revenue[movies$releaseDate%like%"-02-"]),
            median(movies$revenue[movies$releaseDate%like%"-03-"]),
            median(movies$revenue[movies$releaseDate%like%"-04-"]),
            median(movies$revenue[movies$releaseDate%like%"-05-"]),
            median(movies$revenue[movies$releaseDate%like%"-06-"]),
            median(movies$revenue[movies$releaseDate%like%"-07-"]),
            median(movies$revenue[movies$releaseDate%like%"-08-"]),
            median(movies$revenue[movies$releaseDate%like%"-09-"]),
            median(movies$revenue[movies$releaseDate%like%"-10-"]),
            median(movies$revenue[movies$releaseDate%like%"-11-"]),
            median(movies$revenue[movies$releaseDate%like%"-12-"]))
  )
rm(medianMonth)
barplot(medianMonth, main = "Mediana por mes")

# ¿A qué género principal pertenecen las películas más largas?
movies$runtime[]

unique(movies$genres)
library(dplyr)
n_distinct(movies$genres)
# Son 2346 generos dado que hay peliculas que pueden pertenecer a mas de un genero
# Vamos a separar la data
install.packages("tidyr")
# load the tidyr package
library(tidyr)
# Separar los generos
moviesSeparatedGenres <- separate_rows(movies, genres, sep = "\\|")
# 20 generos de peliculas
n_distinct(moviesSeparatedGenres$genres)
unique(moviesSeparatedGenres$genres)

moviesSeparatedGenres%>% group_by(genres)

moviesSeparatedGenres %>% group_by(genres) %>% summarize(runtime_median = median(runtime, na.rm = TRUE))

library(ggplot2)
library(reshape2)
# mediana de tiempo que dura una pelicula por genero
ggplot(moviesSeparatedGenres %>% group_by(genres) %>% summarize(runtime_median = median(runtime, na.rm = TRUE)), aes(x = genres, y = runtime_median)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Median Runtime by Genre", x = "Genres", y = "Median Runtime")

# media de tiempo que dura una pelicula por genero
ggplot(moviesSeparatedGenres %>% group_by(genres) %>% summarize(runtime_mean = mean(runtime, na.rm = TRUE)), aes(x = genres, y = runtime_mean)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Mean Runtime by Genre", x = "Genres", y = "Mean Runtime")
# desviacion estandar del tiempo que dura una pelicula por genero
ggplot(moviesSeparatedGenres %>% group_by(genres) %>% summarize(runtime_sd = sd(runtime, na.rm = TRUE)), aes(x = genres, y = runtime_sd)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Standard Deviation Runtime by Genre", x = "Genres", y = "Standard Deviation Runtime")
median(moviesSeparatedGenres$runtime[moviesSeparatedGenres$genres=="History"])


#¿Cómo se correlacionan las calificaciones con el éxito comercial?
plot(movies[,c("voteAvg", "revenue")])
plot(movies[,c("voteAvg", "revenue")])
plot(movies[,c("revenue", "voteAvg")])
plot(movies[,c("revenue", "popularity")])
# coeficientes de correlacion
cor(movies$revenue, movies$popularity)
cor(movies$revenue-movies$budget, movies$voteAvg)
#diagrama de dispersion ganancias vs votos
plot(movies$voteAvg, movies$revenue-movies$budget, main="Movies Profit vs Movies Vote", xlab = "Average Vote", ylab = "Profit")
fit <- lm(movies$revenue-movies$budget ~ movies$voteAvg)
slope <- coef(fit)[2]
intercept <- coef(fit)
# Add the tendency line to the plot
abline(intercept, slope, col = "red")

boxplot(movies$budget)
shapiro.test(movies$revenue-movies$budget)
library(nortest)
# pruebas de normalidad
lillie.test(movies$voteAvg)
lillie.test(movies$revenue-movies$budget)
# diagrama de dispersion ganancias vs popularidad
plot(movies$popularity, movies$revenue-movies$budget, main="Movies Profit vs Popularity", xlab = "Popularity", ylab = "Profit")
cor(movies$revenue-movies$budget, movies$popularity)
