
library(dplyr)
library(ggplot2)
library(lubridate)
movies <- read.csv("movies.csv")


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

