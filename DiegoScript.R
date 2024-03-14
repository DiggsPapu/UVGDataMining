## Asignar el working directory
setwd('D:/UVG/CODING/Semestre7/MineriaDeDatos/UVGDataMining')
# Aniadir librerias
library(tidyr)
install.packages("DataExplorer")
library(DataExplorer)
library(nortest)
library(dplyr)
library(ggplot2)
library(reshape2)

## asignacion del dataset a trabajar
train<-read.csv("./train.csv")
create_report(
  train,
  output_file = "train_report.html",
  output_dir = getwd(),
  config = configure_report(),
  report_title = "Houses Data Report"
)
View(train)
# Eliminar columnas invalidas
invalidos<-c( "PoolQC", "MiscFeature", "Alley", "Fence", "FireplaceQu", "GarageFinish", "GarageCond", "GarageQual", "GarageYrBlt", "GarageType", "LotFrontage")
# Desarrollar nuevo explorador de datos
train<-train[,-which(names(train)%in% invalidos)]
View(train)
create_report(
  train,
  output_file = "train_report1.html",
  output_dir = getwd(),
  config = configure_report(),
  report_title = "Houses Data Report"
)