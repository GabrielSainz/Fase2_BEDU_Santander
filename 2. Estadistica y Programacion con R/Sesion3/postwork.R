###########################################################-
# Objective: Postwork Sesion 3
# Author: Gabriel Sainz Vázquez
# Date Modified: 15/01/2021
###########################################################-

# Con el último data frame obtenido en el postwork de la sesión 2, elabora tablas 
# de frecuencias relativas para estimar las siguientes probabilidades:

setwd("C:\\Users\\Gabo\\Desktop\\Becas Santander- Data Science\\2.Fase2\\2. Estadística y Programación con R\\Sesion3\\data_postwork")
url1 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv" # CSV 2017/2018
url2 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv" # CSV 2018/2019
url3 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv" # CSV 2019/2020

download.file(url = url1, destfile = "SP1-1718.csv", mode = "wb")
download.file(url = url2, destfile = "SP1-1819.csv", mode = "wb")
download.file(url = url3, destfile = "SP1-1920.csv", mode = "wb")

lista <- lapply(dir(), read.csv) # Guardamos los archivos en lista

library(dplyr)

lista1 <- lapply(lista, select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

lista1 <- lapply(lista1, mutate, Date = as.Date(Date, "%d/%m/%y"))

data <- do.call(rbind, lista1)

head(data)

# - La probabilidad (marginal) de que el equipo que juega en casa anote x goles 
#   (x=0,1,2,)
table(data$FTHG)/dim(data)[1]

# - La probabilidad (marginal) de que el equipo que juega como visitante anote 
#   y goles (y=0,1,2,)
table(data$FTAG)/dim(data)[1]

# - La probabilidad (conjunta) de que el equipo que juega en casa anote x goles 
#   y el equipo que juega como visitante anote y goles (x=0,1,2,, y=0,1,2,)
table(data$FTHG, data$FTAG)/dim(data)[1]

# Realiza lo siguiente:
# - Un gráfico de barras para las probabilidades marginales estimadas del número
#   de goles que anota el equipo de casa
library(ggplot2)

prob_marg <- as.data.frame(table(data$FTHG)/dim(data)[1])
prob_marg$prob_away <- c(table(data$FTAG)/dim(data)[1], 0, 0)

names(prob_marg) <- c("goles", "prob_home", "prob_away")

ggplot(prob_marg, aes(x = goles, y = prob_home)) + 
      geom_bar (stat="identity", col = "black", fill = 'blue') +
      ggtitle('Probabilid Marginal Equipo de Casa') + 
      theme_light()

# - Un gráfico de barras para las probabilidades marginales estimadas del número 
#   de goles que anota el equipo visitante.
ggplot(prob_marg, aes(x = goles, y = prob_away)) + 
      geom_bar (stat="identity", col = "black", fill = 'blue') +
      ggtitle('Probabilid Marginal Equipo de Visitante') + 
      theme_light()

# - Un HeatMap para las probabilidades conjuntas estimadas de los números de 
#   goles que anotan el equipo de casa y el equipo visitante en un partido.

prob_conjunta <- as.data.frame(table(data$FTHG, data$FTAG)/dim(data)[1])
names(prob_conjunta) <- c("home", "away", "prob")

ggplot(prob_conjunta, aes(x = home, y = away, fill = prob)) + 
      geom_tile() + 
      ggtitle("Probabilidad conjunta") 

