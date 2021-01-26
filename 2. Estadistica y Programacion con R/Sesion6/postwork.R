###########################################################-
# Objective: Postwork Sesion 6
# Author: Gabriel Sainz Vázquez
# Date Modified: 26/01/2021
###########################################################-

# Importe el conjunto de datos match.data.csv a R y realice lo siguiente:
setwd("C:/Users/Gabo/Desktop/Becas Santander- Data Science/2.Fase2/2. Estadística y Programación con R/Sesion6/data")

data <- read.csv("match.data.csv")

# - Agrega una nueva columna sumagoles que contenga la suma de goles por partido.
library(dplyr)
library(lubridate)

data <- mutate(data, sumagoles = home.score+ away.score, date = as.Date(date, "%Y-%m-%d"))

head(data)
str(data)
# - Obtén el promedio por mes de la suma de goles.

gol_mean_by_month <- data %>% group_by(año =year(date) , mes = month(date)) %>%
      summarise(mean = mean(sumagoles))

gol_mean_by_month <- as.data.frame(gol_mean_by_month) %>% filter(año < 2020)
gol_mean_by_month


# Nos podemos dar cuenta que la serie empieza en el mes 8 en el año 2010. 
# Y que faltan algunos meses como 6 y 7. 
# Por lo que el periodo es de 10 meses.
# De igual manera, después de analizar la serie, nos podemos dar cuenta que 
# en el año 2013 sí hay un mes 6, por lo que se tiene que quitar para no afectar
# nuestra serie

gol_mean_by_month <- gol_mean_by_month %>% filter(mes != 6)

# - Crea la serie de tiempo del promedio por mes de la suma de goles hasta diciembre de 2019.

gol_promedio_ts <- ts(gol_mean_by_month[, 3], start = 1, freq = 10)


# - Grafica la serie de tiempo.
plot(gol_promedio_ts)








