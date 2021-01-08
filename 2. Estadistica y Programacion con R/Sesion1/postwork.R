###########################################################-
# Objective: Postwork Sesión 1
# Author: Gabriel Sainz Vázquez
# Date Modified: 08/01/2021
###########################################################-


# 1. Importa los datos de soccer de la temporada 2019/2020 de la primera división 
#    de la liga española a R, los datos los puedes encontrar en el siguiente 
#    enlace: https://www.football-data.co.uk/spainm.php

url <- "https://www.football-data.co.uk/mmz4281/2021/SP1.csv"
soccer_spain <- read.csv(url)
head(soccer_spain)

# 2. Del data frame que resulta de importar los datos a R, extrae las columnas que 
#    contienen los números de goles anotados por los equipos que jugaron en casa 
#    (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG)

home_away_gol <- soccer_spain[, c("Date", "Time", "HomeTeam",
                 "AwayTeam", "FTHG", "FTAG")]
head(home_away_gol)

# 3.  Consulta cómo funciona la función table en R al ejecutar en la consola ?table
?table

# Posteriormente elabora tablas de frecuencias relativas para estimar las 
# siguientes probabilidades:
      
# - La probabilidad (marginal) de que el equipo que juega en casa anote x goles 
#   (x = 0, 1, 2, ...)
marg_prob_home <- table(home_away_gol$FTHG)
marg_prob_home/sum(marg_prob_home)

# - La probabilidad (marginal) de que el equipo que juega como visitante anote y 
#   goles (y = 0, 1, 2, ...)
marg_prob_away <- table(home_away_gol$FTAG)
marg_prob_away/sum(marg_prob_away)

# - La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y 
#   el equipo que juega como visitante anote y goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)

table(home_away_gol$FTHG, home_away_gol$FTAG)/sum(table(home_away_gol$FTHG, home_away_gol$FTAG))



