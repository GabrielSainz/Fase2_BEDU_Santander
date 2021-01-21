###########################################################-
# Objective: Postwork Sesion 5
# Author: Gabriel Sainz Vázquez
# Date Modified: 21/01/2021
###########################################################-


# A partir del conjunto de datos de soccer de la liga española de las temporadas 
# 2017/2018, 2018/2019 y 2019/2020, crea el data frame SmallData, que contenga 
# las columnas date, home.team, home.score, away.team y away.score; esto lo 
# puede hacer con ayuda de la función select del paquete dplyr. Luego establece
# un directorio de trabajo y con ayuda de la función write.csv guarda el data 
# frame como un archivo csv con nombre soccer.csv. Puedes colocar como argumento
# row.names = FALSE en write.csv.

library(dplyr)
library(ggplot2)

url1 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv" # CSV 2017/2018
url2 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv" # CSV 2018/2019
url3 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv" # CSV 2019/2020

list.url <- list(url1, url2, url3)

lista <- lapply(list.url, read.csv) # Guardamos los archivos en lista

lista1 <- lapply(lista, select, date = Date, home.team = HomeTeam, 
                 home.score = FTHG, away.team = AwayTeam,  away.score = FTAG)
lista1 <- lapply(lista1, mutate, date = as.Date(date, "%d/%m/%y"))
SmallData <- do.call(rbind, lista1)

setwd("C:\\Users\\Gabo\\Desktop\\Becas Santander- Data Science\\2.Fase2\\2. Estadística y Programación con R\\Sesion5")

write.csv(SmallData, file = "soccer.csv", row.names = FALSE)

# Con la función create.fbRanks.dataframes del paquete fbRanks importe el 
# archivo soccer.csv a R y al mismo tiempo asignelo a una variable llamada 
# listasoccer. Se creará una lista con los elementos scores y teams que son data 
# frames listos para la función rank.teams. Asigna estos data frames a variables 
# llamadas anotaciones y equipos.

# install.packages("fbRanks")

library(fbRanks)

listasoccer <- create.fbRanks.dataframes("soccer.csv")

listasoccer$scores
listasoccer$teams

rank.teams(scores = listasoccer$scores, teams = listasoccer$teams)

# Con ayuda de la función unique crea un vector de fechas (fecha) que no se 
# repitan y que correspondan a las fechas en las que se jugaron partidos. 
# Crea una variable llamada n que contenga el número de fechas diferentes. 
# Posteriormente, con la función rank.teams y usando como argumentos los data
# frames anotaciones y equipos, crea un ranking de equipos usando unicamente 
# datos desde la fecha inicial y hasta la penúltima fecha en la que se jugaron 
# partidos, estas fechas las deberá especificar en max.date y min.date. Guarda 
# los resultados con el nombre ranking.

dates <- unique(SmallData$date)
n <- length(dates)

ranking <- rank.teams(scores = listasoccer$scores, teams = listasoccer$teams, 
                      max.date = dates[n-1], min.date = min(dates))

# Finalmente estima las probabilidades de los eventos, el equipo de casa gana, 
# el equipo visitante gana o el resultado es un empate para los partidos que se 
# jugaron en la última fecha del vector de fechas fecha. Esto lo puedes hacer 
# con ayuda de la función predict y usando como argumentos ranking y fecha[n] 
# que deberá especificar en date.

predict(ranking, min.date = dates[n], max.date = dates[n])











