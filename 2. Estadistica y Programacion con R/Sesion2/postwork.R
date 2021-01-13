###########################################################-
# Objective: Postwork  Sesion 2
# Author: Gabriel Sainz Vázquez
# Date Modified: 12/01/2021
###########################################################-


# 1. Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 
#    2019/2020 de la primera división de la liga española a R, los datos los 
#    puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php

setwd("C:\\Users\\Gabo\\Desktop\\Becas Santander- Data Science\\2.Fase2\\2. Estadística y Programación con R\\Sesion2\\data_postwork")
url1 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv" # CSV 2017/2018
url2 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv" # CSV 2018/2019
url3 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv" # CSV 2019/2020

download.file(url = url1, destfile = "SP1-1718.csv", mode = "wb")
download.file(url = url2, destfile = "SP1-1819.csv", mode = "wb")
download.file(url = url3, destfile = "SP1-1920.csv", mode = "wb")

lista <- lapply(dir(), read.csv) # Guardamos los archivos en lista

# 2. Obten una mejor idea de las características de los data frames al usar las 
#    funciones: str, head, View y summary

str(lista[[1]])
str(lista[[2]])
str(lista[[3]])
lapply(lista, head)
View(lista[[1]])
View(lista[[2]])
View(lista[[3]])
lapply(lista, summary)

#3. Con la función select del paquete dplyr selecciona únicamente las columnas 
#   Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR; esto para cada uno de los data 
#   frames. (Hint: también puedes usar lapply).
library(dplyr)

lista1 <- lapply(lista, select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

lapply(lista1, str)

# 4. Asegúrate de que los elementos de las columnas correspondientes de los 
#    nuevos data frames sean del mismo tipo (Hint 1: usa as.Date y mutate para 
#    arreglar las fechas). Con ayuda de la función rbind forma un único data frame 
#    que contenga las seis columnas mencionadas en el punto 3 (Hint 2: la función 
#    do.call podría ser utilizada).
head(lista1[[3]])

lista1 <- lapply(lista1, mutate, Date = as.Date(Date, "%d/%m/%y"))

lapply(lista1, str)

data <- do.call(rbind, lista1)

head(data)
tail(data)
dim(data)
str(data)
summary(data)

