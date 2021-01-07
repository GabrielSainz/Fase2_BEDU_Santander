###########################################################-
# Objective: RETOS SESIÓN 1
# Author: Gabriel Sainz Vázquez
# Date Modified: 07/01/2021
###########################################################-

###########################################################-
# RETO 1: Lectura y escritura de datos ----
###########################################################-

# 1. Leer el archivo "netflix_titles.csv" desde Github
#  (https://raw.githubusercontent.com/ecoronadoj/Sesion_1/main/Data/netflix_titles.csv), 
#   almacenarlo en un df llamado netflix

url <- "https://raw.githubusercontent.com/ecoronadoj/Sesion_1/main/Data/netflix_titles.csv"
netflix_titles <- read.csv(url)
head(netflix_titles)
names(netflix_titles)

# 2. Obtener la dimensión y el tipo de objeto que se obtiene
dim(netflix_titles)
class(netflix_titles)

# 3. Obtener los títulos que se estrenaron después del 2015. Almacenar este df 
#   en una variable llamada net.2015
net.2015 <- netflix_titles[which(netflix_titles$release_year > 2015),]
net.2015 <- netflix_titles[netflix_titles$release_year > 2015,]
str(net.2015)

# 4. Escribir los resultados en un archivo .csv llamado res.netflix.csv
setwd("C:\\Users\\Gabo\\Desktop\\Becas Santander- Data Science\\2.Fase2\\2. Estadística y Programación con R\\Sesion1")
write.csv(net.2015, file = "res.netflix.csv")

###########################################################-
# RETO 2: Loops ----
###########################################################-

# 1. Genera un vector de 44 entradas (aleatorias) llamado ran

ran <- rnorm(44)

# 2. Escribe un loop que eleve al cubo las primeras 15 entradas y les sume 12
ran1 <- ran

for (i in 1:15){
      ran1[i] <- ran[i]^3 + 12
}

# 3. Guarda el resultado en un data frame, donde la primera columna sea el número 
#    aleatorio y la segunda el resultado, nómbralo df.al

df.al <- data.frame(random = ran, 
                    result = ran1)

# 4. Escribe el pseudocódigo del loop anterior








