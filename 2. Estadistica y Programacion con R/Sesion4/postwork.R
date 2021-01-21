###########################################################-
# Objective: Postwork Sesion 4
# Authors: Team 11
#          - Gabriel Sainz Vázquez
#          - Edgar Arellano Ruelas
#          - Aldo Omar Enriquez Velazquez
#          - Iván Delgado de la paz
# Date Modified: 21/01/2021
###########################################################-


# Ahora investigarás la dependencia o independencia del número de goles anotados 
# por el equipo de casa y el número de goles anotados por el equipo visitante 
# mediante un procedimiento denominado bootstrap, revisa bibliografía en internet
# para que tengas nociones de este desarrollo.

library(dplyr)
library(ggplot2)

url1 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv" # CSV 2017/2018
url2 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv" # CSV 2018/2019
url3 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv" # CSV 2019/2020

list.url <- list(url1, url2, url3)

lista <- lapply(list.url, read.csv) # Guardamos los archivos en lista

lista1 <- lapply(lista, select, Date, HomeTeam, AwayTeam, FTHG, FTAG)
lista1 <- lapply(lista1, mutate, Date = as.Date(Date, "%d/%m/%y"))
data <- do.call(rbind, lista1)
head(data)

# Ya hemos estimado las probabilidades conjuntas de que el equipo de casa anote 
# X=x goles (x=0,1,... ,8), y el equipo visitante anote Y=y goles (y=0,1,... ,6),
# en un partido. Obtén una tabla de cocientes al dividir estas probabilidades 
# conjuntas por el producto de las probabilidades marginales correspondientes.

(table1 <- table(data$FTHG)/dim(data)[1]) # probabilidad marginal equipo casa

(table2 <- table(data$FTAG)/dim(data)[1]) # probabilidad marginal equipo visitante

table(data$FTHG, data$FTAG)
(table3 <- table(data$FTHG, data$FTAG)/dim(data)[1]) # probabilidad conjunta

for (i in 1:length(table1)){
   for (j in 1:length(table2)){
      table3[i, j] <- table3[i, j]/(table1[i]* table2[j])
   }
   
}

table3

# Mediante un procedimiento de boostrap, obtén más cocientes similares a los 
# obtenidos en la tabla del punto anterior. Esto para tener una idea de las 
# distribuciones de la cual vienen los cocientes en la tabla anterior. Menciona 
# en cuáles casos le parece razonable suponer que los cocientes de la tabla en
# el punto 1, son iguales a 1 (en tal caso tendríamos independencia de las 
# variables aleatorias X y Y).

# https://rubenfcasal.github.io/simbook/remuestreo-bootstrap.html
# La idea es aproximar características poblacionales por las correspondientes de 
# la distribución empírica de los datos observados

# Las características de la dist. empirica se puede aproximar mediante simulacion 
# - Caso iid -> remuestreo 

# Primero se realiza un remuestreo del data frame de los goles por partido: 
# el tamaño de la muestra la utilizaremos de 450 con reemplazo

# Se realiza una función con parámetros data (datos de los partidos) y n (numero 
# de veces a iterar)

boot.str <- function(data, n = 1000 ){
      df <- data.frame()
      list <- list()
      for (h in 1:n){
            num <- 0
            data1 <- 0
            table1 <- 0
            table2 <- 0
            table3 <- 0
            
            num <- sample(dim(data)[1], 450, replace = TRUE)
            data1 <- data[num, ]
            
            table1 <- table(data1$FTHG)/dim(data1)[1] # probabilidad marginal equipo casa
            
            table2 <- table(data1$FTAG)/dim(data1)[1] # probabilidad marginal equipo visitante
            
            table3 <- table(data1$FTHG, data1$FTAG)/dim(data1)[1] # probabilidad conjunta
            
            for (i in 1:length(table1)){
                  for (j in 1:length(table2)){
                        table3[i, j] <- table3[i, j]/(table1[i]* table2[j])
                  }
            }
            
            if(dim(table3)[1] != 9){
               
               for(i in 1:(9 - dim(table3)[1])){
                  table3 <- rbind(table3, rep(0, dim(table3)[2]))   
               }
            }
            
            if(dim(table3)[2] != 7){
               for(i in 1:(7 - dim(table3)[2])){
                  table3 <- cbind(table3, rep(0, dim(table3)[1]))   
               }
            }
            
            list[[h]] <- table3
      }
      
      list
}

# Posteriormente, nos podemos dar una idea de las distribuciones de los cocientes
# graficando histogramas que representen la distribución de cada entrada de la 
# tabla

# Cuando el cociente es igual a 1, significa que dividendo y el divisor son 
# practicmanete el mismo. Esto quiere decir que,la función de densidad conjunta 
# en el punto (i, j) es igual a la multiplicación de las funciones marginales 
# fx(i)*fy(j), es decir fxy(i,j) = fx(i)*fy(j), por lo que, fx y fy son indepen-
# dientes.

hist_bootstrap <- function(list, gol_home, gol_away){
      m <- mean(sapply(a, function(list){list[gol_home+1,gol_away+1]}))
      
      hist(sapply(a, function(list){list[gol_home + 1,gol_away + 1]}), 
           main = paste("Histogrma Bootstrap",  gol_home, 
           "goles casa y", gol_away, "goles visitante"), 
           sub = paste("Media:", m) , xlab = "")
      abline(v = m, col = "red", lty = c(2), lwd = 3)
}


table3

a <- boot.str(data, 1000)

# A continuación, se muestran los histogramas con su respectiva media del 
# remuestreo para el número de goles anotados del equipo visitante y el de casa.

# Note que los histogramas marcado con un asterisco (*) son los que consideramos 
# que tienen una independencia entre el número anotado de goles por el equipo
# visitante y el equipo de casa. 
# Por otro lado, nos damos cuenta que esta independencia sólo se da en casos 
# donde los goles no tienen mucha diferencia entre ambos.
# Sin embargo, consideramos que los que no son independientes (iguales a 1), 
# pueden llegar a ser por una demotivación del equipo contrario 
# lo que les llegan a meter más goles, por lo que sí existiría una dependencia.

hist_bootstrap(a, 0, 0) # * 
hist_bootstrap(a, 0, 1) # * 
hist_bootstrap(a, 0, 2) # * 
hist_bootstrap(a, 0, 3)
hist_bootstrap(a, 0, 4)
hist_bootstrap(a, 0, 5)
hist_bootstrap(a, 0, 6)


hist_bootstrap(a, 1, 0) # * 
hist_bootstrap(a, 1, 1) # * 
hist_bootstrap(a, 1, 2) # * 
hist_bootstrap(a, 1, 3) # * 
hist_bootstrap(a, 1, 4)
hist_bootstrap(a, 1, 5)
hist_bootstrap(a, 1, 6)


hist_bootstrap(a, 2, 0)
hist_bootstrap(a, 2, 1) # * 
hist_bootstrap(a, 2, 2) # * 
hist_bootstrap(a, 2, 3)
hist_bootstrap(a, 2, 4)
hist_bootstrap(a, 2, 5)
hist_bootstrap(a, 2, 6)

hist_bootstrap(a, 3, 0)
hist_bootstrap(a, 3, 1)
hist_bootstrap(a, 3, 2) # * 
hist_bootstrap(a, 3, 3) # * 
hist_bootstrap(a, 3, 4)
hist_bootstrap(a, 3, 5)
hist_bootstrap(a, 3, 6)

hist_bootstrap(a, 4, 0)
hist_bootstrap(a, 4, 1)
hist_bootstrap(a, 4, 2)
hist_bootstrap(a, 4, 3)
hist_bootstrap(a, 4, 4)
hist_bootstrap(a, 4, 5)
hist_bootstrap(a, 4, 6)

hist_bootstrap(a, 5, 0)
hist_bootstrap(a, 5, 1)
hist_bootstrap(a, 5, 2) # * 
hist_bootstrap(a, 5, 3)
hist_bootstrap(a, 5, 4)
hist_bootstrap(a, 5, 5)
hist_bootstrap(a, 5, 6)

hist_bootstrap(a, 6, 0)
hist_bootstrap(a, 6, 1)
hist_bootstrap(a, 6, 2) 
hist_bootstrap(a, 6, 3)
hist_bootstrap(a, 6, 4)
hist_bootstrap(a, 6, 5)
hist_bootstrap(a, 6, 6)

hist_bootstrap(a, 7, 0)
hist_bootstrap(a, 7, 1)
hist_bootstrap(a, 7, 2)
hist_bootstrap(a, 7, 3)
hist_bootstrap(a, 7, 4)
hist_bootstrap(a, 7, 5)
hist_bootstrap(a, 7, 6)

hist_bootstrap(a, 8, 0)
hist_bootstrap(a, 8, 1)
hist_bootstrap(a, 8, 2)
hist_bootstrap(a, 8, 3)
hist_bootstrap(a, 8, 4)
hist_bootstrap(a, 8, 5)
hist_bootstrap(a, 8, 6)

