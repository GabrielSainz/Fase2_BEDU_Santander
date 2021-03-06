###########################################################-
# Objective: Sesion 2
# Author: Gabriel Sainz V�zquez
# Date Modified: 11/01/2021
###########################################################-

# EJEMPLO 1 ----

# Medidas de tendencia central

x <- c(4000,9000,9000,10000)
mean(x)
median(x)

# Si lo que deseamos es obtener la moda de un conjunto de mediciones, una alternativa
# es instalar el paquete DescTools en R mediante la instrucci�n 
# install.packages("DescTools"), luego utilizamos la funci�n Mode del paquete DescTools

# install.packages("DescTools")

library(DescTools)

Mode(x)

# Medidas de posici�n

# En R utilizamos la funci�n quantile para obtener cuantiles muestrales. Por ejemplo

x <- c(29, 13, 62, 4, 63, 96, 1, 90, 50, 46)
quantile(x, 0.25) #cuantil del 25%
quantile(x, c(0.25, 0.50, 0.75)) #cuartiles
quantile(x, seq(0.1,0.9, by = 0.1)) #deciles

# Medidas de dispersi�n 

# Podemos calcular el rango intercuart�lico en R con la funci�n IQR, por ejemplo
IQR(x)

# o bien 
quantile(x, probs = 0.75) - quantile(x, probs = 0.25) # Tercer cuartil menos primer cuartil

var(x)
sd(x)


# RETO 1 ----

#Considere el siguiente vector

set.seed(134)
x <- round(rnorm(1000, 175, 6), 1)
# 1. Calcule, la media, mediana y moda de los valores en x
median(x)
mean(x)
Mode(x)

# Obtenga los deciles de los n�meros en x
quantile(x, seq(.1,.9, by = 0.1))

# Encuentre la rango intercuart�lico, la desviaci�n est�ndar y varianza muestral
# de las mediciones en x
IQR(x)
sd(x)
var(x)

# EJEMPLO 2 ----

# Funci�n str
# str es una funci�n que muestra de manera compacta la estructura interna de un 
# objeto de R. Por ejemplo, si usamos como argumento de str el conjunto de datos 
# iris que podemos encontrar en R

str(iris)

# entonces la salida de la instrucci�n nos muestra el tipo de objeto, n�mero de 
# observaciones y de variables, as� como el tipo de dato al que corresponde cada variable.

# Funci�n summary
# La funci�n summary es una funci�n gen�rica usada para obtener resumenes de 
# diferentes objetos de R, por ejemplo

summary(1:100)
summary(mtcars)

# Tambi�n es �til para obtener resumenes de los resultados de diferentes ajustes a modelos

set.seed(57)
x <- rnorm(35)
e <- rnorm(35)
y <- 5 + 2*x + e
modelo <- lm(y~x)
summary(modelo)

# Funci�n head
# La funci�n head devuelve la primera parte de un data frame, tabla, matriz, 
# vector o funci�n. Por ejemplo, al usar el data frame mtcars como argumento de 
# la funci�n head, se devolver�n �nicamente las primeras seis filas del data frame

head(mtcars)
# la funci�n tail funciona de manera similar, pero en lugar de devolver la 
# primera parte de un objeto, devuelve la �ltima parte de este, por ejemplo, al 
# ejecutarse la siguiente instrucci�n

tail(mtcars)
# se devolver�n las �ltimas seis filas del data frame

# Funci�n View
# La funci�n View aplicada a un objeto de R como un data frame, invoca un visor 
# de datos al estilo de una hoja de c�lculo, por ejemplo
View(iris)

# RETO 2 ----

# Considere el data frame `mtcars` de `R` y utilice las funciones `str`, 
# `summary`, `head` y `View` para observar las caracter�sticas del objeto
# y tener una mayor comprensi�n de este.

head(mtcars)
str(mtcars)
summary(mtcars)
View(mtcars)


# EJEMPLO 3 ----

# El paquete dplyr cuenta con varias funciones muy �tiles para manipular y 
# transformar data frames. Una vez instalado el paquete dplyr, puede cargarlo en 
# R de la siguiente manera (sin mensajes ni advertencias)

suppressMessages(suppressWarnings(library(dplyr)))
# Vamos a descargar archivos csv que contienen datos del covid-19 para mostrar 
# como funcionan algunas funciones del paquete dplyr. Las url desde las cuales 
# descargamos los datos son las siguientes

setwd("C:\\Users\\Gabo\\Desktop\\Becas Santander- Data Science\\2.Fase2\\2. Estad�stica y Programaci�n con R\\Sesion2\\coronavirus")

url1 <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_confirmed_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv"
url2 <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_deaths_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv"
# Descargamos los datos en nuestro directorio de trabajo con la siguiente instrucci�n

download.file(url = url1, destfile = "st19ncov-confirmados.csv", mode = "wb")
download.file(url = url2, destfile = "st19ncov-muertes.csv", mode = "wb")

# Una vez que hemos descargado los datos, importamos a R los datos de casos 
# confirmados y muertes por covid-19

conf <- read.csv("st19ncov-confirmados.csv")
mu <- read.csv("st19ncov-muertes.csv")
# Utilizamos las funciones str y head para ver algunas caracter�sticas

str(conf); str(mu)
head(conf); head(mu)

# Ahora seleccionamos todas las filas excepto la primera, esto para cada data frame

Sconf <- conf[-1, ]
Smu <- mu[-1, ]
# Con la funci�n select del paquete dplyr, del data frame de casos confirmados 
# seleccionamos �nicamente las columnas de pa�s, fecha y n�mero acumulado de casos

Sconf <- select(Sconf, Country.Region, Date, Value) # Pa�s, fecha y acumulado de infectados
# Con la funci�n rename, renombramos las columnas correspondientes al pa�s y al 
# n�mero acumulado de infectados por covid-19

Sconf <- rename(Sconf, Country = Country.Region, Infectados = Value)
str(Sconf)
# Como cada una de las columnas del �ltimo data frame aparecen como factor, con 
# la funci�n mutate transformamos las columnas correspondientes a fechas y a 
# n�mero de infectados, esto para que R reconozca como fechas la columna 
# correspondiente y como n�meros los elementos de la columna que indica el 
# acumulado de casos.

Sconf <- mutate(Sconf, Date = as.Date(Date, "%Y-%m-%d"), Infectados = as.numeric(as.character(Infectados)))
# Hacemos algo similar con el data frame correspondiente al n�mero acumulado de muertos

Smu <- select(Smu, Country.Region, Date, Value) # Seleccionamos pa�s, fecha y acumulado de muertos
Smu <- rename(Smu, Country = Country.Region, Muertos = Value) # Renombramos
Smu <- mutate(Smu, Date = as.Date(Date, "%Y-%m-%d"), Muertos = as.numeric(as.character(Muertos))) # Transformamos
Scm <- merge(Sconf, Smu) # Unimos infectados y muertos acumulados para cada fecha
mex <- filter(Scm, Country == "Mexico") # Seleccionamos s�lo a M�xico
mex <- filter(mex, Infectados != 0) # Primer d�a de infectados
# Para M�xico, creamos otras variables o columnas de inter�s con ayuda de la 
# funci�n mutate

mex <- mutate(mex, NI = c(1, diff(Infectados))) # Nuevos infectados por d�a
mex <- mutate(mex, NM = c(0, diff(Muertos))) # Nuevos muertos por d�a

mex <- mutate(mex, Letalidad = round(Muertos/Infectados*100, 1)) # Tasa de letalidad

mex <- mutate(mex, IDA = lag(Infectados), MDA = lag(Muertos)) # Valores d�a anterior
mex <- mutate(mex, FCI = Infectados/IDA, FCM = Muertos/MDA) # Factores de Crecimiento
mex <- mutate(mex, Dia = 1:dim(mex)[1]) # D�as de contingencia

# Finalmente, observamos algunas filas de nuestro �ltimo data frame

head(mex); tail(mex)

library(ggplot2)

ggplot(mex, aes(x = Dia, y = Letalidad)) + geom_line() + theme_bw()

# EJEMPLO 4 ----

# Funci�n cbind
# La funci�n cbind toma una sucesi�n de argumentos que pueden ser vectores, 
# matrices o data frames y los combina por columnas, por ejemplo

cbind(1:10, 11:20, 21:30)
cbind(1:10, matrix(11:30, ncol =2))
cbind(data.frame(x = 1:10, y = 11:20), z = 21:30)

# Funci�n rbind
# La funci�n rbind funciona de manera similar a cbind, pero en lugar de combinar
# los objetos por columnas, los combina por filas, como ejemplo tenemos lo siguiente

df1 <- data.frame(x = 1:5, y = 6:10, z = 16:20)
df2 <- data.frame(x = 51:55, y = 101:105, z = 151:155)
df1; df2
rbind(df1, df2)

# EJEMPLO 5 ----

# Funci�n apply
# La funci�n apply regresa un vector, arreglo o lista de valores obtenidos al 
# aplicar una funci�n a los m�rgenes de un arreglo o matriz. Por ejemplo

X <- matrix(1:49, ncol = 7)
X
apply(X, 1, mean) # c�lculo de la media para las filas
apply(X, 2, median) # c�lculo de la mediana para las columnas

# Funci�n lapply
# La funci�n lapply se usa de la siguiente manera lapply(X, FUN, ...) donde X 
# puede ser un vector o una lista, FUN es una funci�n que ser� aplicada a cada 
# elemento de X y ... representa argumentos opcionales para FUN. lapply regresa 
# una lista de la misma longitud que X, en donde cada elemento de la lista es el
# resultado de aplicar FUN al elemento que corresponde de X.

# Vamos a utilizar lapply para leer un conjunto de archivos csv de manera 
# consecutiva y "r�pida", para esto debemos especificar un directorio de trabajo 
# y descargar los archivos csv en nuestro directorio, por ejemplo, puede crear 
# la carpeta soccer para descargar los datos

setwd("C:\\Users\\Gabo\\Desktop\\Becas Santander- Data Science\\2.Fase2\\2. Estad�stica y Programaci�n con R\\Sesion2\\soccer")
u1011 <- "https://www.football-data.co.uk/mmz4281/1011/SP1.csv"
u1112 <- "https://www.football-data.co.uk/mmz4281/1112/SP1.csv"
u1213 <- "https://www.football-data.co.uk/mmz4281/1213/SP1.csv"
u1314 <- "https://www.football-data.co.uk/mmz4281/1314/SP1.csv"

download.file(url = u1011, destfile = "SP1-1011.csv", mode = "wb")
download.file(url = u1112, destfile = "SP1-1112.csv", mode = "wb")
download.file(url = u1213, destfile = "SP1-1213.csv", mode = "wb")
download.file(url = u1314, destfile = "SP1-1314.csv", mode = "wb")

# podemos visualizar el nombre de los archivos descargados en un vector de 
# strings de la siguiente manera

dir()

# podemos leer con una sola instrucci�n los archivos descargados usando la 
# funci�n lapply de la siguiente manera

lista <- lapply(dir(), read.csv) # Guardamos los archivos en lista
# los elementos de lista son los archivos csv leidos y se encuentran como data frames

library(dplyr)
lista <- lapply(lista, select, Date:FTR) # seleccionamos solo algunas columnas de cada data frame
head(lista[[1]]); head(lista[[2]]); head(lista[[3]]); head(lista[[4]])
# cada uno de los data frames que tenemos en lista, los podemos combinar en 
# un �nico data frame utilizando las funciones rbind y do.call de la siguiente manera

# Funci�n do.call
data <- do.call(rbind, lista)
head(data)
dim(data)

# RETO 3----

# Descargue los archivos csv que corresponden a las temporadas 2017/2018, 
# 2018/2019, 2019/2020 y 2020/2021 de la Bundesliga 1 y que se encuentran en el 
# siguiente enlace https://www.football-data.co.uk/germanym.php

setwd("C:\\Users\\Gabo\\Desktop\\Becas Santander- Data Science\\2.Fase2\\2. Estad�stica y Programaci�n con R\\Sesion2\\soccer1")
url1 <- "https://www.football-data.co.uk/mmz4281/2021/D1.csv"
url2 <- "https://www.football-data.co.uk/mmz4281/1920/D1.csv"
url3 <- "https://www.football-data.co.uk/mmz4281/1819/D1.csv"
url4 <- "https://www.football-data.co.uk/mmz4281/1718/D1.csv"

# Importe los archivos descargados a R

download.file(url = url1, destfile = "D1-2021.csv", mode = "wb")
download.file(url = url2, destfile = "D1-1920.csv", mode = "wb")
download.file(url = url3, destfile = "D1-1819.csv", mode = "wb")
download.file(url = url4, destfile = "D1-1718.csv", mode = "wb")

# Usando la funci�n select del paquete dplyr, seleccione �nicamente las columnas:
# - Date
# - HomeTeam
# - AwayTeam
# - FTHG
# - FTAG
# - FTR

lista <- lapply(dir(), read.csv) 
lista <- lapply(lista, select, Date, HomeTeam, AwayTeam,FTHG,FTAG, FTR)

# Combine cada uno de los data frames en un �nico data frame con ayuda de las 
# funciones: rbind, do.call
data <- do.call(rbind, lista)
head(data)

# EJEMPLO 6 ----
# Lectura de JSON y XML
# install.packages("rjson")

library(rjson)
library(XML)

# Lectura de JSON
# Podemos leer un archivo json de la siguiente manera

URL1 <- "https://tools.learningcontainer.com/sample-json-file.json"
JsonData <- fromJSON(file = URL1)
class(JsonData)
length(JsonData)
str(JsonData)

# Lectura de XML
# Podemos leer un archivo xml de la siguiente manera

URL2 <- "http://www-db.deis.unibo.it/courses/TW/DOCS/w3schools/xml/cd_catalog.xml"
xmlfile <- xmlTreeParse(URL2) # Parse the XML file. Analizando el XML
topxml <- xmlSApply(xmlfile, function(x) xmlSApply(x, xmlValue)) # Mostrando los datos de una forma amigable
xml_df <- data.frame(t(topxml), row.names= NULL) # Colocandolos en un Data Frame
str(xml_df) # Observar la naturaleza de las variables del DF
head(xml_df)


# Una manera m�s f�cil para el usuario

url3 <- URL2 # cargue el URL del XML
data_df <- xmlToDataFrame(url3)
head(data_df)
# Datos obtenidos de: https://datos.gob.mx/busca/dataset/saldo-de-bonos-de-proteccion-al-ahorro-bpas


# EJEMPLO 7 ----

# Ahora vamos a considerar el conjunto de datos airquality, observamos primero 
# algunas de sus filas

head(airquality)
library(dplyr)
# El tipo de objeto que es y el tipo de variables que contiene

str(airquality)
# observamos la dimensi�n

dim(airquality)

# Con la funci�n complete.cases podemos averiguar cuales son aquellas filas que 
# no contienen ning�n valor perdido (NA) y cuales son aquellas filas que tienen 
# al menos un valor perdido.

bien <- complete.cases(airquality)
# La variable bien, es un vector l�gico con TRUE en las posiciones que 
# representan filas de airquality en donde no hay NA's y con FALSE en las 
# posiciones que representan aquellas filas de airquality en donde se encontraron NA's

# Por tanto, podemos contar el n�mero de filas en donde no hay NA�s de la 
# siguiente manera

sum(bien)
# Podemos filtrar aquellas filas sin NA's de la siguiente manera

airquality[bien,]
data <- select(airquality, Ozone:Temp)
apply(data, 2, mean)
apply(data, 2, mean, na.rm = T)
# na.omit devuelve el objeto con casos incompletos eliminados

(m1 <- apply(na.omit(data), 2, mean))

b <- complete.cases(data)

(m2 <- apply(data[b,], 2, mean))

identical(m1, m2)















