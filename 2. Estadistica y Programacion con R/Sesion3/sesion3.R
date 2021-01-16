###########################################################-
# Objective: Sesion 3
# Author: Gabriel Sainz V�zquez
# Date Modified: 13/01/2021
###########################################################-


# EJEMPLO 1: Paquete ggplot2----

# Comenzando con gráficos simples; vamos a utilizar el dataset mtcars.
# Instalamos el paquete (si es necesario) y lo cargamos

library(ggplot2)
# Primero recordamos cuales son las variables que contiene el dataset

names(mtcars)
str(mtcars)
# Graficamos las variables cyl en el eje x y hp en y, observa el comando ➡️ 
# geom_point()

ggplot(mtcars, aes(x=cyl, y = hp, colour = mpg )) + 
      geom_point()  # Tipo de geometría, intenta utilizar alguna otra

# Agregando carácteristicas de tema y facewrap

names(mtcars)
ggplot(mtcars, aes(x=cyl, y = hp, colour = mpg )) + 
      geom_point() +   
      theme_gray() +   # Temas (inteta cambiarlo)
      facet_wrap("cyl")  # Lo divide por el núm de cilindros

# Agregando nombres a los ejes x, y

names(mtcars)
ggplot(mtcars, aes(x=cyl, y = hp, colour = mpg )) + 
      geom_point() +   
      theme_gray() +   # Temas (inteta cambiarlo)
      facet_wrap("cyl") +  # Lo divide por el núm de cilindros
      xlab('Núm Acilindros')+  # Nombre en los ejes
      ylab('Caballos de Fuerza')

# Adicionalmente se pueden realizar otros tipos de gráficos, estos se verán en 
# los próximos ejemplos.

# EJEMPLO 2: Histogramas----

data2 <- read.csv("C:\\Users\\Gabo\\Desktop\\Becas Santander- Data Science\\2.Fase2\\2. Estadística y Programación con R\\Sesion3/Data/boxp.csv")
head(data2)
names(data2)
library(dplyr)

data <- mutate(data2, Mediciones = Mediciones*1.23)

# Utilizando la función hist

hist(data$Mediciones, breaks = seq(0,360, 20), 
     main = "Histograma de Mediciones",
     xlab = "Mediciones",
     ylab = "Frecuencia")

# Ahora utilizando ggplot para apreciar los resultados de las dos funciones

# Evitar el Warning de filas con NA´s

data <- na.omit(data) 

data %>%
      ggplot() + 
      aes(Mediciones) +
      geom_histogram(binwidth = 10)

# Agregando algunas etiquetas y tema, intenta modificar algunas de las opciones 
# para que aprecies los resultados

data %>%
      ggplot() + 
      aes(Mediciones) +
      geom_histogram(binwidth = 10, col="black", fill = "blue") + 
      ggtitle("Histograma de Mediciones") +
      ylab("Frecuencia") +
      xlab("Mediciones") + 
      theme_light()

# Nota: para realizar un histograma y parecerse a la distribución, hágase lo 
# siguiente

data %>%
   ggplot() + 
   aes(x = Mediciones) +
   geom_histogram(binwidth = 10,aes(y=..density..),col="black", fill = "blue") + 
   ggtitle("Histograma de Mediciones") +
   ylab("Densidad") +
   xlab("Mediciones") + 
   geom_density(aes(x = Mediciones, y = ..density..), col = "red") + 
   theme_light()

# Tanto hist(), como ggplot() + aes() + geom_histogram() son útiles para generar 
# los histogramas, tu decide cual te funciona mejor.

# RETO 1: ALtura de los alumnos ----

# Este ejemplo aunque es básico, servirá para realizar una comparación entre 
# las funciones hist() y ggplot, los dos útiles, comparalos y decide cual es de 
# tu agrado de acuerdo a los requerimientos.
url <- "https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-03/Reto-01/BD_Altura_Alunos.csv"

setwd("C:\\Users\\Gabo\\Desktop\\Becas Santander- Data Science\\2.Fase2\\2. Estadística y Programación con R\\Sesion3/Data")

# Carga el data set BD_Altura_Alunos.csv
# Realiza el histograma con la función hist(), nativa de R
download.file(url = url, destfile = "BD_Altura_Alunos.csv", mode = "wb")
data <- read.csv("BD_Altura_Alunos.csv", sep = ";")
head(data)

# Ahora realiza el histograma con la función ggplot. (Recuerda que debes 
# instalar el paquete ggplot2)
# Reflexiona sobre el ejercicio y saca tus conclusiones sobre cuál es el método 
# que más te convence.

data %>% ggplot() + 
   aes(x = Altura) + 
   geom_histogram(binwidth = 4, col="black", fill = "blue") + 
   ggtitle("Histograma de Mediciones") +
   ylab("Frecuencia") +
   xlab("Alturas") + 
   theme_light()

# Histograma (Densidad)

data %>% ggplot() + 
   aes(x = Altura) + 
   geom_histogram(binwidth = 4,aes(y = ..density..), col="black", fill = "blue") + 
   ggtitle("Histograma de Mediciones") +
   ylab("Frecuencia") +
   xlab("Alturas") + 
   geom_density(aes(x = Altura, y = ..density..), col = "red") + 
   theme_light()

hist(data$Altura, 
     breaks = 20,
     main = " Histograma de alturas",
     ylab = "Frecuencia",
     xlab = "Altura", 
     col = "blue")

# EJEMPLO 3: Gr�ficos de dispersi�n (scatter plots) ----

# Realizamos un scatter plot de las variables wt y mpg, debemos utilizar 
# necesariamente geom_point()

head(mtcars)
(my_scatplot <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point())

# Adicionalmente se puede agregar una línea de tendencia

(my_scatplot <- ggplot(mtcars, aes(x = wt, y = mpg)) + 
            geom_point() + 
            geom_smooth(method = "lm", se = T))  # modelo lineal, cambia el 
# parametro `se`, este hace referencia al intervalo de confianza

ggplot(mtcars, aes(x = wt, y = mpg)) + 
   geom_point() + 
   geom_smooth(method = "lm", se = F)  # modelo lineal, cambia el 


# Agregando los nombres de los ejes, observa que se almacenó el gráfico en el 
# objeto my_scatplot (nota que pueden agregarse más características seguido del 
# signo +)

my_scatplot + xlab('Weight (x 1000lbs)') + ylab('Miles per Gallon')

# Otras características interesantes

(my_scatplot <- ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) + geom_point())
my_scatplot + labs(x = 'Weight (x1000lbs)', y = 'Miles per Gallon', colour = 'Number of\n Cylinders')

# Haciendo un facewrap con la variable cyl

my_scatplot + facet_wrap("cyl")
# Separándolas por tipo de transmisión (am = Transmission (0 = automatic, 
# 1 = manual))

my_scatplot + facet_grid(am~cyl)
# Como puedes observar, hay muchas formas de representar el gráfico de 
# dispersión, éstas son algunas de ellas, obviamente existen muchas más.

# RETO 2: Estad�sticos de la NBA----

data <- read.csv("players_stats.csv")
head(data)

# Generar un histograma de los minuntos totales (MIN), de los jugadores y agregar 
# una línea donde se muestre la media

m <- round(mean(data$MIN), 2)

#Densidad

ggplot(data, aes(x = MIN)) + 
   geom_histogram(binwidth = 100, aes(y = ..density..), fill = "blue", col = "black") +
   ggtitle("Histograma de los minutos totales", 
           subtitle = paste("Media", m)) + 
   geom_density(aes(y = ..density..), col = "red")+
   geom_vline(xintercept = m, col = "red", lwd = 1.5, lty =2) + 
   xlab("Minutos") + 
   ylab("Densidad") + 
   theme_light()

data %>% ggplot() + 
      aes(MIN) + 
      geom_histogram(binwidth = 100, col="black", fill = "blue") + 
      ggtitle("Histograma de MIN",paste("Media=",mean(data$MIN))) +
      ylab("Frecuencia") +
      xlab("Minutos") + 
      geom_vline(xintercept = mean(data$MIN), col = "red", lwd = 1.5, lty =2) + 
      theme_light()


# Generar un histograma de edad (Age) y agregar una línea con la media

m1 <- round(mean(na.omit(data$Age)), 2)

#Densidad

ggplot(data, aes(x = Age)) + 
   geom_histogram(binwidth = 1, aes(y = ..density..), fill = "blue", col = "black") +
   ggtitle("Histograma de edad", 
           subtitle = paste("Media", m1)) + 
   geom_density(aes(y = ..density..), col = "red")+
   geom_vline(xintercept = m1, col = "red", lwd = 1.5, lty =2) + 
   xlab("Edad") + 
   ylab("Densidad") + 
   theme_light()

data %>% ggplot() + 
      aes(Age) + 
      geom_histogram(binwidth = 1, col="black", fill = "blue") + 
      ggtitle("Histograma de Edad",paste("Media=",mean(na.omit(data$Age)))) +
      ylab("Frecuencia") +
      xlab("Edad") + 
      geom_vline(xintercept = mean(na.omit(data$Age)), col = "red", lwd = 1.5, lty =2) + 
      theme_light()

# Hacer un scatterplot de las variables Weight y Height y observar la correlacón 
# que existe entre ambas variables (1 sola gráfica)

data1 <- na.omit(data$Weight)

c <- cor(na.omit(data$Weight), na.omit(data$Height))

p <- data %>% ggplot() + 
      aes(x = Weight, y = Height) + 
      geom_point() + geom_smooth(method = "lm", se = T) + 
   ggtitle("Weight vs Height", paste("Correlación:", c))

(lmnba <- coef(lm(Height ~ Weight, data = data)))

p + geom_abline(intercept = lmnba[1], slope = lmnba[2], col = "red", lwd = 1.5, lty = 2)


# Utiliza la función which.max para saber quién es el jugador más alto, una vez 
# hecho esto, presenta los resultados en una leyenda que diga "El jugador más 
# alto es: Name, con una altura de: Height". Las unidades de altura deben ser en
# metros.

?which.max
which.max(data$Height) # te devuelve la posición

paste("El jugador más alto es:", data$Name[which.max(data$Height)], 
      ", con una altura de: ", data$Height[which.max(data$Height)])

# Utiliza la función which.min para saber quién es el jugador más bajito, una vez 
# hecho esto, presenta los resultados en una leyenda que diga "El jugador más 
# bajito es: Name, con una altura de: Height". Las unidades de altura deben ser en
# metros.
paste("El jugador más bajito es:", data$Name[which.min(data$Height)], 
      ", con una altura de: ", data$Height[which.min(data$Height)])

# ¿Cuál es la altura promedio?, representa el resultado en una frase que diga: 
# "La altura promedio es: ALTURA"

paste("La altura promedio es: ", mean(na.omit(data$Height)))


# Generar un scatterplot donde se representen las Asistencias totales (AST.TOV) 
# vs Puntos (PTS), además has un face wrap con la posición (Pos).

data %>% ggplot( aes(AST.TOV, PTS )) +
      geom_point() + 
      facet_wrap("Pos")


# EJEMPLO 4: Boxplots y outliers ----

# Comenzamos leyendo un fichero, el cual contiene información sobre dos grupos 
# de control G1 y G2, a los cuales se les realizó a cada uno una medición en 3 
# momentos diferentes C1, C2 y C3

# Cargamos las librerias necesarias para la realización del ejemplo, además del 
# fichero CVS a utilizar

library(ggplot2)
library(dplyr)

data2 <- read.csv("C:\\Users\\Gabo\\Desktop\\Becas Santander- Data Science\\2.Fase2\\2. Estadística y Programación con R\\Sesion3/Data/boxp.csv")

# Revisamos el encabezado del fichero y el nombre de sus variables o columnas

head(data2)
names(data2)
# Vamos a realizar un cambio en la variable Mediciones para practicar

data <- mutate(data2, Mediciones = Mediciones*1.23)
head(data)
# Observamos algunos datos estadísticos sobre las variables

summary(data)

# Como estamos ante la presencia de NA´s los eliminamos con complete.cases() y 
# solamente seleccionamos aquellos sin NAs y convertimos en factores la 
# variable Categoria y Grupo`

bien <- complete.cases(data)
data <- data[bien,] # filtramos filas que están completas
data <- mutate(data, Categoria = factor(Categoria), Grupo = factor(Grupo))
head(data)
str(data)

# Finalmente realizamos el boxplot

ggplot(data, aes(x = Categoria, y = Mediciones, fill = Grupo)) + geom_boxplot() +
      ggtitle("Boxplots") +
      xlab("Categorias") +
      ylab("Mediciones")

# Agregamos el nombre de las etiquetas para los grupos G1 y G2

ggplot(data, aes(x = Categoria, y = Mediciones, fill = Grupo)) + geom_boxplot() +
      scale_fill_discrete(name = "Dos Gps", labels = c("G1", "G2")) + 
      ggtitle("Boxplots") +
      xlab("Categorias") +
      ylab("Mediciones")


# EJEMPLO 5. Diversos gr�ficos de tendencias: COVID-19 -----

# Al inicio es posible que no comprendas todo el código, trata de leerlo e ir 
# asimilando que es lo que realiza cada línea. Comenzaremos descargando los 
# archivos CSV, con los últimos resultados de los datos de la enfermedad COVID-19

library(dplyr)

setwd("C:\\Users\\Gabo\\Desktop\\Becas Santander- Data Science\\2.Fase2\\2. Estad�stica y Programaci�n con R\\Sesion3\\Data")

url1 <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_confirmed_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv"
url2 <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_deaths_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv"
# Una vez que se leyeron los URL´s se procede a la descarga de los archivos:

download.file(url = url1, destfile = "st19ncov-confirmados.csv", mode = "wb")
download.file(url = url2, destfile = "st19ncov-muertes.csv", mode = "wb")

# También se puede hacer la lectura directamente desde el URL si asi se prefiere
# (ten en cuenta que es un poco más lento debido al tamaño de los archivos)

# conf <- read.csv(url1)
# mu <- read.csv(url2)

conf <- read.csv("st19ncov-confirmados.csv")
mu <- read.csv("st19ncov-muertes.csv")
head(conf)

# Eliminamos la primer fila

Sconf <- conf[-1, ]
Smu <- mu[-1, ]

Sconf <- select(Sconf, Country.Region, Date, Value) # País, 

Sconf <- rename(Sconf, Country = Country.Region, Infectados = Value) # Cambiamos el nombre de las variables

Sconf <- mutate(Sconf, Infectados = as.numeric(as.character(Infectados)), 
                Date = as.Date(Date, "%Y-%m-%d")) #Transformamos la variable

# Seleccionamos país, fecha y acumulado de muertos

Smu <- select(Smu, Country.Region, Date, Value) 
Smu <- rename(Smu, Country = Country.Region, Muertos = Value) # Renombramos
Smu <- mutate(Smu, Date = as.Date(Date, "%Y-%m-%d"), 
              Muertos = as.numeric(as.character(Muertos))) # Transformamos

# Unimos infectados y muertos acumulados para cada fecha

Scm <- merge(Sconf, Smu)  

mex <- filter(Scm, Country == "Mexico") # Seleccionamos sólo a México

mex <- filter(mex, Infectados != 0) # Comienzan los infectados en México
mex <- mutate(mex, NI = c(1, diff(Infectados))) # Nuevos infectados por día
mex <- mutate(mex, NM = c(0, diff(Muertos))) # Nuevos muertos por día

mex <- mutate(mex, Letalidad = round(Muertos/Infectados*100, 1)) # Tasa de letalidad

mex <- mutate(mex, IDA = lag(Infectados), MDA = lag(Muertos)) # Valores día anterior
mex <- mutate(mex, FCI = Infectados/IDA, FCM = Muertos/MDA) # Factores de Crecimiento
mex <- mutate(mex, Dia = 1:dim(mex)[1]) # Días de contingencia


setwd("C:\\Users\\Gabo\\Desktop\\Becas Santander- Data Science\\2.Fase2\\2. Estad�stica y Programaci�n con R\\Sesion3\\Covid")  # Fijando el wd (depende del usuario)
# Escribimos los resultados de la variable mex, en el archivo C19Mexico.csv

write.csv(mex, "C19Mexico.csv", row.names = F) # La ruta depende del usuario
dir()  # observemos que se creo en la ruta deseada

library(dplyr)
library(ggplot2)
library(scales)

# Ahora vamos a leer nuestro archivo con los resultados de la variable mex con 
# los infectados y muertos acumulados para cada fecha (Los pasos anteriores 
# fueron solo para recordar, ya que el ejemplo se realizó durante la sesión 2)

mex <- read.csv("C19Mexico.csv")

head(mex); tail(mex)

mex <- mutate(mex, Date = as.Date(Date, "%Y-%m-%d"))
str(mex)

# A continuación, te presentamos un panorama de la situación que se ha estado 
# viviendo en México, debido al coronavirus. Es información simple, que puede 
# resultar valiosa para algunas personas. Las gráficas, las hemos realizado 
# utilizando datos que puedes encontrar en el siguiente sitio: 
# https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases

# Acumulado de Casos Confirmados
p <- ggplot(mex, aes(x=Date, y=Infectados)) + 
      geom_line(color="blue") + 
      geom_point() +
      labs(x = "Fecha", 
           y = "Acumulado de casos confirmados",
           title = paste("Confirmados de COVID-19 en M�xico:", 
                         format(Sys.time(), 
                                tz="America/Mexico_City", 
                                usetz=TRUE))) +
      theme(plot.title = element_text(size=12))  +
      theme(axis.text.x = element_text(face = "bold", color="#993333" , 
                                       size = 10, angle = 45, 
                                       hjust = 1),
            axis.text.y = element_text(face = "bold", color="#993333" , 
                                       size = 10, angle = 45, 
                                       hjust = 1))  # color, �ngulo y estilo de las abcisas y ordenadas 

p <- p  + scale_x_date(labels = date_format("%d-%m-%Y")) # paquete scales

###

p <- p +
      theme(plot.margin=margin(10,10,20,10), plot.caption=element_text(hjust=1.05, size=10)) +
      annotate("text", x = mex$Date[round(dim(mex)[1]*0.4)], 
               y = max(mex$Infectados), colour = "blue", size = 6, 
               label = paste("�ltima actualizaci�n: ", mex$Infectados[dim(mex)[1]]))
p

# Casos Confirmados por D�a

p <- ggplot(mex, aes(x=Date, y=NI)) + 
   geom_point(col = "blue") + 
      geom_line(stat = "identity") + 
   geom_smooth(method = "loess", se = F) +
      labs(x = "Fecha", y = "Incidencia (N�mero de casos nuevos)",
           title = paste("Casos de Incidencia de COVID-19 en M�xico:", 
                         format(Sys.time(), 
                                tz="America/Mexico_City", usetz=TRUE))) +
      theme(plot.title = element_text(size=12))  +
      theme(axis.text.x = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1),
            axis.text.y = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1))  # color, Ángulo y estilo de las abcisas y ordenadas

p <- p  + scale_x_date(labels = date_format("%d-%m-%Y")) # paquete scales
p

###

p <- p +
      theme(plot.margin=margin(10,10,20,10), plot.caption=element_text(hjust=1.05, size=10)) +
      annotate("text", x = mex$Date[round(dim(mex)[1]*0.4)], y = max(mex$NI), colour = "blue", size = 6, 
               label = paste("�ltima actualizaci�n: ", mex$NI[length(mex$NI)]))
p

# Muertes Acumuladas

mexm <- subset(mex, Muertos > 0) # Tomamos el subconjunto desde que comenzaron las muertes

p <- ggplot(mexm, aes(x=Date, y=Muertos)) + geom_line( color="red") + 
      geom_point() +
      labs(x = "Fecha", 
           y = "Muertes acumuladas",
           title = paste("Muertes por COVID-19 en M�xico:", format(Sys.time(), tz="America/Mexico_City",usetz=TRUE))) +
      theme(axis.text.x = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1),
            axis.text.y = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1))  # color, Ángulo y estilo de las abcisas y ordenadas

p <- p  + scale_x_date(labels = date_format("%d-%m-%Y"))

p

###

p <- p +
      theme(plot.margin=margin(10,10,20,10), plot.caption=element_text(hjust=1.05, size=10)) +
      annotate("text", x = mexm$Date[round(dim(mexm)[1]*0.4)], 
               y = max(mexm$Muertos), colour = "red", size = 5, label = paste("�ltima actualizaci�n: ", mexm$Muertos[dim(mexm)[1]]))
p

# Muertes por Día

p <- ggplot(mexm, aes(x=Date, y=NM)) + 
   geom_point(col = "blue") + 
      geom_line(stat = "identity") + 
   geom_smooth(method = "loess", se = F) +
      labs(x = "Fecha", y = "N�mero de nuevos decesos",
           title = paste("Nuevos decesos por COVID-19 en M�xico:", 
                         format(Sys.time(), tz="America/Mexico_City",usetz=TRUE))) +
      theme(plot.title = element_text(size=12)) +
      theme(axis.text.x = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1),
            axis.text.y = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1))  # color, Ángulo y estilo de las abcisas y ordenadas

p <- p  + scale_x_date(labels = date_format("%d-%m-%Y")) 
###

p <- p +
      theme(plot.margin=margin(10,10,20,10), plot.caption=element_text(hjust=1.05, size=10)) +
      annotate("text", x = mexm$Date[round(dim(mexm)[1]*0.2)], 
               y = max(mexm$NM), colour = "red", size = 5, label = paste("�ltima actualizaci�n: ", mexm$NM[dim(mexm)[1]]))
p

# Acumulado de Casos Confirmados y Muertes

p <- ggplot(mex, aes(x=Date, y=Infectados)) + geom_line(color="blue") + 
      labs(x = "Fecha", 
           y = "Acumulado de casos",
           title = paste("COVID-19 en M�xico:", format(Sys.time(), tz="America/Mexico_City",usetz=TRUE))) +
      geom_line(aes(y = Muertos), color = "red") +
      theme(axis.text.x = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1),
            axis.text.y = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1))  # color, Ángulo y estilo de las abcisas y ordenadas

p <- p  + scale_x_date(labels = date_format("%d-%m-%Y"))

###

p <- p +
      theme(plot.margin=margin(10,10,20,10), plot.caption=element_text(hjust=1.05, size=10)) +
      annotate("text", x = mex$Date[round(dim(mex)[1]*0.4)], 
               y = max(mex$Infectados), colour = "blue", size = 5, label = paste("�ltima actualizaci�n para Infectados:", mex$Infectados[dim(mex)[1]])) +
      annotate("text", x = mex$Date[round(dim(mex)[1]*0.4)], 
               y = max(mex$Infectados)-100000, colour = "red", size = 5, label = paste("�ltima actualizaci�n para Muertes:", mex$Muertos[dim(mex)[1]])) 
p

# Tasa de Letalidad: La tasa de letalidad observada para un día determinado, la 
# calculamos dividiendo las muertes acumuladas reportadas hasta ese día, entre 
# el acumulado de casos confirmados para el mismo día. Multiplicamos el 
# resultado por 100 para reportarlo en forma de porcentaje. Lo que obtenemos es 
# el porcentaje de muertes del total de casos confirmados.

p <- ggplot(mexm, aes(x=Date, y=Letalidad)) + geom_line(color="red") + 
      labs(x = "Fecha", 
           y = "Tasa de letalidad",
           title = paste("COVID-19 en M�xico:", format(Sys.time(), tz="America/Mexico_City",usetz=TRUE))) +
      theme(axis.text.x = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1),
            axis.text.y = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1)) + # color, Ángulo y estilo de las abcisas y ordenadas 
      scale_y_discrete(name ="Tasa de letalidad", 
                       limits=factor(seq(1, 13.5, 1)), labels=paste(seq(1, 13.5, 1), "%", sep = ""))

p <- p  + scale_x_date(labels = date_format("%d-%m-%Y"))

###

p <- p +
      theme(plot.margin=margin(10,10,20,10), plot.caption=element_text(hjust=1.05, size=10)) +
      annotate("text", x = mexm$Date[round(length(mexm$Date)*0.2)], 
               y = max(mexm$Letalidad)-1, colour = "red", size = 4, label = paste("�ltima actualizaci�n: ", mexm$Letalidad[dim(mexm)[1]], "%", sep = "")) 
p

# Factores de Crecimiento:
# El factor de crecimiento de infectados para un día determinado, lo calculamos 
# al dividir el acumulado de infectados para ese día, entre el acumulado de 
# infectados del día anterior. El factor de crecimiento de muertes lo calculamos 
# de forma similar.

mex <- filter(mex, FCM < Inf) # Tomamos solo valores reales de factores de crecimiento

p <- ggplot(mex, aes(x=Date, y=FCI)) + geom_line(color="blue") + 
      labs(x = "Fecha", 
           y = "Factor de crecimiento",
           title = paste("COVID-19 en México:", format(Sys.time(), tz="America/Mexico_City",usetz=TRUE))) +
      geom_line(aes(y = FCM), color = "red") + theme(plot.title = element_text(size=12)) +
      theme(axis.text.x = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1),
            axis.text.y = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1))  # color, Ángulo y estilo de las abcisas y ordenadas

p <- p  + scale_x_date(labels = date_format("%d-%m-%Y"))

###

p <- p +
      annotate("text", x = mex$Date[round(length(mex$Date)*0.4)], y = max(mex$FCM), colour = "blue", size = 5, label = paste("�ltima actualizaci�n para infectados: ", round(mex$FCI[dim(mex)[1]], 4))) +
      annotate("text", x = mex$Date[round(length(mex$Date)*0.4)], y = max(mex$FCM)-0.2, colour = "red", size = 5, label = paste("�ltima actualizaci�n para muertes: ", round(mex$FCM[dim(mex)[1]], 4))) 
p


# Ejemplo 6. Series de tiempo y descomposici�n ----

# Utilizaremos un data set de Kaggle el cual contiene datos sobre la temperatura 
# en Fortaleza, Brasil. Lo primero que se hará será ajustar los datos para poder 
# leerlos adecuadamente (Este ejemplo es principalmente para conocer las funciones
# de R que nos ayudan a trabajar con series de tiempo).

library(dplyr)    

setwd("C:\\Users\\Gabo\\Desktop\\Becas Santander- Data Science\\2.Fase2\\2. Estad�stica y Programaci�n con R\\Sesion3\\Data")  # Fijando el wd (depende del usuario)

w.brazil <- read.csv("station_fortaleza.csv")
tail(w.brazil)

w.brazil <- w.brazil[, -c(1,14:18)]
tail(w.brazil)
class(w.brazil)

plot(w.brazil)
# Quitamos los varoles que sean mayores a 50

w.brazil <- w.brazil %>% filter(JAN<50,FEB<50,	MAR<50,	APR<50,	MAY<50,	JUN<50,	JUL<50,	AUG<50,	SEP<50,	OCT<50,	NOV<50,	DEC<50)
plot(w.brazil)

bras <- apply(w.brazil, 2, c)
class(bras)
bra1 <-  as.vector(t(bras))
# Convertimos los datos en serie de tiempo con el comando ts

tsb <- ts(bra1, start = c(1946,01), frequency = 12)
class(tsb)
summary(tsb)

start(tsb); end(tsb); frequency(tsb)  # Inicio, fin y frecuencia de la serie
# Graficamos la serie de tiempo

plot(tsb, main = "Serie de tiempo", ylab = "Temp", xlab = "Año")
# Descomposición aditiva de la serie de tiempo

tsbd <- decompose(tsb, type = "additive")

plot(tsbd)

plot(tsbd$trend)  # Gráfica  de la tendencia 
plot(tsbd$seasonal) # Gráfica  de la temporalidad

# Realizamos la gráfica de la descomposición aditiva con la tendencia y la estacionalidad utilizando el comando lines

plot(tsbd$trend , main  = "Aditiva", ylab = "Tendencia", xlab = "A�o")
lines(tsbd$seasonal + tsbd$trend , col = 2, lty = 2, lwd = 2 )

# Descomposición multiplicativa

# Debemos elegir entre descomposición aditiva o multiplicativa
tsbd <- decompose(tsb, type = "multiplicative")
plot(tsbd)

plot(tsbd$trend, main = "Tendencia", ylab = "Tendencia", xlab = "Año")  # Gráfica de la tendencia 
plot(tsbd$seasonal, main = "Estacionalidad", ylab = "Tendencia", xlab = "Año") # Gráfica de la estacionalidad

# Realizamos la gráfica de la descomposición aditiva con la tendencia y la estacionalidad utilizando el comando lines

plot(tsbd$trend , main  = "Multiplicativa", ylab = "Tendencia", xlab = "Año")
lines(tsbd$seasonal * tsbd$trend, col = 2, lty = 2, lwd = 2 )

# Comportamiento mes a mes

boxplot(tsb ~ cycle(tsb), ylim = c(min(tsb), max(tsb) ) )

library(TSA)

plot(AirPassengers)
d <- decompose(AirPassengers, type = "additive")

lines(d$trend, col = 6)
lines(d$trend + d$seasonal, col = 2, lty = 2)

plot(AirPassengers)
d1 <- decompose(AirPassengers, type = "multiplicative")
lines(d1$trend, col = 4)
lines(d1$trend * d1$seasonal, col = 3, lty = 2)

plot(d1$seasonal)
plot(d1)
# RETO 3: Series de tiempo ----
# Utiliza los datos (AP), de la siguiente librería para realizar lo que se te pide
library(forecast)

data(AirPassengers)
AP <- AirPassengers
# Realiza la gráfica de la serie de tiempo
plot(AirPassengers)

# Descompón la serie de tiempo en aditiva y multiplicativa y realiza sus gráficas
d <- decompose(AP, type = "additive")
d2 <- decompose(AP, type = "multiplicative")

# Realiza la gráfica de la descomposición aditiva con la tendencia y la 
# estacionalidad utilizando el comando lines
plot(AirPassengers)
lines(d$trend,  col = 4)
lines(d$trend + d$seasonal, col = 3, lty = 2)

# Realiza la gráfica de la descomposición multiplicativa con la tendencia y la 
# estacionalidad utilizando el comando lines
plot(AirPassengers)
lines(d2$trend,  col = 4)
lines(d2$trend * d2$seasonal, col = 3, lty = 2)











