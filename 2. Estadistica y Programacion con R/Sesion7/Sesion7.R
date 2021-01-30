###########################################################-
# Objective: Sesion 7: RStudio Cloud - Github, conexiones con BDs y lectura de datos externos
# Author: Gabriel Sainz Vázquez
# Date Modified: 27/01/2021
###########################################################-



# Ejemplo 2. Conexión a una BDD con R  ----

# CONFIGURACIONES DE CONEXIÓN: Hay 5 configuraciones necesarias para hacer una 
# conexión:
      
# - Driver : consulta la sección previa de controladores para obtener información 
#   sobre la configuración, se utilizarán los drivers de MySQL
# - Server : una ruta de red al servidor de la base de datos
# - UID : nombre de usuario utilizado para acceder al servidor MySQL
# - PWD : la contraseña correspondiente al UID proporcionado
# - Port : debe establecerse en 3306 generalmente

# Comenzaremos instalando las librerías necesarias para realizar la conexión y 
# lectura de la base de datos en RStudio, si previamente los tenías instalados 
# omite la instalación, recuerda que solo necesitas realizarla una vez.

# install.packages("DBI")
# install.packages("RMySQL")

library(DBI)
library(RMySQL)

# Una vez que se tengan las librerías necesarias se procede a la lectura (podría
# ser que necesites otras, si te las solicita instalalas y cargalas), de la base
# de datos de Shiny la cual es un demo y nos permite interactuar con este tipo 
# de objetos. El comando dbConnect es el indicado para realizar la lectura, los 
 #demás parámetros son los que nos dan acceso a la BDD.

MyDataBase <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "shinydemo",
      host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
      username = "guest",
      password = "guest")

# Si no se arrojaron errores por parte de R, vamos a explorar la BDD

dbListTables(MyDataBase)

# Ahora si se quieren desplegar los campos o variables que contiene la tabla 
# City se hará lo siguiente

dbListFields(MyDataBase, 'City')

# Para realizar una consulta tipo MySQL sobre la tabla seleccionada haremos lo 
# siguiente

DataDB <- dbGetQuery(MyDataBase, "select * from City")

# Observemos que el objeto DataDB es un data frame, por lo tanto ya es un objeto 
# de R y podemos aplicar los comandos usuales

class(DataDB)
dim(DataDB)
head(DataDB)

pop.mean <- mean(DataDB$Population)  # Media a la variable de población
pop.mean 

pop.3 <- pop.mean *3   # Operaciones aritméticas
pop.3

# Incluso podemos hacer uso de otros comandos de búsqueda aplicando la librería 
# dplyr

library(dplyr)
pop50.mex <-  DataDB %>% filter(CountryCode == "MEX" ,  Population > 50000)   # Ciudades del país de México con más de 50,000 habitantes

head(pop50.mex)

unique(DataDB$CountryCode)   # Países que contiene la BDD


# Reto 1. RStudio Cloud -> Github ---- 

# - Crea un repositorio en Github llamado Reto_Sesion_7

# - Crea un Project llamado Reto_Sesion_07 dentro de RStudio utilizando tu cuenta
#   de RStudio, que esté ligado al repositorio recién creado

# - Ahora en RStudio crea un script llamado queries.Ren donde se conecte a la 
#   BDD shinydemo

# - Una vez hecha la conexión a la BDD, generar una busqueda con dplyr que
#   devuelva el porcentaje de personas que hablan español en todos los países

# - Realizar una gráfica con ggplot que represente este porcentaje de tal modo 
#    que en el eje de las Y aparezca el país y en X el porcentaje, y que 
#    diferencíe entre aquellos que es su lengua oficial y los que no con diferente color (puedes utilizar la geom_bin2d() y coord_flip())
 
# -Una vez hecho esto hacer el commit y push para mandar tu archivo al 
#   repositorio de Github Reto_Sesion_7

# Ejemplo 3. Variantes en la lectura de BDD con R ----

# Ahora utilizaremos otra opción para realizar queries a una BDD con la ayuda de 
# dplyr que sustituye a SELECT en MySQL y el operador %>%, hay que recordar que 
# con este comando también podemos realizar búsquedas de forma local.

# Comenzamos instalando las paqueterías necesarias y cargándolas a R

# install.packages("pool")
# install.packages("dbplyr")

library(dbplyr)
library(pool)

# Se realiza la lectura de la BDD con el comando dbPool, los demás parámetros 
# se siguen utilizando igual que el ejemplo anterior

my_db <- dbPool(
      RMySQL::MySQL(), 
      dbname = "shinydemo",
      host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
      username = "guest",
      password = "guest"
)

# Para ver el contenido de la BDD y realizar una búsqueda se procede de la 
# siguiente manera

dbListTables(my_db)

# Obtener los primeros 5 registros de Country

my_db %>% tbl("Country") %>% head(5) # library(dplyr)

# Obtener los primeros 5 registros de CountryLanguage

my_db %>% tbl("CountryLanguage") %>% head(5)

# Otra forma de generar una búsqueda será con la librería DBI, utilizando el comando dbSendQuery

library(DBI)
conn <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "shinydemo",
      host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
      username = "guest",
      password = "guest")

rs <- dbSendQuery(conn, "SELECT * FROM City LIMIT 5;")

dbFetch(rs)

# Para finalizar nos desconectamos de la BDD

dbClearResult(rs)
dbDisconnect(conn)

# Ejemplo 4. Lectura de archivos JSON, XML y tablas en HTML ---- 

# Comenzaremos instalando los paquetes necesarios para después cargarlos a R

# install.packages("rjson")   #Siempre usar comillas en el nombre del paquete

library(rjson)            # Quitar comillas del nombre

# Json
# Vamos a leer un archivo Json de prueba alojado aquí

URL <- "https://tools.learningcontainer.com/sample-json-file.json" # Asignando el link a una variable

JsonData <- fromJSON(file= URL)     # Se guarda el JSon en un objeto de R

class(JsonData)                     # Vemos que tipo de objeto es JsonData

str(JsonData)                       # Vemos la naturaleza de sus variables

# Finalmente ya que pudimos acceder al contenido del Json, también podemos 
# realizar la manipulación de los datos dentro del Json, por ejemplo:
      
sqrt(JsonData$Mobile)

# Para entrar a las demás variables recuerda que puedas usar el operador de $, 
# es decir, JsonData$
      
# XML
# Ahora vamos a leer datos XML en R, utilizando un archivo XML alojado aquí

# Lo primero es instalar y cargar el paquete XML y alojar el link en una 
# variable link, para su lectura

# install.packages("XML")
library(XML)
link <- "http://www-db.deis.unibo.it/courses/TW/DOCS/w3schools/xml/cd_catalog.xml"

# Analizando el XML desde la web
xmlfile <- xmlTreeParse(link)

# Ahora ya podemos ver las propiedades del objetvo xmlfile

summary(xmlfile)
head(xmlfile)

# También gracias al xmlTreeParse podemos extraer los datos contenidos en el archivo

#Extraer los valores xml
topxml <- xmlSApply(xmlfile, function(x) xmlSApply(x, xmlValue))

# Colocandolos en un Data Frame
xml_df <- data.frame(t(topxml), row.names= NULL)

str(xml_df) # Observar la naturaleza de las variables del DF

# Convertiremos incluso las variables de PRICE y YEAR en datos numéricos para 
# poder realizar operaciones con este dato

xml_df$PRICE <- as.numeric(xml_df$PRICE) 
xml_df$YEAR <- as.numeric(xml_df$YEAR)

mean(xml_df$PRICE)
mean(xml_df$YEAR)

# Todo esto se puede realizar en un solo paso utilizando el siguiente comando

data_df <- xmlToDataFrame(link)
head(data_df)

# Tablas en HTML
# Comenzamos instalando el paquete rvest el cual nos permitirá realizar la 
# lectura de la tabla en el HTML

# install.packages("rvest")
library(rvest)
 # Introducimos una dirección URL donde se encuentre una tabla

theurl <- "https://solarviews.com/span/data2.htm"
file <- read_html(theurl)    # Leemos el html

# Selecciona pedazos dentro del HTML para identificar la tabla

tables <- html_nodes(file, "table")  

# Hay que analizar 'tables' para determinar cual es la posición en la lista que 
# contiene la tabla, en este caso es la no. 4

# Extraemos la tabla de acuerdo a la posición en la lista

table1 <- html_table(tables[4], fill = TRUE)

table <- na.omit(as.data.frame(table1))   # Quitamos NA´s que meten filas extras y convertimos la lista en un data frame para su manipulación con R

str(table)  # Vemos la naturaleza de las variables

# Por último realizamos una conversión de una columna tipo chr a num, se pueden 
# hacer las conversiones que se requieran

table$Albedo <- as.numeric(table$Albedo)
str(table)


# Reto 2. Extracción de tablas en un HTML ----

# Ahora es momento de realizar la extracción de una tabla desde un html, realiza 
# este reto desde tu RStudio Desktop.

#De la siguiente dirección donde se muestran los sueldos para Data Scientists
# (https://www.glassdoor.com.mx/Sueldos/data-scientist-sueldo-SRCH_KO0,14.htm),
# realiza las siguientes acciones:
      
# - Extraer la tabla del HTML

library(rvest)

theurl <- "https://www.glassdoor.com.mx/Sueldos/data-scientist-sueldo-SRCH_KO0,14.htm"

file<-read_html(theurl)

tables<-html_nodes(file, "table")
# Hay que analizar 'tables' para determinar cual es la posición en la lista que contiene la tabla, en este caso es la no. 4 

table1 <- html_table(tables[1], fill = TRUE)

table <- na.omit(as.data.frame(table1))   # Quitamos NA´s que meten filas extras y convertimos la lista en un data frame para su manipulación con R

str(table)  # Vemos la naturaleza de las variables


# - Quitar los caracteres no necesarios de la columna sueldos (todo lo que no sea
#   número), para dejar solamente la cantidad mensual (Hint: la función gsub podría
#   ser de utilidad)

a <- gsub("MXN","",table$Sueldo)
a <- gsub("[^[:alnum:][:blank:]?]", "", a)
a <- gsub("mes", "", a)

# - Asignar ésta columna como tipo numérico para poder realizar operaciones con ella

a <- as.numeric(a)
table$Sueldo <- a

# Ahora podrás responder esta pregunta ¿Cuál es la empresa que más paga y la que 
# menos paga?

#Removiendo caracteres inncesarios
b <- gsub("Sueldos para Data Scientist en ", "", table$Cargo)
table$Cargo <-b

# Maximo sueldo
table %>% filter(Sueldo == max(table$Sueldo))

# max.sueldo <- which.max(table$Sueldo)
# table[max.sueldo,]

# Minimo sueldo

table %>% filter(Sueldo == min(table$Sueldo))

# min.sueldo <- which.min(table$Sueldo)
# table[min.sueldo,]

# Reto 3 Github -> RStudio ----

# Del script que se generó en el reto 2, deberás realizar las siguientes acciones

# 1. Tendras que subir el archivo al repositorio en github que se creo
#    Reto_Sesion_7

# 2. Ahora realizar el pull esto es mandarlo a RStudio Cloud, para poder 
#    realizar su manipulación desde la nube




