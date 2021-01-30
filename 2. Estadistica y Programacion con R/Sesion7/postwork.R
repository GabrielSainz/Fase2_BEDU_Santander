###########################################################-
# Objective: Sesion 7: Postwork
# Author: Gabriel Sainz Vázquez
# Date Modified: 30/01/2021
###########################################################-

# Utilizando el manejador de BDD Mongodb Compass (previamente instalado), 
# deberás de realizar las siguientes acciones:

# install.packages("mongolite")  
# https://datascienceplus.com/using-mongodb-with-r/
# To use MongoDB with R, first, we have to download and install MongoDB
# Se debe estar conectado a mongoDB

library(mongolite)
setwd("C:/Users/Gabo/Desktop/Becas Santander- Data Science/2.Fase2/2. Estadística y Programación con R/Sesion7")
soccer <- read.csv("data.csv") 

# https://cran.r-project.org/web/packages/mongolite/mongolite.pdf


# - Alojar el fichero data.csv en una base de datos llamada match_games, 
#   nombrando al collection como match

database = data.table::fread("data.csv")

# we can use the insert function from the mongolite package to insert rows to 
# a collection in MongoDB. 

my_collection = mongo(collection = "database", db = "match_games") # create connection, database and collection
my_collection$insert(soccer)

# - Una vez hecho esto, realizar un count para conocer el número de registros 
#   que se tiene en la base

# Let’s check if we have inserted the “soccer” data.
my_collection$count()

# We see that the collection has 1140 records.

my_collection$find()


# - Realiza una consulta utilizando la sintaxis de Mongodb, en la base de datos 
#   para conocer el número de goles que metió el Real Madrid el 20 de diciembre
#   de 2015 y contra qué equipo jugó, ¿perdió ó fue goleada?

# To get the filtered data and we can also retrieve only the columns of interest

my_collection$find('{"HomeTeam":"Real Madrid"}')
my_collection$find('{"HomeTeam":"Real Madrid", "Date": "2015-12-20"}') 
# No se econtró ningún partido del real Madrid en el 2015-15-20

my_collection$find('{"HomeTeam":"Real Madrid", "Date": "2020-06-18"}') 

rm(my_collection)

rm(my_collection)
gc()


