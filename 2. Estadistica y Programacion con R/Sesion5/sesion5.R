###########################################################-
# Objective: Sesion 5
# Author: Gabriel Sainz V�zquez
# Date Modified: 20/01/2021
###########################################################-

# Ejemplo 1. Regresi�n Lineal Simple----

# Primero hay que establecer el directorio de trabajo y este deber� contener el 
# archivo de datos production.txt

# Leemos nuestros datos con la funci�n read.table
setwd("C:\\Users\\Gabo\\Desktop\\Becas Santander- Data Science\\2.Fase2\\2. Estad�stica y Programaci�n con R\\Sesion5\\data")

production <- read.table("production.txt", header = TRUE)

# Los datos que importamos a R se encuentran como data frame con nombre 
# production. Aplicamos la funci�n attach al data frame production para poder 
# manipular las columnas mediante sus nombres.
RunTime

attach(production)

RunTime

# Hacemos el gr�fico de dispersi�n

plot(RunSize, RunTime, xlab = "Tama�o de ejecuci�n", 
     ylab = "Tiempo de ejecuci�n", pch = 16)

# Ajustamos un modelo de regresi�n lineal simple con la funci�n lm, en donde la 
#variable de respuesta es RunTime y la variable predictora es RunSize. Guardamos 
#nuestro modelo ajustado con el nombre de m1.

m1 <- lm(RunTime~RunSize)

#Obtenemos un resumen de nuestro modelo ajustado mediante la funci�n summary

summary(m1)

#Graficamos nuestros datos nuevamente, ahora con la recta de regresi�n estimada, 
# mostrando algunas ecuaciones y algunos residuales gr�ficamente.

plot(RunSize, RunTime, xlab = "Tama�o de ejecuci�n", 
     ylab = "Tiempo de ejecuci�n", pch = 16)
abline(lsfit(RunSize, RunTime)) # Trazamos la recta de regresi�n estimada
mtext(expression(paste('Modelo de regresi�n lineal simple:',
                       ' ',
                       y[i] == beta[0] + beta[1]*x[i] + e[i])),
      side = 3, adj=1, font = 2)

# Recta de regresi�n poblacional

text(x = 200, y = 240, expression(paste('Recta de regresi�n:',
                                        ' ',
                                        y[i] == beta[0] + beta[1]*x[i])),
     adj = 1, font = 2)


# Recta de regresi�n estimada

text(x = 350, y = 180, expression(paste('Recta estimada:',
                                        ' ',
                                        hat(y)[i] == hat(beta)[0] + hat(beta)[1]*x[i])),
     adj = 1, font = 2)

# Recta de regresi�n estimada

text(x = 350, y = 160, expression(paste('Recta estimada:',
                                        ' ',
                                        hat(y)[i] == 149.74770 + 0.25924*x[i])),
     adj = 1, font = 2)

# Residuales

points(189, 215, pch=16, col = "red") # Punto muestral
149.74770 + 0.25924 * 189 # Valor y sobre la recta estimada
lines(c(189, 189), c(198.7441, 215), col = "red")

points(173, 166, pch=16, col = "red") # Punto muestral
149.74770 + 0.25924 * 173 # Valor y sobre la recta estimada
lines(c(173, 173), c(166, 194.5962), col = "red")

# Acontinuaci�n encontramos el cuantil de orden 0.975 de la distribuci�n t de 
# Student con 18 (n - 2) grados de libertad. En total tenemos n = 20 
# observaciones en nuestro conjunto de datos. Estamos encontrando el valor que 
# satisface P(T > tval) = 0.025

tval <- qt(1-0.05/2, 18)
tval

# Comprobamos

pt(tval, df = 18)
# Encontramos intervalos de confianza del 95% para el intercepto y la pendiente 
# del modelo de regresi�n lineal simple

round(confint(m1, level = 0.95), 3)

# Ahora encontramos intervalos de confianza del 95% para la recta de regresi�n 
# poblacional en algunos valores de X (RunSize)

RunSize0 <- c(50,100,150,200,250,300,350) # Algunos posibles valores de RunSize

(conf <- predict(m1, newdata = 
                       data.frame(RunSize = RunSize0), 
                 interval = "confidence", level = 0.95))

# Podemos visualizar gr�ficamente estos intervalos de confianza

lines(RunSize0, conf[, 2], lty = 2, lwd = 2, col = "green") # l�mites inferiores
lines(RunSize0, conf[, 3], lty = 2, lwd = 2, col = "green") # l�mites superiores

# Tambi�n podemos encontrar intervalos de predicci�n del 95% para el valor real 
# de la variable de respuesta Y (RunTime) en algunos valores de X (RunSize)

(pred <- predict(m1, newdata = 
                       data.frame(RunSize = RunSize0), 
                 interval = "prediction", level = 0.95))

# Podemos visualizar gr�ficamente estos intervalos de predicci�n

lines(RunSize0, pred[, 2], lty = 2, lwd = 2, col = "blue") # l�mites inferiores
lines(RunSize0, pred[, 3], lty = 2, lwd = 2, col = "blue") # l�mites superiores

# Note como los intervalos de confianza est�n contenidos dentro de los 
# intervalos de predicci�n correspondientes.

# Tambi�n es posible llevar a cabo un an�lisis de varianza para decidir si 
# existe asociaci�n lineal entre RunSize y RunTime

anova(m1)

# Gr�ficos de diagn�stico de R
# Cuando usamos un modelo de regresi�n, hacemos una serie de suposiciones.
# Entonces debemos hacer diagn�sticos de regresi�n para verificar las supocisiones.
hist(m1$residuals)

par(mfrow = c(2, 2))

plot(m1)

shapiro.test(m1$residuals)
dev.off()

# Reto 1. Regresi�n lineal simple ----

# Se cree que entre las variables x y y del archivo csv adjunto, podr�a haber 
# una relaci�n m�s o menos lineal. Para tener m�s evidencia sobre esto lleve a 
# cabo lo siguiente:
data <- read.csv("datoslineal.csv")
attach(data)
head(data)
      
# Realice el gr�fico de dispersi�n de los datos
plot(data)

# Ajuste un modelo de regresi�n lineal simple a los datos, muestre un resumen 
# del modelo ajustado y trace la recta de regresi�n estimada junto con el gr�fico 
# de dispersi�n
m2 <- lm(data$y ~ data$x)
abline(lsfit(data$x, data$y))
summary(m2)

# Obtenga algunas gr�ficas de diagn�stico y diga si es razonable suponer para 
# los errores aleatoriedad, normalidad y varianza constante.
par(mfrow = c(2,2))
plot(m2)

dev.off()

# Ejemplo 2: regresi�n Lineal M�ltiple ----

# Supongamos que queremos emprender un negocio o que se nos colicita un estudio 
# en en cual se requiere predecir el precio de cena (platillo), para poder estar 
# dentro de los rangos de precios del mercado y que el restaurante sea rentable.

# Entonces primero vamos a analizar los datos de encuestas de clientes de 168 
# restaurantes Italianos en el �rea deseada que est�n disponibles, los cuales 
# tienen las siguientes variables de estudio:
      
# Y: Price (Precio): el precio (en USD) de la cena
# X1: Food: Valuaci�n del cliente de la comida (sacado de 30)
# X2: D�cor: Valuaci�n del cliente de la decoraci�n (sacado de 30)
# X3: Service: Valuaci�n del cliente del servicio (sacado de 30)
# X4: East: variable dummy: 1 (0) si el restaurante est� al este (oeste) de 
# la quinta avenida
 
#Primero debemos establecer nuestro directorio de trabajo y el archivo de datos 
# (nyc.csv) que importaremos a R deber� de estar en este directorio.

nyc <- read.csv("nyc.csv", header = TRUE)

# Observamos algunas filas y la dimensi�n del data frame

head(nyc, 2); tail(nyc, 2); dim(nyc)
attach(nyc)
# Llevamos a cabo el ajuste de un modelo Y = beta0 + beta1Food + beta2Decor + 
# beta3Service + beta4East + e

m1 <- lm(Price ~ Food + Decor + Service + East)

# Obtenemos un resumen

summary(m1)
# Ajustamos nuevamente un modelo pero ahora sin considerar la variable Service 
# ya que en el resultado anterior se observ� que su coeficiente de regresi�n no 
# fue estad�sticamente significativo Y = beta0 + beta1Food + beta2Decor +
# beta4*East + e (Reducido)

m2 <- lm(Price ~ Food + Decor + East)

# Obtenemos un resumen del modelo ajustado

summary(m2)

# Una forma alternativa de obtener m2 es usar el comando update

m2 <- update(m1, ~.-Service)
summary(m2)

# An�lisis de covarianza
# Para investigar si el efecto de los predictores depende de la variable dummy 
# East consideraremos el siguiente modelo el cual es una extensi�n a m�s de una 
# variable predictora del modelo de rectas de regresi�n no relacionadas 
# Y = beta0 + beta1Food + beta2Decor + beta3Service + beta4East + beta5FoodEast
# + beta6DecorEast + beta7ServiceEast + e (Completo)

mfull <- lm(Price ~ Food + Decor + Service + East + 
                  Food:East + Decor:East + Service:East)

# Note como ninguno de los coeficientes de regresi�n para los t�rminos de 
# interacci�n son estad�sticamente significativos

summary(mfull)

# Ahora compararemos el modelo completo guardado en mfull contra el modelo 
# reducido guardado en m2. Es decir, llevaremos a cabo una prueba de hip�tesis 
# general de

# H0: beta3 = beta5 = beta6 = beta7 = 0

# es decir Y = beta0 + beta1Food + beta2Decor + beta4*East + e (Reducido)

# contra

# H1: H0 no es verdad

# es decir, Y = beta0 + beta1Food + beta2Decor + beta3Service + beta4East + 
# beta5FoodEast + beta6DecorEast + beta7ServiceEast + e (Completo)

# La prueba de si el efecto de los predictores depende de la variable dummy 
# East puede lograrse usando la siguiente prueba-F parcial.

anova(m2,mfull)

# Dado que el p-value es aproximadamente 0.36, fallamos en rechazar la 
# hip�tesis nula y adopatamos el modelo reducido Y = beta0 + beta1Food + 
# beta2Decor + beta4*East + e (Reducido)

# Diagn�sticos
# En regresi�n m�ltiple, las gr�ficas de residuales o de residuales
# estandarizados proporcionan informaci�n directa sobre la forma en la cual 
# el modelo est� mal especificado cuando se cumplen las siguientes dos condiciones:
      
# E(Y | X = x) = g(beta0 + beta1x1 + ... + betapxp)

# y

# E(Xi | Xj) aprox alpha0 + alpha1*Xj

# Cuando estas condiciones se cumplen, la gr�fica de Y contra los valores 
# ajustados, proporciona informaci�n directa acerca de g. En regresi�n lineal
# m�ltiple g es la funci�n identidad. En este caso la gr�fica de Y contra los
# valores ajustados debe producir puntos dispersos alrededor de una recta. Si 
# las condiciones no se cumplen, entonces un patr�n en la gr�fica de los 
# residuales indica que un modelo incorrecto ha sido ajustado, pero el patr�n
# mismo no proporciona informaci�n directa sobre como el modelo est� mal 
# espec�ficado.

# Ahora tratemos de verificar si el modelo ajustado es un modelo v�lido.

# Acontinuaci�n mostramos una matriz de gr�ficos de dispersi�n de los tres 
# predictores continuos. Los predictores parecen estar linealmente relacionados 
# al menos aproximadamente

pairs(~ Food + Decor + Service, data = nyc, gap = 0.4, cex.labels = 1.5)

# Acontinuaci�n veremos gr�ficas de residuales estandarizados contra cada 
# predictor. La naturaleza aleatoria de estas gr�ficas es un indicativo de 
# que el modelo ajustado es un modelo v�lido para los datos.

m1 <- lm(Price ~ Food + Decor + Service + East)
summary(m1)
StanRes1 <- rstandard(m1)
par(mfrow = c(2, 2))
plot(Food, StanRes1, ylab = "Residuales Estandarizados")
plot(Decor, StanRes1, ylab = "Residuales Estandarizados")
plot(Service, StanRes1, ylab = "Residuales Estandarizados")
plot(East, StanRes1, ylab = "Residuales Estandarizados")
dev.off()

# Finalmente mostramos una gr�fica de Y, el precio contra los valores ajustados

plot(m1$fitted.values, Price, xlab = "Valores ajustados", ylab = "Price")
abline(lsfit(m1$fitted.values, Price))


# Ejemplo 3. M�quinas de vectores de soporte (Compa��a de tarjetas de cr�dito) ----

# Paquetes de R utilizados

suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(e1071)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(ISLR)))

#Observemos algunas caracter�sticas del data frame Default del paquete ISLR, 
# con funciones tales como head, tail, dim y str.

?Default
head(Default)
tail(Default)
dim(Default)
str(Default)

# Usando ggplot del paquete ggplot2, realicemos un gr�fico de dispersi�n con 
# la variable balance en el eje x, la variable income en el eje y, diferenciando 
# las distintas categor�as en la variable default usando el argumento colour. 
# Lo anterior para estudiantes y no estudiantes usando facet_wrap.

ggplot(Default, aes(x = balance, y = income, colour = default)) + 
      geom_point() + facet_wrap('student') + 
      theme_grey() + ggtitle("Datos Default")

# Generemos un vector de �ndices llamado train, tomando de manera aleatoria 5000 
# n�meros de los primeros 10,000 n�meros naturales, esto servir� para filtrar el 
# conjunto de entrenamiento y el conjunto de prueba del data frame Default. 
# Realicemos el gr�fico de dispersi�n an�logo al punto 2, pero para los conjuntos 
# de entrenamiento y de prueba.

set.seed(2020)
train = sample(nrow(Default), 
               round(nrow(Default)/2))
tail(Default[train, ])

ggplot(Default[train, ], 
       aes(x = balance, y = income, colour = default)) + 
      geom_point() + facet_wrap('student') + 
      theme_dark() + ggtitle("Conjunto de entrenamiento")

ggplot(Default[-train, ], 
       aes(x = balance, y = income, colour = default)) + 
      geom_point() + facet_wrap('student') + 
      theme_light() + ggtitle("Conjunto de prueba")

# Ahora utilicemos la funci�n tune junto con la funci�n svm para seleccionar el 
# mejor modelo de un conjunto de modelos, los modelos considerados ser�n aquellos 
# obtenidos al variar los valores de los par�metros cost y gamma (usaremos un 
# kernel radial).

# Ahora utilizamos la funci�n `tune` junto con la funci�n `svm` para 
# seleccionar el mejor modelo de un conjunto de modelos, los modelos 
# considerados son aquellos obtenidos al variar los valores de los 
# par�metros `cost` y `gamma`. Kernel Radial

#tune.rad = tune(svm, default~., data = Default[train,], 
#                kernel = "radial", 
#                ranges = list(
#                  cost = c(0.1, 1, 10, 100, 1000), 
#                  gamma = seq(0.01, 10, 0.5)
#                ) 
#)

# Se ha elegido el mejor modelo utilizando *validaci�n cruzada de 10 
# iteraciones*

# summary(tune.rad)

# Aqu� un resumen del modelo seleccionado

# summary(tune.rad$best.model)

best <- svm(default~.,  data = Default[train,],
            kernel = "radial",
            cost = 100,
            gamma = 1.51
)

# Con el mejor modelo seleccionado y utilizando el conjunto de prueba, 
# obtengamos una matriz de confusi�n, para observar el n�mero de aciertos y
# errores cometidos por el modelo. Tambi�n obtengamos la proporci�n total de
# aciertos y la matriz que muestre las proporciones de aciertos y errores
# cometidos pero por categor�as.

mc <- table(true = Default[-train, "default"], 
            pred = predict(best, 
                           newdata = Default[-train,]))
mc

# El porcentaje total de aciertos obtenido por el modelo usando el 
# conjunto de prueba es el siguiente

round(sum(diag(mc))/sum(colSums(mc)), 5)

# Ahora observemos las siguientes proporciones

rs <- apply(mc, 1, sum)
r1 <- round(mc[1,]/rs[1], 5)
r2 <- round(mc[2,]/rs[2], 5)
rbind(No=r1, Yes=r2)

# Ajustemos nuevamente el mejor modelo, pero ahora con el argumento 
# decision.values = TRUE. Obtengamos los valores predichos para el conjunto de 
# prueba utilizando el mejor modelo, las funciones predict, attributes y el 
# argumento decision.values = TRUE dentro de predict.

fit <- svm(default ~ ., data = Default[train,], 
           kernel = "radial", cost = 100, gamma = 1.51,
           decision.values = TRUE)

fitted <- attributes(predict(fit, Default[-train,], 
                             decision.values = TRUE))$decision.values

# Realicemos clasificaci�n de las observaciones del conjunto de prueba 
# utilizando los valores predichos por el modelo y un umbral de decisi�n igual 
# a cero. Tambi�n obtengamos la matriz de confusi�n y proporciones como 
# anteriormente hicimos.
eti <- ifelse(fitted < 0, "Yes", "No")

mc <- table(true = Default[-train, "default"], 
            pred = eti)
mc

round(sum(diag(mc))/sum(colSums(mc)), 5)

rs <- apply(mc, 1, sum)
r1 <- round(mc[1,]/rs[1], 5)
r2 <- round(mc[2,]/rs[2], 5)
rbind(No=r1, Yes=r2)

# Repitamos el paso 7 pero con un umbral de decisi�n diferente, de tal manera
# que se reduzca la proporci�n del error m�s grave para la compa��a de tarjetas 
# de cr�dito.
eti <- ifelse(fitted < 1.002, "Yes", "No")

mc <- table(true = Default[-train, "default"], 
            pred = eti)
mc

round(sum(diag(mc))/sum(colSums(mc)), 5)

rs <- apply(mc, 1, sum)
r1 <- round(mc[1,]/rs[1], 5)
r2 <- round(mc[2,]/rs[2], 5)
rbind(No=r1, Yes=r2)


