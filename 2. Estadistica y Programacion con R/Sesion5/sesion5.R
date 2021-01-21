###########################################################-
# Objective: Sesion 5
# Author: Gabriel Sainz Vázquez
# Date Modified: 20/01/2021
###########################################################-

# Ejemplo 1. Regresión Lineal Simple----

# Primero hay que establecer el directorio de trabajo y este deberá contener el 
# archivo de datos production.txt

# Leemos nuestros datos con la función read.table
setwd("C:\\Users\\Gabo\\Desktop\\Becas Santander- Data Science\\2.Fase2\\2. Estadística y Programación con R\\Sesion5\\data")

production <- read.table("production.txt", header = TRUE)

# Los datos que importamos a R se encuentran como data frame con nombre 
# production. Aplicamos la función attach al data frame production para poder 
# manipular las columnas mediante sus nombres.
RunTime

attach(production)

RunTime

# Hacemos el gráfico de dispersión

plot(RunSize, RunTime, xlab = "Tamaño de ejecución", 
     ylab = "Tiempo de ejecución", pch = 16)

# Ajustamos un modelo de regresión lineal simple con la función lm, en donde la 
#variable de respuesta es RunTime y la variable predictora es RunSize. Guardamos 
#nuestro modelo ajustado con el nombre de m1.

m1 <- lm(RunTime~RunSize)

#Obtenemos un resumen de nuestro modelo ajustado mediante la función summary

summary(m1)

#Graficamos nuestros datos nuevamente, ahora con la recta de regresión estimada, 
# mostrando algunas ecuaciones y algunos residuales gráficamente.

plot(RunSize, RunTime, xlab = "Tamaño de ejecución", 
     ylab = "Tiempo de ejecución", pch = 16)
abline(lsfit(RunSize, RunTime)) # Trazamos la recta de regresión estimada
mtext(expression(paste('Modelo de regresión lineal simple:',
                       ' ',
                       y[i] == beta[0] + beta[1]*x[i] + e[i])),
      side = 3, adj=1, font = 2)

# Recta de regresión poblacional

text(x = 200, y = 240, expression(paste('Recta de regresión:',
                                        ' ',
                                        y[i] == beta[0] + beta[1]*x[i])),
     adj = 1, font = 2)


# Recta de regresión estimada

text(x = 350, y = 180, expression(paste('Recta estimada:',
                                        ' ',
                                        hat(y)[i] == hat(beta)[0] + hat(beta)[1]*x[i])),
     adj = 1, font = 2)

# Recta de regresión estimada

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

# Acontinuación encontramos el cuantil de orden 0.975 de la distribución t de 
# Student con 18 (n - 2) grados de libertad. En total tenemos n = 20 
# observaciones en nuestro conjunto de datos. Estamos encontrando el valor que 
# satisface P(T > tval) = 0.025

tval <- qt(1-0.05/2, 18)
tval

# Comprobamos

pt(tval, df = 18)
# Encontramos intervalos de confianza del 95% para el intercepto y la pendiente 
# del modelo de regresión lineal simple

round(confint(m1, level = 0.95), 3)

# Ahora encontramos intervalos de confianza del 95% para la recta de regresión 
# poblacional en algunos valores de X (RunSize)

RunSize0 <- c(50,100,150,200,250,300,350) # Algunos posibles valores de RunSize

(conf <- predict(m1, newdata = 
                       data.frame(RunSize = RunSize0), 
                 interval = "confidence", level = 0.95))

# Podemos visualizar gráficamente estos intervalos de confianza

lines(RunSize0, conf[, 2], lty = 2, lwd = 2, col = "green") # límites inferiores
lines(RunSize0, conf[, 3], lty = 2, lwd = 2, col = "green") # límites superiores

# También podemos encontrar intervalos de predicción del 95% para el valor real 
# de la variable de respuesta Y (RunTime) en algunos valores de X (RunSize)

(pred <- predict(m1, newdata = 
                       data.frame(RunSize = RunSize0), 
                 interval = "prediction", level = 0.95))

# Podemos visualizar gráficamente estos intervalos de predicción

lines(RunSize0, pred[, 2], lty = 2, lwd = 2, col = "blue") # límites inferiores
lines(RunSize0, pred[, 3], lty = 2, lwd = 2, col = "blue") # límites superiores

# Note como los intervalos de confianza están contenidos dentro de los 
# intervalos de predicción correspondientes.

# También es posible llevar a cabo un análisis de varianza para decidir si 
# existe asociación lineal entre RunSize y RunTime

anova(m1)

# Gráficos de diagnóstico de R
# Cuando usamos un modelo de regresión, hacemos una serie de suposiciones.
# Entonces debemos hacer diagnósticos de regresión para verificar las supocisiones.
hist(m1$residuals)

par(mfrow = c(2, 2))

plot(m1)

shapiro.test(m1$residuals)
dev.off()

# Reto 1. Regresión lineal simple ----

# Se cree que entre las variables x y y del archivo csv adjunto, podría haber 
# una relación más o menos lineal. Para tener más evidencia sobre esto lleve a 
# cabo lo siguiente:
data <- read.csv("datoslineal.csv")
attach(data)
head(data)
      
# Realice el gráfico de dispersión de los datos
plot(data)

# Ajuste un modelo de regresión lineal simple a los datos, muestre un resumen 
# del modelo ajustado y trace la recta de regresión estimada junto con el gráfico 
# de dispersión
m2 <- lm(data$y ~ data$x)
abline(lsfit(data$x, data$y))
summary(m2)

# Obtenga algunas gráficas de diagnóstico y diga si es razonable suponer para 
# los errores aleatoriedad, normalidad y varianza constante.
par(mfrow = c(2,2))
plot(m2)

dev.off()

# Ejemplo 2: regresión Lineal Múltiple ----

# Supongamos que queremos emprender un negocio o que se nos colicita un estudio 
# en en cual se requiere predecir el precio de cena (platillo), para poder estar 
# dentro de los rangos de precios del mercado y que el restaurante sea rentable.

# Entonces primero vamos a analizar los datos de encuestas de clientes de 168 
# restaurantes Italianos en el área deseada que están disponibles, los cuales 
# tienen las siguientes variables de estudio:
      
# Y: Price (Precio): el precio (en USD) de la cena
# X1: Food: Valuación del cliente de la comida (sacado de 30)
# X2: Décor: Valuación del cliente de la decoración (sacado de 30)
# X3: Service: Valuación del cliente del servicio (sacado de 30)
# X4: East: variable dummy: 1 (0) si el restaurante está al este (oeste) de 
# la quinta avenida
 
#Primero debemos establecer nuestro directorio de trabajo y el archivo de datos 
# (nyc.csv) que importaremos a R deberá de estar en este directorio.

nyc <- read.csv("nyc.csv", header = TRUE)

# Observamos algunas filas y la dimensión del data frame

head(nyc, 2); tail(nyc, 2); dim(nyc)
attach(nyc)
# Llevamos a cabo el ajuste de un modelo Y = beta0 + beta1Food + beta2Decor + 
# beta3Service + beta4East + e

m1 <- lm(Price ~ Food + Decor + Service + East)

# Obtenemos un resumen

summary(m1)
# Ajustamos nuevamente un modelo pero ahora sin considerar la variable Service 
# ya que en el resultado anterior se observó que su coeficiente de regresión no 
# fue estadísticamente significativo Y = beta0 + beta1Food + beta2Decor +
# beta4*East + e (Reducido)

m2 <- lm(Price ~ Food + Decor + East)

# Obtenemos un resumen del modelo ajustado

summary(m2)

# Una forma alternativa de obtener m2 es usar el comando update

m2 <- update(m1, ~.-Service)
summary(m2)

# Análisis de covarianza
# Para investigar si el efecto de los predictores depende de la variable dummy 
# East consideraremos el siguiente modelo el cual es una extensión a más de una 
# variable predictora del modelo de rectas de regresión no relacionadas 
# Y = beta0 + beta1Food + beta2Decor + beta3Service + beta4East + beta5FoodEast
# + beta6DecorEast + beta7ServiceEast + e (Completo)

mfull <- lm(Price ~ Food + Decor + Service + East + 
                  Food:East + Decor:East + Service:East)

# Note como ninguno de los coeficientes de regresión para los términos de 
# interacción son estadísticamente significativos

summary(mfull)

# Ahora compararemos el modelo completo guardado en mfull contra el modelo 
# reducido guardado en m2. Es decir, llevaremos a cabo una prueba de hipótesis 
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
# hipótesis nula y adopatamos el modelo reducido Y = beta0 + beta1Food + 
# beta2Decor + beta4*East + e (Reducido)

# Diagnósticos
# En regresión múltiple, las gráficas de residuales o de residuales
# estandarizados proporcionan información directa sobre la forma en la cual 
# el modelo está mal especificado cuando se cumplen las siguientes dos condiciones:
      
# E(Y | X = x) = g(beta0 + beta1x1 + ... + betapxp)

# y

# E(Xi | Xj) aprox alpha0 + alpha1*Xj

# Cuando estas condiciones se cumplen, la gráfica de Y contra los valores 
# ajustados, proporciona información directa acerca de g. En regresión lineal
# múltiple g es la función identidad. En este caso la gráfica de Y contra los
# valores ajustados debe producir puntos dispersos alrededor de una recta. Si 
# las condiciones no se cumplen, entonces un patrón en la gráfica de los 
# residuales indica que un modelo incorrecto ha sido ajustado, pero el patrón
# mismo no proporciona información directa sobre como el modelo está mal 
# específicado.

# Ahora tratemos de verificar si el modelo ajustado es un modelo válido.

# Acontinuación mostramos una matriz de gráficos de dispersión de los tres 
# predictores continuos. Los predictores parecen estar linealmente relacionados 
# al menos aproximadamente

pairs(~ Food + Decor + Service, data = nyc, gap = 0.4, cex.labels = 1.5)

# Acontinuación veremos gráficas de residuales estandarizados contra cada 
# predictor. La naturaleza aleatoria de estas gráficas es un indicativo de 
# que el modelo ajustado es un modelo válido para los datos.

m1 <- lm(Price ~ Food + Decor + Service + East)
summary(m1)
StanRes1 <- rstandard(m1)
par(mfrow = c(2, 2))
plot(Food, StanRes1, ylab = "Residuales Estandarizados")
plot(Decor, StanRes1, ylab = "Residuales Estandarizados")
plot(Service, StanRes1, ylab = "Residuales Estandarizados")
plot(East, StanRes1, ylab = "Residuales Estandarizados")
dev.off()

# Finalmente mostramos una gráfica de Y, el precio contra los valores ajustados

plot(m1$fitted.values, Price, xlab = "Valores ajustados", ylab = "Price")
abline(lsfit(m1$fitted.values, Price))


# Ejemplo 3. Máquinas de vectores de soporte (Compañía de tarjetas de crédito) ----

# Paquetes de R utilizados

suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(e1071)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(ISLR)))

#Observemos algunas características del data frame Default del paquete ISLR, 
# con funciones tales como head, tail, dim y str.

?Default
head(Default)
tail(Default)
dim(Default)
str(Default)

# Usando ggplot del paquete ggplot2, realicemos un gráfico de dispersión con 
# la variable balance en el eje x, la variable income en el eje y, diferenciando 
# las distintas categorías en la variable default usando el argumento colour. 
# Lo anterior para estudiantes y no estudiantes usando facet_wrap.

ggplot(Default, aes(x = balance, y = income, colour = default)) + 
      geom_point() + facet_wrap('student') + 
      theme_grey() + ggtitle("Datos Default")

# Generemos un vector de índices llamado train, tomando de manera aleatoria 5000 
# números de los primeros 10,000 números naturales, esto servirá para filtrar el 
# conjunto de entrenamiento y el conjunto de prueba del data frame Default. 
# Realicemos el gráfico de dispersión análogo al punto 2, pero para los conjuntos 
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

# Ahora utilicemos la función tune junto con la función svm para seleccionar el 
# mejor modelo de un conjunto de modelos, los modelos considerados serán aquellos 
# obtenidos al variar los valores de los parámetros cost y gamma (usaremos un 
# kernel radial).

# Ahora utilizamos la función `tune` junto con la función `svm` para 
# seleccionar el mejor modelo de un conjunto de modelos, los modelos 
# considerados son aquellos obtenidos al variar los valores de los 
# parámetros `cost` y `gamma`. Kernel Radial

#tune.rad = tune(svm, default~., data = Default[train,], 
#                kernel = "radial", 
#                ranges = list(
#                  cost = c(0.1, 1, 10, 100, 1000), 
#                  gamma = seq(0.01, 10, 0.5)
#                ) 
#)

# Se ha elegido el mejor modelo utilizando *validación cruzada de 10 
# iteraciones*

# summary(tune.rad)

# Aquí un resumen del modelo seleccionado

# summary(tune.rad$best.model)

best <- svm(default~.,  data = Default[train,],
            kernel = "radial",
            cost = 100,
            gamma = 1.51
)

# Con el mejor modelo seleccionado y utilizando el conjunto de prueba, 
# obtengamos una matriz de confusión, para observar el número de aciertos y
# errores cometidos por el modelo. También obtengamos la proporción total de
# aciertos y la matriz que muestre las proporciones de aciertos y errores
# cometidos pero por categorías.

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

# Realicemos clasificación de las observaciones del conjunto de prueba 
# utilizando los valores predichos por el modelo y un umbral de decisión igual 
# a cero. También obtengamos la matriz de confusión y proporciones como 
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

# Repitamos el paso 7 pero con un umbral de decisión diferente, de tal manera
# que se reduzca la proporción del error más grave para la compañía de tarjetas 
# de crédito.
eti <- ifelse(fitted < 1.002, "Yes", "No")

mc <- table(true = Default[-train, "default"], 
            pred = eti)
mc

round(sum(diag(mc))/sum(colSums(mc)), 5)

rs <- apply(mc, 1, sum)
r1 <- round(mc[1,]/rs[1], 5)
r2 <- round(mc[2,]/rs[2], 5)
rbind(No=r1, Yes=r2)


