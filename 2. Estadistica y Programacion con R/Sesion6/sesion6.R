###########################################################-
# Objective: Sesion 6: Series de tiempo
# Author: Gabriel Sainz Vázquez
# Date Modified: 25/01/2021
###########################################################-


# Ejemplo 1. Ejemplos de series de tiempo y técnicas descriptivas ----

# En este ejemplo se hará la visualización y descomposición de series de tiempo, 
# además de poder determinar la tendencia de nuestros datos a lo largo de un 
# periodo de tiempo determinado, las series de tiempo son recolectadas y sirven 
# para estudios de tipo retrospectivo, además también se pueden realizar
# predicciones a futuro. A continuación verás aplicaciones que te serán de
# mucha utilidad.

# Técnicas descriptivas: gráficas, tendencias y variación estacional

library(TSA)

data(oilfilters)
plot(oilfilters, type = "o", ylab = "Ventas", xlab = "Tiempo", 
     main = "Ventas Mesuales ")
plot(oilfilters, type = "l", ylab = "Ventas", xlab = "Tiempo",
     main = "Ventas Mensuales de Filtro de Aceite",
     sub = "Símbolos Especiales")
points(y = oilfilters, x = time(oilfilters),
       pch = as.vector(season(oilfilters)))
data(AirPassengers)
AP <- AirPassengers
AP

# Clase de un objeto

class(AP)

start(AP); end(AP); frequency(AP)

summary(AP)

plot(AP, ylab = "Pasajeros (1000's)", xlab = "Tiempo", 
     main = "Reserva de pasajeros aéreos internacionales", 
     sub = "Estados Unidos en el periodo 1949-1960")
layout(1:2)
plot(aggregate(AP), xlab = "Tiempo",
     main = "Reserva de pasajeros aéreos internacionales", 
     sub = "Estados Unidos en el periodo 1949-1960")

boxplot(AP ~ cycle(AP),
        xlab = "Boxplot de valores estacionales",
        sub = "Estados Unidos en el periodo 1949-1960",
        main = "Reserva de pasajeros aéreos internacionales")
dev.off()

# Algunos datos en https://github.com/AtefOuni/ts/tree/master/Data

# Series de Tiempo Múltiple
# Serie de producción de electricidad, cerveza y chocolate

setwd("C:\\Users\\Gabo\\Desktop\\Becas Santander- Data Science\\2.Fase2\\2. Estadística y Programación con R\\Sesion6\\data")

CBE <- read.csv("cbe.csv", header = TRUE)
CBE[1:4,]
class(CBE)

Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
Beer.ts <- ts(CBE[, 2], start = 1958, freq = 12)
Choc.ts <- ts(CBE[, 1], start = 1958, freq = 12)

Electricidad <- Elec.ts
Cerveza <- Beer.ts
Chocolate <- Choc.ts

plot(cbind(Electricidad, Cerveza, Chocolate), 
     main = "Producción de Chocolate, Cerveza y Electricidad", 
     xlab = "Tiempo",
     sub = "Enero de 1958 - Diciembre de 1990")

# Serie de temperatura global

Global <- scan("global.txt")
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12), fr = 12)
Global.annual <- aggregate(Global.ts, FUN = mean)
plot(Global.ts, xlab = "Tiempo", ylab = "Temperatura en °C", main = "Serie de Temperatura Global",
     sub = "Serie mensual: Enero de 1856 a Diciembre de 2005")
plot(Global.annual, xlab = "Tiempo", ylab = "Temperatura en °C", main = "Serie de Temperatura Global",
     sub = "Serie anual de temperaturas medias: 1856 a 2005")


New.series <- window(Global.ts, start = c(1970, 1), end = c(2005, 12)) 
New.time <- time(New.series)
plot(New.series, xlab = "Tiempo", ylab = "Temperatura en °C", main = "Serie de Temperatura Global",
     sub = "Serie mensual: Enero de 1970 a Diciembre de 2005"); abline(reg = lm(New.series ~ New.time))

# Descomposición de series

CBE <- read.csv("cbe.csv", header = TRUE)
Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)

# Modelo Aditivo

Elec.decom.A <- decompose(Elec.ts)

plot(Elec.decom.A, xlab = "Tiempo", 
     sub = "Descomposición de los datos de producción de electricidad")

# Componentes

Tendencia <- Elec.decom.A$trend
Estacionalidad <- Elec.decom.A$seasonal
Aleatorio <- Elec.decom.A$random

ts.plot(cbind(Tendencia, Tendencia + Estacionalidad), 
        xlab = "Tiempo", main = "Datos de Producción de Electricidad", 
        ylab = "Producción de electricidad", lty = 1:2,
        sub = "Tendencia con efectos estacionales aditivos sobrepuestos")

Tendencia[20] + Estacionalidad[20] + Aleatorio[20]
Elec.ts[20]

# Modelo Multiplicativo

Elec.decom.M <- decompose(Elec.ts, type = "mult")

plot(Elec.decom.M, xlab = "Tiempo", 
     sub = "Descomposición de los datos de producción de electricidad")

# Componentes

Trend <- Elec.decom.M$trend
Seasonal <- Elec.decom.M$seasonal
Random <- Elec.decom.M$random

ts.plot(cbind(Trend, Trend*Seasonal), xlab = "Tiempo", main = "Datos de Producción de Electricidad", 
        ylab = "Producción de electricidad", lty = 1:2,
        sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")

Trend[7]*Seasonal[7]*Random[7]
Elec.ts[7]

Trend[100]*Seasonal[100]*Random[100]
Elec.ts[100]


# Ejemplo 2. Modelos estocásticos básicos, modelos estacionarios y predicción ----

# Ruido Blanco y simulación en R

set.seed(1)
w <- rnorm(100)
plot(w, type = "l", xlab = "")
title(main = "Ruido Blanco Gaussiano", xlab = "Tiempo")
# Para ilustrar mediante simulación como las muestras pueden diferir de sus
# poblaciones subyacentes considere lo siguiente

x <- seq(-3, 3, length = 1000)
hist(rnorm(100), prob = T, ylab = "", xlab = "", main = "") 
points(x, dnorm(x), type = "l")
title(ylab = "Densidad", xlab = "Valores simulados de la distribución normal estandar",
      main = "Comparación de una muestra con su población subyacente")
set.seed(2)
acf(rnorm(100), main = "") # autocorrelaciones muestrales
title(main = "Función de Autocorrelación Muestral", 
      sub = "Valores simulados de la distribución normal estandar")

# Caminata aleatoria y simulación en R

x <- w <- rnorm(1000)
for(t in 2:1000) x[t] <- x[t-1] + w[t]
plot(x, type = "l", main = "Caminata Aleatoria Simulada", 
     xlab = "t", ylab = expression(x[t]), 
     sub = expression(x[t]==x[t-1]+w[t]))
acf(x, main = "")
title(main = "Correlograma para la caminata aleatoria simulada", 
      sub = expression(x[t]==x[t-1]+w[t]))

# Modelos ajustados y gráficas de diágnostico, series de caminatas aleatorias 
# simuladas. 
# El correlograma de las series de diferencias puede usarse para 
# evaluar si una serie dada puede modelarse como una caminata aleatoria

acf(diff(x), main = "")
title(main = "Correlograma de la serie de diferencias", 
      sub = expression(nabla*x[t]==x[t]-x[t-1]))

# Modelos AR(p), MA(q) y ARMA(p, q)
# Modelos AR(p)

# Correlograma de un proceso AR(1)

rho <- function(k, alpha) alpha^k
plot(0:10, rho(0:10, 0.7), type = "h", ylab = "", xlab = "")
title(main = "Correlograma para un proceso AR(1)",
      ylab = expression(rho[k] == alpha^k),
      xlab = "lag k",
      sub = expression(x[t]==0.7*x[t-1]+w[t]))

plot(0:10, rho(0:10, -0.7), type = "h", ylab = "", xlab = "")
title(main = "Correlograma para un proceso AR(1)",
      ylab = expression(rho[k] == alpha^k),
      xlab = "lag k",
      sub = expression(x[t]==-0.7*x[t-1]+w[t]))
abline(h = 0)

# Simulación en R

# Un proceso AR(1) puede ser simulado en R como sigue:
      
set.seed(1)
x <- w <- rnorm(100)
for(t in 2:100) x[t] <- 0.7 * x[t-1] + w[t]
plot(x, type = "l", xlab = "", ylab = "")
title(main = "Proceso AR(1) simulado",
      xlab = "Tiempo",
      ylab = expression(x[t]),
      sub = expression(x[t]==0.7*x[t-1]+w[t]))
acf(x, main = "")
title(main = "Correlograma del proceso AR(1) simulado", 
      sub = expression(x[t]==0.7*x[t-1]+w[t]))
pacf(x, main = "")
title(main = "Correlograma Parcial del proceso AR(1) simulado", 
      sub = expression(x[t]==0.7*x[t-1]+w[t]))

# Modelos Ajustados

# Ajuste de modelos a series simuladas

x.ar <- ar(x, method = "mle")
x.ar$order
x.ar$ar # Estimación puntual
x.ar$ar + c(-2, 2)*sqrt(x.ar$asy.var) # Intervalo de confianza

# Serie de temperatura global: Ajuste de un modelo AR

Global <- scan("global.txt")
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12), fr = 12)
Global.annual <- aggregate(Global.ts, FUN = mean)
plot(Global.ts, xlab = "Tiempo", ylab = "Temperatura en °C", 
     main = "Serie de Temperatura Global",
     sub = "Serie mensual: Enero de 1856 a Diciembre de 2005")
plot(Global.annual, xlab = "Tiempo", ylab = "Temperatura en °C", 
     main = "Serie de Temperatura Global",
     sub = "Serie anual de temperaturas medias: 1856 a 2005")
mean(Global.annual)

Global.ar <- ar(Global.annual, method = "mle")
Global.ar$order
Global.ar$ar
acf(Global.ar$res[-(1:Global.ar$order)], lag = 50, main = "")
title(main = "Correlograma de la serie de residuales",
      sub = "Modelo AR(4) ajustado a la serie de temperaturas globales anuales")

# Modelos MA(q)

# Ejemplos en R: Correlograma y Simulación

# Función en R para calcular la Función de Autocorrelación

rho <- function(k, beta){
      q <- length(beta) - 1
      if(k > q) ACF <- 0 else {
            s1 <- 0; s2 <- 0
            for(i in 1:(q-k+1)) s1 <- s1 + beta[i]*beta[i + k]
            for(i in 1:(q+1)) s2 <- s2 + beta[i]^2
            ACF <- s1/s2}
      ACF}

# Correlograma para un proceso MA(3)

beta <- c(1, 0.7, 0.5, 0.2)
rho.k <- rep(1, 10)
for(k in 1:10) rho.k[k] <- rho(k, beta)
plot(0:10, c(1, rho.k), ylab = expression(rho[k]), xlab = "lag k", type = "h",
     sub = expression(x[t] == w[t] + 0.7*w[t-1] + 0.5*w[t-2] + 0.2*w[t-3]),
     main = "Función de autocorrelación para un proceso MA(3)")
abline(0, 0)

# Correlograma para otro proceso MA(3)

beta <- c(1, -0.7, 0.5, -0.2)
rho.k <- rep(1, 10)
for(k in 1:10) rho.k[k] <- rho(k, beta)
plot(0:10, c(1, rho.k), ylab = expression(rho[k]), xlab = "lag k", type = "h",
     sub = expression(x[t] == w[t] - 0.7*w[t-1] + 0.5*w[t-2] - 0.2*w[t-3]),
     main = "Función de autocorrelación para un proceso MA(3)")
abline(0, 0)

# Simulación de un proceso MA(3)

set.seed(1)
b <- c(0.8, 0.6, 0.4)
x <- w <- rnorm(1000)
for(t in 4:1000){
      for(j in 1:3) x[t] <- x[t] + b[j]*w[t-j]
}

plot(x, type = "l", ylab = expression(x[t]), xlab = "Tiempo t",
     sub = expression(x[t] == w[t] + 0.8*w[t-1] + 0.6*w[t-2] + 0.4*w[t-3]),
     main = "Serie de tiempo simulada de un proceso MA(3)")
acf(x, main = "")
title(main = "Correlograma para un proceso MA(3) simulado", 
      sub = expression(x[t] == w[t] + 0.8*w[t-1] + 0.6*w[t-2] + 0.4*w[t-3]))

# Ajuste de modelos MA

x.ma <- arima(x, order = c(0, 0, 3))
x.ma

# Modelos ARMA(p, q)

# Simulación y ajuste

set.seed(1)
x <- arima.sim(n = 10000, list(ar = -0.6, ma = 0.5))
plot(x[1:100], type = "l", xlab = "")
title(main = "Serie simulada", xlab = "Tiempo", 
      sub = expression(x[t] == -0.6*x[t-1] + w[t] + 0.5*w[t-1]))
coef(arima(x, order = c(1, 0, 1)))

# Predicción
# Serie de producción de electricidad

CBE <- read.csv("cbe.csv", header = TRUE)
Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
plot(Elec.ts, xlab = "", ylab = "")
title(main = "Serie de Producción de Electricidad",
      xlab = "Tiempo",
      ylab = "Producción de electricidad")

plot(log(Elec.ts), xlab = "", ylab = "")
title(main = "Serie-log de Producción de Electricidad",
      xlab = "Tiempo",
      ylab = "Log de Producción de electricidad")

Time <- 1:length(Elec.ts)
Imth <- cycle(Elec.ts)
Elec.lm <- lm(log(Elec.ts) ~ Time + I(Time^2) + factor(Imth))

acf(resid(Elec.lm), main = "")
title(main = "Correlograma de la serie de residuales del modelo de regresión",
      sub = "Serie de producción de electricidad")
plot(resid(Elec.lm), type = "l", main = "", xlab = "", ylab = "")
title(main = "Serie de residuales del modelo de regresión ajustado",
      sub = "Serie de producción de electricidad",
      xlab = "Tiempo",
      ylab = "Residuales")

# Código para encontrar el mejor modelo ARMA(p, q) considerando el AIC (Akaike
# Information Criterion)

best.order <- c(0, 0, 0)
best.aic <- Inf
for(i in 0:2)for(j in 0:2){
      model <- arima(resid(Elec.lm), order = c(i, 0, j))
      fit.aic <- AIC(model)
      if(fit.aic < best.aic){
            best.order <- c(i, 0, j)
            best.arma <- arima(resid(Elec.lm), order = best.order)
            best.aic <- fit.aic
      }
}

best.order
acf(resid(best.arma), main = "")
title(main = "Serie de residuales del modelo ARMA(2, 0) ajustado",
      sub = "Serie de residuales del modelo de regresión ajustado a los datos de electricidad")
new.time <- seq(length(Elec.ts)+1, length = 36)
new.data <- data.frame(Time = new.time, Imth = rep(1:12, 3))
predict.lm <- predict(Elec.lm, new.data)
predict.arma <- predict(best.arma, n.ahead = 36)
elec.pred <- ts(exp(predict.lm + predict.arma$pred), start = 1991, freq = 12)

ts.plot(cbind(Elec.ts, elec.pred), lty = 1:2, 
        col = c("blue", "red"), xlab = "Tiempo", 
        ylab = "Producción de electricidad",
        main = "Predicción de los datos de producción de electricidad",
        sub = "Predicción de 36 meses")

# Reto 1: Proceso AR(1) ----

# 1. Simula un proceso AR(1) de la forma x[t] = 0.5 * x[t-1] + w[t] para 
#    t = 1, 2, ..., 200 y muestra gráficamente la serie de tiempo obtenida

x <- w <- rnorm(200)
for(t in 2:200) x[t] <- 0.5 * x[t-1] + w[t]
plot(x, type = "l", xlab = "", ylab = "")
title(main = "Proceso AR(1) simulado",
      xlab = "Tiempo",
      ylab = expression(x[t]),
      sub = expression(x[t]==0.5*x[t-1]+w[t]))

# 2. Obtén el correlograma y el correlograma parcial del proceso AR(1) simulado

acf(x, main = "")
title(main = "Correlograma del proceso AR(1) simulado", 
      sub = expression(x[t]==0.5*x[t-1]+w[t]))
pacf(x, main = "")
title(main = "Correlograma Parcial del proceso AR(1) simulado", 
      sub = expression(x[t]==0.5*x[t-1]+w[t]))

# 3. Ajusta un modelo autorregresivo a la serie simulada utilizando la función ar, 
#    observa el orden del modelo y el parámetro estimado (los parámetros estimados)

x.ar <- ar(x, method = "mle")
x.ar$order
x.ar$ar # Estimación puntual
x.ar$ar + c(-2, 2)*sqrt(x.ar$asy.var) # Intervalo de confianza


# Ejemplo 3. Modelos no estacionarios y predicción ----

# Tomamos datos de https://github.com/AtefOuni/ts/tree/master/Data

# Serie de producción de electricidad de Australia

CBE <- read.csv("cbe.csv", header = TRUE)
Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
plot(Elec.ts, xlab = "", ylab = "")
title(main = "Serie de Producción de Electricidad Australiana",
      ylab = "Producción de electricidad (GWh)",
      xlab = "Tiempo")

plot(diff(Elec.ts), xlab = "", ylab = "")
title(main = "Serie Diferenciada de Producción de Electricidad Australiana",
      xlab = "Tiempo", ylab = "Dif Serie",
      sub = "Gráfica de la serie diferenciada de primer órden")

plot(diff(log(Elec.ts)), xlab = "", ylab = "")
title(main = "Serie de log dif de Producción de Electricidad Australiana",
      xlab = "Tiempo", ylab = "Dif log-Serie",
      sub = "Gráfica de la serie log-transformada diferenciada de primer órden")

# Simulación y ajuste

# A continuación, simulamos datos de un modelo ARIMA(1, 1, 1) y luego ajustamos
# un modelo a la serie simulada para recuperar los parámetros estimados.

set.seed(1)
x <- w <- rnorm(1000)
for(i in 3:1000) x[i] <- 0.5*x[i-1] + x[i-1] - 0.5*x[i-2] + w[i] + 0.3*w[i-1]
plot(x, type = "l", 
     main = "Serie simulada de un modelo ARIMA(1, 1, 1)",
     xlab = "Tiempo",
     ylab = expression(x[t]),
     sub = expression(x[t] == 0.5*x[t-1] + x[t-1] - 0.5*x[t-2] + w[t] + 0.3*w[t-1]))
arima(x, order = c(1, 1, 1))

# Simulación con la función arima.sim

x <- arima.sim(model = list(order = c(1, 1, 1), ar = 0.5, ma = 0.3), n = 1000)
arima(x, order = c(1, 1, 1))

# Serie de producción de cerveza

CBE <- read.csv("cbe.csv", header = TRUE)
Beer.ts <- ts(CBE[, 2], start = 1958, freq = 12)
plot(Beer.ts, xlab = "", ylab = "")
title(main = "Serie de Producción de Cerveza en Australia",
      ylab = "Producción de Cerveza (Megalitros)",
      xlab = "Mes")

Beer.ima <- arima(Beer.ts, order = c(0, 1, 1))
Beer.ima

acf(resid(Beer.ima), main = "")
title(main = "Autocorrelaciones para los Residuales del Ajuste",
      sub = expression(x[t]==x[t-1]+w[t]-0.33*w[t-1]))
Beer.1991 <- predict(Beer.ima, n.ahead = 12)
sum(Beer.1991$pred)

# Modelos Arima estacionales
# Procedimiento de ajuste Serie de producción de electricidad de Australia

CBE <- read.csv("cbe.csv", header = TRUE)
Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
plot(Elec.ts, xlab = "", ylab = "")
title(main = "Serie de Producción de Electricidad Australiana",
      ylab = "Producción de electricidad (GWh)",
      xlab = "Tiempo")

plot(log(Elec.ts), xlab = "", ylab = "")
title(main = "Log de Serie de Producción de Electricidad Australiana",
      ylab = "Log de Producción de electricidad (GWh)",
      xlab = "Tiempo")
Elec.AR <- arima(log(Elec.ts), order = c(1, 1, 0), 
                 seas = list(order = c(1, 0, 0), 12))

Elec.MA <- arima(log(Elec.ts), order = c(0, 1, 1),
                 seas = list(order = c(0, 0, 1), 12))


AIC(Elec.AR)
AIC(Elec.MA)

# Función para buscar un buen modelo

get.best.arima <- function(x.ts, maxord = c(1, 1, 1, 1, 1, 1)){
      best.aic <- 1e8
      n <- length(x.ts)
      for(p in 0:maxord[1])for(d in 0:maxord[2])for(q in 0:maxord[3])
            for(P in 0:maxord[4])for(D in 0:maxord[5])for(Q in 0:maxord[6])
            {
                  fit <- arima(x.ts, order = c(p, d, q),
                               seas = list(order = c(P, D, Q),
                                           frequency(x.ts)), method = "CSS")
                  fit.aic <- -2*fit$loglik + (log(n) + 1)*length(fit$coef)
                  if(fit.aic < best.aic){
                        best.aic <- fit.aic
                        best.fit <- fit
                        best.model <- c(p, d, q, P, D, Q)
                  }
            }
      list(best.aic, best.fit, best.model)
}

# Nuevo ajuste a los datos de la serie transformada de producción de electricidad

best.arima.elec <- get.best.arima(log(Elec.ts),
                                  maxord = c(2, 2, 2, 2, 2, 2))

best.fit.elec <- best.arima.elec[[2]]  # Modelo
best.arima.elec[[3]] # Tipo de modelo (órdenes)
best.fit.elec
best.arima.elec[[1]] # AIC

# ACF para residuales del ajuste

acf(resid(best.fit.elec), main = "")
title(main = "Correlograma de los residuales del ajuste")

# Predicción

pr <- predict(best.fit.elec, 12)$pred 
ts.plot(cbind(window(Elec.ts, start = 1981),
              exp(pr)), col = c("blue", "red"), xlab = "")
title(main = "Predicción para la serie de producción de electricidad",
      xlab = "Mes",
      ylab = "Producción de electricidad (GWh)")


# Reto 2. Simulación de un proceso ARIMA(1, 1, 1) ----

# Realiza la siguiente simulación con las siguientes características: n = 1000
# valores de un proceso ARIMA(1, 1, 1) con parámetros ar = 0.6 y ma = 0.2

x <- arima.sim(model = list(order = c(1, 1, 1), ar = 0.6, ma = 0.2), n = 1000)

# Ajusta un modelo Arima a la serie simulada para estimar los parámetros y
# observe las estimaciones de los parámetros

modelo <- arima(x, order = c(1, 1, 1))
modelo$coef


# Obtén el correlograma de los residuales del ajuste
acf(modelo$residuals, main = "Autocorrelaciónes para los Residuales del Ajuste")

# Realiza tres predicciones con ayuda del modelo ajustado y la función predict
prediccion <- predict(modelo, n.ahead = 3)
prediccion$pred

