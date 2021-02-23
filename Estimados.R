# Librerias
library(dplyr)
library(DBI)
library(RMySQL)
library(ggplot2)

# Se generan series temporales


sgpu = read.csv("gpu2.csv", header = TRUE)

gpu.ts = ts(sgpu[, 1], start = 2007, freq = 12)

plot(gpu.ts, xlab = "", ylab = "")
title(main = "AMD",
      ylab = "Uso de gpu",
      xlab = "Tiempo")

gpu2.ts = ts(sgpu[, 2], start = 2007, freq = 12)

plot(gpu2.ts, xlab = "", ylab = "")
title(main = "Intel",
      ylab = "Uso de gpu",
      xlab = "Tiempo")


gpu3.ts = ts(sgpu[, 3], start = 2007, freq = 12)

plot(gpu3.ts, xlab = "", ylab = "")
title(main = "Nvidia",
      ylab = "Uso de gpu",
      xlab = "Tiempo")

gpu4.ts = ts(sgpu[, 4], start = 2007, freq = 12)

plot(gpu4.ts, xlab = "", ylab = "")
title(main = "Otros",
      ylab = "Uso de gpu",
      xlab = "Tiempo")

# 2. Modelo multiplicativo

gpu.decom.M = decompose(gpu.ts, type = "mult")

plot(gpu.decom.M, xlab = "Tiempo", 
     sub = "Descomposición de los datos de uso de AMD")

gpu2.decom.M = decompose(gpu2.ts, type = "mult")

plot(gpu.decom.M, xlab = "Tiempo", 
     sub = "Descomposición de los datos de uso de Intel")

gpu3.decom.M = decompose(gpu3.ts, type = "mult")

plot(gpu.decom.M, xlab = "Tiempo", 
     sub = "Descomposición de los datos de uso de Nvidia")

gpu4.decom.M = decompose(gpu3.ts, type = "mult")

plot(gpu.decom.M, xlab = "Tiempo", 
     sub = "Descomposición de los datos de uso de Otras tarjetas")

#Valor estacional con tendencia
Trend = gpu.decom.M$trend
Seasonal = gpu.decom.M$seasonal
Random = gpu.decom.M$random

ts.plot(cbind(Trend, Trend*Seasonal), xlab = "Tiempo", main = "Datos de uso de AMD", 
        ylab = "AMD", lty = 1:2,
        sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")

Trend[7]*Seasonal[7]*Random[7]
gpu.ts[7]

Trend[100]*Seasonal[100]*Random[100]
gpu.ts[100]

Trend2 = gpu2.decom.M$trend
Seasonal2 = gpu2.decom.M$seasonal
Random2 = gpu2.decom.M$random

ts.plot(cbind(Trend2, Trend2*Seasonal2), xlab = "Tiempo", main = "Datos de uso de AMD", 
        ylab = "Intel", lty = 1:2,
        sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")

Trend2[7]*Seasonal2[7]*Random2[7]
gpu2.ts[7]

Trend2[100]*Seasonal2[100]*Random2[100]
gpu2.ts[100]

Trend3 = gpu3.decom.M$trend
Seasonal = gpu3.decom.M$seasonal
Random = gpu3.decom.M$random

ts.plot(cbind(Trend3, Trend3*Seasona3l), xlab = "Tiempo", main = "Datos de uso de AMD", 
        ylab = "Nvidia", lty = 1:2,
        sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")

Trend3[7]*Seasonal3[7]*Random3[7]
gpu3.ts[7]

Trend3[100]*Seasonal3[100]*Random3[100]
gpu3.ts[100]

# 4. Modelo Arima a 12 meses

library(forecast)
arima560 = auto.arima(gpu.ts, stepwise = F,
                      approximation = F,
                      trace = T)
print(arima560)
proy = forecast(arima560,d=1,D=1, h = 12, level = c(12))
plot(proy)
autoplot(proy)
checkresiduals(proy)