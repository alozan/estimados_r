# Librerias
library(dplyr)
library(DBI)
library(RMySQL)
library(ggplot2)
library(forecast)

# Se generan series temporales

f = file.choose()
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
     sub = "Descomposici?n de los datos de uso de AMD")

gpu2.decom.M = decompose(gpu2.ts, type = "mult")

plot(gpu.decom.M, xlab = "Tiempo", 
     sub = "Descomposici?n de los datos de uso de Intel")

gpu3.decom.M = decompose(gpu3.ts, type = "mult")

plot(gpu.decom.M, xlab = "Tiempo", 
     sub = "Descomposici?n de los datos de uso de Nvidia")

gpu4.decom.M = decompose(gpu3.ts, type = "mult")

plot(gpu.decom.M, xlab = "Tiempo", 
     sub = "Descomposici?n de los datos de uso de Otras tarjetas")

#Valor estacional con tendencia
Trend = gpu.decom.M$trend
Seasonal = gpu.decom.M$seasonal
Random = gpu.decom.M$random

ts.plot1(cbind(Trend, Trend*Seasonal), xlab = "Tiempo", main = "Datos de uso de AMD", 
        ylab = "AMD", lty = 1:2,
        sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")

Trend[7]*Seasonal[7]*Random[7]
gpu.ts[7]

Trend[100]*Seasonal[100]*Random[100]
gpu.ts[100]

Trend2 = gpu2.decom.M$trend
Seasonal2 = gpu2.decom.M$seasonal
Random2 = gpu2.decom.M$random

ts.plot2(cbind(Trend2, Trend2*Seasonal2), xlab = "Tiempo", main = "Datos de uso de AMD", 
        ylab = "Intel", lty = 1:2,
        sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")

Trend2[7]*Seasonal2[7]*Random2[7]
gpu2.ts[7]

Trend2[100]*Seasonal2[100]*Random2[100]
gpu2.ts[100]

Trend3 = gpu3.decom.M$trend
Seasonal3 = gpu3.decom.M$seasonal
Random3 = gpu3.decom.M$random

ts.plot3(cbind(Trend3, Trend3*Seasonal3), xlab = "Tiempo", main = "Datos de uso de AMD", 
        ylab = "Nvidia", lty = 1:2,
        sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")

Trend3[7]*Seasonal3[7]*Random3[7]
gpu3.ts[7]

Trend3[100]*Seasonal3[100]*Random3[100]
gpu3.ts[100]

Trend4 = gpu4.decom.M$trend
Seasonal4 = gpu4.decom.M$seasonal
Random4 = gpu4.decom.M$random

ts.plot4(cbind(Trend4, Trend4*Seasonal4), xlab = "Tiempo", main = "Datos de uso de AMD", 
         ylab = "Nvidia", lty = 1:2,
         sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")

Trend4[7]*Seasonal4[7]*Random4[7]
gpu4.ts[7]

Trend4[100]*Seasonal4[100]*Random4[100]
gpu4.ts[100]

# 4. Modelo Arima a 12 meses


arima560 = auto.arima(gpu.ts, stepwise = F,
                      approximation = F,
                      trace = T)
print(arima560)
proy = forecast(arima560,d=1,D=1, h = 12, level = c(12))
plot(proy)
autoplot(proy)
checkresiduals(proy)


arima570 = auto.arima(gpu2.ts, stepwise = F,
                      approximation = F,
                      trace = T)
print(arima570)
proy2 = forecast(arima570,d=1,D=1, h = 12, level = c(12))
plot(proy2)
autoplot(proy2)
checkresiduals(proy2)


arima580 = auto.arima(gpu3.ts, stepwise = F,
                      approximation = F,
                      trace = T)
print(arima580)
proy3 = forecast(arima580,d=1,D=1, h = 12, level = c(12))
plot(proy3)
autoplot(proy3)
checkresiduals(proy3)


arima590 = auto.arima(gpu4.ts, stepwise = F,
                      approximation = F,
                      trace = T)
print(arima590)
proy4 = forecast(arima590,d=1,D=1, h = 12, level = c(12))
plot(proy4)
autoplot(proy4)
checkresiduals(proy4)
