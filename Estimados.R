# Librerias
library(dplyr)
library(DBI)
library(RMySQL)
library(ggplot2)
library(forecast)

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


pdf(file = "E:/Github/Proyecto_R/estimados_r/estimados_r/PDF/Amd.pdf", width = 8,height = 6)
gpu.decom.M = decompose(gpu.ts, type = "mult")

plot(gpu.decom.M, xlab = "Tiempo", 
     sub = "Descomposici?n de los datos de uso de AMD")
dev.off()


pdf(file = "E:/Github/Proyecto_R/estimados_r/estimados_r/PDF/Intel.pdf", width = 8,height = 6)
gpu2.decom.M = decompose(gpu2.ts, type = "mult")

plot(gpu.decom.M, xlab = "Tiempo", 
     sub = "Descomposici?n de los datos de uso de Intel")
dev.off()


pdf(file = "E:/Github/Proyecto_R/estimados_r/estimados_r/PDF/Nvidia.pdf", width = 8,height = 6)
gpu3.decom.M = decompose(gpu3.ts, type = "mult")

plot(gpu.decom.M, xlab = "Tiempo", 
     sub = "Descomposici?n de los datos de uso de Nvidia")
dev.off()


pdf(file = "E:/Github/Proyecto_R/estimados_r/estimados_r/PDF/otros.pdf", width = 8,height = 6)
gpu4.decom.M = decompose(gpu3.ts, type = "mult")

plot(gpu.decom.M, xlab = "Tiempo", 
     sub = "Descomposici?n de los datos de uso de Otras tarjetas")
dev.off()


#Valor estacional con tendencia

pdf(file = "E:/Github/Proyecto_R/estimados_r/estimados_r/PDF/etamd.pdf", width = 8,height = 6)
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
dev.off()


pdf(file = "E:/Github/Proyecto_R/estimados_r/estimados_r/PDF/etintel.pdf", width = 8,height = 6)
Trend2 = gpu2.decom.M$trend
Seasonal2 = gpu2.decom.M$seasonal
Random2 = gpu2.decom.M$random

ts.plot(cbind(Trend2, Trend2*Seasonal2), xlab = "Tiempo", main = "Datos de uso de Intel", 
        ylab = "Intel", lty = 1:2,
        sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")

Trend2[7]*Seasonal2[7]*Random2[7]
gpu2.ts[7]

Trend2[100]*Seasonal2[100]*Random2[100]
gpu2.ts[100]
dev.off()

pdf(file = "E:/Github/Proyecto_R/estimados_r/estimados_r/PDF/etnvidia.pdf", width = 8,height = 6)
Trend3 = gpu3.decom.M$trend
Seasonal3 = gpu3.decom.M$seasonal
Random3 = gpu3.decom.M$random

ts.plot(cbind(Trend3, Trend3*Seasonal3), xlab = "Tiempo", main = "Datos de uso de Nvidia", 
        ylab = "Nvidia", lty = 1:2,
        sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")

Trend3[7]*Seasonal3[7]*Random3[7]
gpu3.ts[7]

Trend3[100]*Seasonal3[100]*Random3[100]
gpu3.ts[100]
dev.off()

pdf(file = "E:/Github/Proyecto_R/estimados_r/estimados_r/PDF/etotro.pdf", width = 8,height = 6)
Trend4 = gpu4.decom.M$trend
Seasonal4 = gpu4.decom.M$seasonal
Random4 = gpu4.decom.M$random

ts.plot(cbind(Trend4, Trend4*Seasonal4), xlab = "Tiempo", main = "Datos de uso de Otros", 
         ylab = "Otros", lty = 1:2,
         sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")

Trend4[7]*Seasonal4[7]*Random4[7]
gpu4.ts[7]

Trend4[100]*Seasonal4[100]*Random4[100]
gpu4.ts[100]
dev.off()

# 4. Modelo Arima a 12 meses


arima560 = auto.arima(gpu.ts, stepwise = F,
                      approximation = F,
                      trace = T)
print(arima560)
proy = forecast(arima560,d=1,D=1, h = 12, level = c(12))
pdf(file = "E:/Github/Proyecto_R/estimados_r/estimados_r/PDF/arimaamd.pdf", width = 8,height = 6)
plot(proy)
autoplot(proy)
dev.off()
checkresiduals(proy)


arima570 = auto.arima(gpu2.ts, stepwise = F,
                      approximation = F,
                      trace = T)
print(arima570)
proy2 = forecast(arima570,d=1,D=1, h = 12, level = c(12))
pdf(file = "E:/Github/Proyecto_R/estimados_r/estimados_r/PDF/arimaintel.pdf", width = 8,height = 6)
plot(proy2)
autoplot(proy2)
dev.off()
checkresiduals(proy2)


arima580 = auto.arima(gpu3.ts, stepwise = F,
                      approximation = F,
                      trace = T)
print(arima580)
proy3 = forecast(arima580,d=1,D=1, h = 12, level = c(12))
pdf(file = "E:/Github/Proyecto_R/estimados_r/estimados_r/PDF/arimanvidia.pdf", width = 8,height = 6)
plot(proy3)
autoplot(proy3)
dev.off()
checkresiduals(proy3)


arima590 = auto.arima(gpu4.ts, stepwise = F,
                      approximation = F,
                      trace = T)
print(arima590)
proy4 = forecast(arima590,d=1,D=1, h = 12, level = c(12))
pdf(file = "E:/Github/Proyecto_R/estimados_r/estimados_r/PDF/arimaotro.pdf", width = 8,height = 6)
plot(proy4)
autoplot(proy4)
dev.off()
checkresiduals(proy4)



