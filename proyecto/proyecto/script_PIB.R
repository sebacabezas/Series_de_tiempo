PIBE = rio::import("PIB_historico.xlsx")

aux = PIBE$ROMI_PIB[1:28]

plot(aux, type = "l")

plot(PIBE$`Desestacionalizado (variación trimestral)`, type = "l")

#aux = ts(PIBE$`Serie original (variación anual)`, freq = 4)
aux = ts(aux, freq = 4)



aux2 = periodogram(aux)
plot(aux2$periodogram, type = "l")
abline(v = 2)
fit = auto.arima(aux, d = 1, D = 1)
fit

forecast::ndiffs(aux)
forecast::nsdiffs(diff(aux))

fit <- forecast::Arima(aux, 
                       order = c(2,0,21), 
                       seasonal = c(0,0,1),
                       fixed = c(NA,NA,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,0,0,NA,NA,NA,NA))
fit

TS.diag(fit$res)


pre <- forecast::forecast(fit, h = 40, level = 0.95)
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre)

lines(aux, col = "red")
