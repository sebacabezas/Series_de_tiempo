library(tidyverse)
library(forecast)
library(LSTS)
library(splines)



# Variables ---------------------------------------------------------------

df = rio::import("data.xlsx")

lr <- df$Dx_6_L_R
r_500 <- df$Dx_6_R500

Y <- ts(df$Dx_6_R500, start = c(2016,1), frequency = 12, end = c(2021,12))

# df$tiempo = c(1:72, rep(NA, 108))
# 
# library(lmtest)
# bptest(lm(Y[1:72]~df$tiempo[1:72])) # No rechazamos la no homocedasticidad
# # De igual manera intentamos
# # Box cox
# par(mfrow = c(1,1))
# plot(BoxCox(Y, lambda = "auto"))
# library(car)
# lambda <- BoxCox.lambda(Y)
# boxCox(lm(Dx_6_L_R ~ tiempo, data = df))
# lynx.fit <- ar(BoxCox(Y,lambda))
# plot(forecast(lynx.fit,h=20,lambda=lambda))
# # No se hace transformación ya que contiene al 1
# 
# ''
# # consideramos un arfima y lo descartamos por el acf
# 
# ''
# 
# 
# 
# lr.ts <- ts(lr,start=c(2016,01), end=c(2021,12), frequency = 12)
# plot(lr.ts)
# 
# par(mfrow = c(1,1))
# acf(lr.ts,lag=100)
# pacf(lr.ts,lag=100)
# 
# r500.ts <- ts(r_500,start=c(2016,01), end=c(2021,12), frequency = 12)
# plot(r500.ts)
# acf(r500.ts,lag=100)
# pacf(r500.ts,lag=100)
# par(mfrow = c(1,1))
# 
# mod1_lr <- auto.arima(lr.ts) #Todas las variables son significativas
# mod1_r500 <- auto.arima(r500.ts) #Todas las variables son significativas
# 
# # -------------------------------------------------------------------------
# 
# 
# library(lmtest)
# bptest(lm(Y[1:72]~df$tiempo[1:72])) # No rechazamos la no homocedasticidad
# # De igual manera intentamos
# # Box cox
# Y = diff(Y)
# Y = Y[13:72]
# Y = ts(Y)
# Y = abs(Y)
# par(mfrow = c(1,1))
# plot(BoxCox(Y, lambda = "auto"))
# library(car)
# lambda <- BoxCox.lambda(Y)
# boxCox(lm(Dx_6_L_R ~ tiempo, data = df))
# lynx.fit <- ar(BoxCox(Y,lambda))
# plot(forecast(lynx.fit,h=20,lambda=lambda))
# 
# # -------------------------------------------------------------------------
# 
# plot(periodogram(r500.ts)$plot)
# 
# aux = periodogram(diff(r500.ts))
# 
# plot(diff(r500.ts))
# 
# periodogram(diff(lr.ts))
# 
# par(mfrow = c(1,1))
# plot(aux$periodogram, type = "l")
# abline(v = 30)
# abline(v = 12)
# 
# aux2 = periodogram(diff(diff(lr.ts),lag = 12, differences = 1))
# plot(aux2$periodogram, type = "l")
# abline(v = 2)
# 
# 
# #diferenciación ----------------------------------------------------------
# 
# Y <- ts(df$Dx_6_R500[1:72], start = c(2016,1), frequency = 12, end = c(2021,12))
# #Y <- ts(df$Dx_6_L_R[1:70])
# # d
# forecast::ndiffs(Y)
# plot(diff(Y), col = "gray", lwd = 1, type = "l")
# acf(diff(Y), na.action = na.pass, lag.max = 60)
# pacf(diff(Y), lag.max = 60)
# 
# # ar(1)
# # D
# 
# forecast::nsdiffs(diff(Y))
# plot(diff(Y, lag = frequency(Y)), col = "gray", lwd = 3)
# acf(diff(Y, lag = frequency(Y)), na.action = na.pass, lag.max = 60)
# pacf(diff(Y, lag = frequency(Y)), na.action = na.pass, lag.max = 60)
# 
# # -------------------------------------------------------------------------
# # PRueba
# 
# par(mfrow = c(1,1))
# library(PSF)
# auxiliar <- psf(Y)
# summary(auxiliar)
# 
# pred <- predict(auxiliar, n.ahead = 12)
# plot(auxiliar, pred)
# 
# # -------------------------------------------------------------------------


exogenas1 = data.frame("IMACEC" = df$IMACEC_forecast[1:72],
                       "Poblacion" = df$Poblacion_comuna[1:72],
                       "PIB" = df$PIB_var_an_porc[1:72])
exogenas2 = data.frame("IMACEC" = df$IMACEC_forecast[73:length(df$IMACEC_forecast)],
                       "Poblacion" = df$Poblacion_comuna[73:length(df$IMACEC_forecast)],
                       "PIB" = df$PIB_var_an_porc[73:length(df$PIB_var_an_porc)])


#fit <- forecast::auto.arima(Y, max.d = 6, max.D = 6, max.p = 6, max.q = 6, 
#                            max.P = 6, max.Q = 6, lambda = NULL,
#                            xreg = as.matrix(exogenas1[,2]))
fit <- forecast::auto.arima(Y, max.d = 6, max.D = 6, max.p = 6, max.q = 6, 
                            max.P = 6, max.Q = 6, lambda = NULL)

summary(fit)
TS.diag(fit$res)
pre <- forecast::forecast(fit, h = 100, level = 0.95)
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre)

lines(Y, col = "red")



# -------------------------------------------------------------------------



fit1 <- forecast::Arima(Y, 
                       order = c(1,0,6), 
                       seasonal = c(1,1,0),
                       include.drift = TRUE,
                       fixed = c(NA,NA,NA,NA,0,0,NA,NA,NA))

TS.diag(fit1$res)

pre1 <- forecast::forecast(fit1, h = 100, level = 0.95)
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre1)

lines(Y, col = "red")



# -------------------------------------------------------------------------




fit2 <- forecast::Arima(Y, 
                        order = c(1,0,3), 
                        seasonal = c(1,1,0),
                        include.drift = TRUE,
                        fixed = c(NA,0,0,NA,NA, NA))


fit2
TS.diag(fit2$res)

pre2 <- forecast::forecast(fit2, h = 100, level = 0.95)
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre2)

lines(Y, col = "red")



# -------------------------------------------------------------------------

fit3 <- forecast::auto.arima(Y, d = 1, D = 1, max.p = 6, max.q = 6, 
                            max.P = 6, max.Q = 6, lambda = NULL)

fit3

fit3 <- forecast::Arima(Y, 
                        order = c(1,1,3), 
                        seasonal = c(1,1,0),
                        include.drift = FALSE,
                        fixed = c(NA,NA,NA,NA,NA))



fit3
TS.diag(fit3$res)

pre3 <- forecast::forecast(fit3, h = 100, level = 0.95)
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre3)

lines(Y, col = "red")


# -------------------------------------------------------------------------


fit5 <- forecast::auto.arima(Y, max.d = 1, max.D = 1, max.p = 6, max.q = 6, 
                             max.P = 6, max.Q = 6, lambda = NULL,
                             allowdrift = TRUE)
fit5 = forecast::auto.arima(Y)

fit5

fit5 <- forecast::Arima(Y, 
                        order = c(1,0,7), 
                        seasonal = c(1,1,0),
                        include.drift = TRUE,
                        fixed = c(NA,NA,NA,NA,0,0,NA,NA,NA,NA))


TS.diag(fit5$res)

pre5 <- forecast::forecast(fit5, h = 100, level = 0.95)
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre5)

lines(Y, col = "red")





# -------------------------------------------------------------------------

exogenas1 = data.frame("IMACEC" = df$IMACEC_forecast[1:72],
                       "Poblacion" = df$Poblacion_comuna[1:72],
                       "PIB" = df$PIB_var_an_porc[1:72],
                       "auxiliar" = df$Dx_6_L_R[1:72])
exogenas2 = data.frame("IMACEC" = df$IMACEC_forecast[73:length(df$IMACEC_forecast)],
                       "Poblacion" = df$Poblacion_comuna[73:length(df$IMACEC_forecast)],
                       "PIB" = df$PIB_var_an_porc[73:length(df$PIB_var_an_porc)],
                       "auxiliar" = auxiliar2)

#exogenas1 = data.frame("IMACEC" = df$IMACEC_forecast[1:60],
#                       "Poblacion" = df$Poblacion_comuna[1:60],
#                       "PIB" = df$PIB_var_an_porc[1:60])
#exogenas2 = data.frame("IMACEC" = df$IMACEC_forecast[61:length(df$IMACEC_forecast)],
#                       "Poblacion" = df$Poblacion_comuna[61:length(df$IMACEC_forecast)],
#                       "PIB" = df$PIB_var_an_porc[61:length(df$PIB_var_an_porc)])


Y <- ts(df$Dx_6_R500[1:72], start = c(2016,1), frequency = 12, end = c(2021,12))

fit4 <- forecast::auto.arima(Y, max.d = 1, max.D = 1, max.p = 6, max.q = 6, 
                             max.P = 6, max.Q = 6, lambda = NULL, xreg = as.matrix(exogenas1[,2]))

fit4

fit4 <- forecast::Arima(Y, 
                        order = c(1,0,4), 
                        seasonal = c(1,1,0),
                        include.drift = TRUE,
                        fixed = c(NA,NA,NA,0,NA,NA,NA,NA),
                        xreg = as.matrix(exogenas1[,2]))


fit4
TS.diag(fit4$res)

#fit4 <- forecast::Arima(Y, 
#                        order = c(1,0,11), 
#                        seasonal = c(1,1,0),
#                        include.drift = TRUE,
#                        fixed = c(NA,NA,NA,NA,0,0,0,0,0,0,0,NA,NA,NA, NA),
#                        xreg = as.matrix(exogenas1[,2]))


fit4
TS.diag(fit4$res)

pre4 <- forecast::forecast(fit4, level = 0.95, xreg = as.matrix(exogenas2[,2]))
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre4)

# -------------------------------------------------------------------------


lines(Y, col = "red")



# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------




source("TS.diag.R")
TS.diag(fit$res)


shapiro.test(fit$residuals) # Los residuos no son normales, osea, rechazamos la hipotesis de normalidad



ks.test(fit$residuals, pnorm) # Los residuos no son normales, osea, rechazamos la hipotesis de normalidad

library(nortest)
lillie.test(fit$residuals)


# predicción --------------------------------------------------------------
# Predicción

pre <- forecast::forecast(fit, h = 100, level = 0.95)
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre)

lines(Y, col = "red")



# -------------------------------------------------------------------------

Y <- ts(df$Dx_6_R500[1:60], start = c(2016,1), frequency = 12, end = c(2020,12))

fit <- forecast::auto.arima(Y, max.d = 6, max.D = 6, max.p = 6, max.q = 6, 
                            max.P = 6, max.Q = 6, lambda = NULL)

summary(fit)
TS.diag(fit$res)
pre <- forecast::forecast(fit, h = 100, level = 0.95)
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre)

lines(Y, col = "red")

Y <- ts(df$Dx_6_R500[1:72], start = c(2016,1), frequency = 12, end = c(2021,12))

val_cruzada = sum(abs(pre$fitted[1:12]-Y[61:72]))/12





##### 
Y <- ts(df$Dx_6_R500[1:60], start = c(2016,1), frequency = 12, end = c(2020,12))
fit1 <- forecast::Arima(Y, 
                        order = c(1,0,6), 
                        seasonal = c(1,1,0),
                        include.drift = TRUE,
                        fixed = c(NA,NA,NA,NA,0,0,NA,NA,NA))

TS.diag(fit1$res)

pre1 <- forecast::forecast(fit1, h = 100, level = 0.95)
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre1)

lines(Y, col = "red")

Y <- ts(df$Dx_6_R500[1:72], start = c(2016,1), frequency = 12, end = c(2021,12))

val_cruzada1 = sum(abs(pre2$fitted[1:12]-Y[61:72]))/12

#######


Y <- ts(df$Dx_6_R500[1:60], start = c(2016,1), frequency = 12, end = c(2020,12))
fit2 <- forecast::Arima(Y, 
                        order = c(1,0,3), 
                        seasonal = c(1,1,0),
                        include.drift = TRUE,
                        fixed = c(NA,0,0,NA,NA, NA))


fit2
TS.diag(fit2$res)

pre2 <- forecast::forecast(fit2, h = 100, level = 0.95)
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre2)

lines(Y, col = "red")

Y <- ts(df$Dx_6_R500[1:72], start = c(2016,1), frequency = 12, end = c(2021,12))

val_cruzada2 = sum(abs(pre2$fitted[1:12]-Y[61:72]))/12

####
Y <- ts(df$Dx_6_R500[1:60], start = c(2016,1), frequency = 12, end = c(2020,12))

fit3 <- forecast::auto.arima(Y, d = 1, D = 1, max.p = 6, max.q = 6, 
                             max.P = 6, max.Q = 6, lambda = NULL)

fit3

fit3 <- forecast::Arima(Y, 
                        order = c(1,1,3), 
                        seasonal = c(1,1,0),
                        include.drift = FALSE,
                        fixed = c(NA,NA,NA,NA,NA))



fit3
TS.diag(fit3$res)

pre3 <- forecast::forecast(fit3, h = 100, level = 0.95)
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre3)

lines(Y, col = "red")

Y <- ts(df$Dx_6_R500[1:72], start = c(2016,1), frequency = 12, end = c(2021,12))

val_cruzada3 = sum(abs(pre3$fitted[1:12]-Y[61:72]))/12

### 

Y <- ts(df$Dx_6_R500[1:60], start = c(2016,1), frequency = 12, end = c(2020,12))
fit5 <- forecast::auto.arima(Y, max.d = 1, max.D = 1, max.p = 6, max.q = 6, 
                             max.P = 6, max.Q = 6, lambda = NULL,
                             allowdrift = TRUE)
fit5 = forecast::auto.arima(Y)

fit5

fit5 <- forecast::Arima(Y, 
                        order = c(1,0,7), 
                        seasonal = c(1,1,0),
                        include.drift = TRUE,
                        fixed = c(NA,NA,NA,NA,0,0,NA,NA,NA,NA))


TS.diag(fit5$res)

pre5 <- forecast::forecast(fit5, h = 100, level = 0.95)
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre5)

lines(Y, col = "red")

Y <- ts(df$Dx_6_R500[1:72], start = c(2016,1), frequency = 12, end = c(2021,12))


val_cruzada5 = sum(abs(pre5$fitted[1:12]-Y[61:72]))/12

### 
Y <- ts(df$Dx_6_R500[1:60], start = c(2016,1), frequency = 12, end = c(2020,12))

exogenas1 = data.frame("IMACEC" = df$IMACEC_forecast[1:60],
                       "Poblacion" = df$Poblacion_comuna[1:60],
                       "PIB" = df$PIB_var_an_porc[1:60],
                       "auxiliar" = df$Dx_6_L_R[1:60])
exogenas2 = data.frame("IMACEC" = df$IMACEC_forecast[61:length(df$IMACEC_forecast)],
                       "Poblacion" = df$Poblacion_comuna[61:length(df$IMACEC_forecast)],
                       "PIB" = df$PIB_var_an_porc[61:length(df$PIB_var_an_porc)],
                       "auxiliar" = auxiliar2)

#exogenas1 = data.frame("IMACEC" = df$IMACEC_forecast[1:60],
#                       "Poblacion" = df$Poblacion_comuna[1:60],
#                       "PIB" = df$PIB_var_an_porc[1:60])
#exogenas2 = data.frame("IMACEC" = df$IMACEC_forecast[61:length(df$IMACEC_forecast)],
#                       "Poblacion" = df$Poblacion_comuna[61:length(df$IMACEC_forecast)],
#                       "PIB" = df$PIB_var_an_porc[61:length(df$PIB_var_an_porc)])



fit4 <- forecast::auto.arima(Y, max.d = 1, max.D = 1, max.p = 6, max.q = 6, 
                             max.P = 6, max.Q = 6, lambda = NULL, xreg = as.matrix(exogenas1[,2]))

fit4

fit4 <- forecast::Arima(Y, 
                        order = c(1,0,4), 
                        seasonal = c(1,1,0),
                        include.drift = TRUE,
                        fixed = c(NA,NA,NA,0,NA,NA,NA,NA),
                        xreg = as.matrix(exogenas1[,2]))


fit4
TS.diag(fit4$res)

#fit4 <- forecast::Arima(Y, 
#                        order = c(1,0,11), 
#                        seasonal = c(1,1,0),
#                        include.drift = TRUE,
#                        fixed = c(NA,NA,NA,NA,0,0,0,0,0,0,0,NA,NA,NA, NA),
#                        xreg = as.matrix(exogenas1[,2]))


fit4
TS.diag(fit4$res)

pre4 <- forecast::forecast(fit4, level = 0.95, xreg = as.matrix(exogenas2[,2]))
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre4)

Y <- ts(df$Dx_6_R500[1:72], start = c(2016,1), frequency = 12, end = c(2021,12))

lillie.test(fit4$residuals)

val_cruzada4 = sum(abs(pre4$fitted[1:12]-Y[61:72]))/12


# -------------------------------------------------------------------------

# ajustes -----------------------------------------------------------------

fit
fit1
fit2
fit3
fit4
fit5

fit$aic
fit1$aic
fit2$aic
fit3$aic #*
fit4$aic
fit5$aic

summary(fit)
summary(fit1) #MAPE = 2.0988 RMSE = 5526
summary(fit2) #MAPE = 2.1171 RMSE = 5601
summary(fit3) #MAPE = 2.0253 RMSE = 5587 *
summary(fit4) #MAPE = 2.3618 RMSE = 5549
summary(fit5) #MAPE = 2.0454 RMSE = 5465 *

plot(pre , xlim = c(2016,2031), ylim = c(50000, 250000))
plot(pre1, xlim = c(2016,2031), ylim = c(50000, 250000))
plot(pre2, xlim = c(2016,2031), ylim = c(50000, 250000))
plot(pre3, xlim = c(2016,2031), ylim = c(50000, 250000))
plot(pre4, xlim = c(2016,2031), ylim = c(50000, 250000))
plot(pre5, xlim = c(2016,2031), ylim = c(50000, 250000))

TS.diag(fit$residuals )
TS.diag(fit1$residuals)
TS.diag(fit2$residuals)
TS.diag(fit3$residuals)
TS.diag(fit4$residuals)
TS.diag(fit5$residuals)

lillie.test(fit$residuals )
lillie.test(fit1$residuals)
lillie.test(fit2$residuals)
lillie.test(fit3$residuals)
lillie.test(fit4$residuals)
lillie.test(fit5$residuals)

# Todos son homocedasticos
bptest(lm( fit$residuals~df$tiempo[1:72]))
bptest(lm(fit1$residuals~df$tiempo[1:72]))
bptest(lm(fit2$residuals~df$tiempo[1:72]))
bptest(lm(fit3$residuals~df$tiempo[1:72]))
bptest(lm(fit4$residuals~df$tiempo[1:72]))
bptest(lm(fit5$residuals~df$tiempo[1:72]))

val_cruzada
val_cruzada1
val_cruzada2
val_cruzada3
val_cruzada4
val_cruzada5

par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)

Y <- ts(df$Dx_6_R500[1:60], start = c(2016,1), frequency = 12, end = c(2020,12))
plot(pre, xlim = c(2020.5, 2022), ylim = c(120000, 230000))
Y <- ts(df$Dx_6_R500[1:72], start = c(2016,1), frequency = 12, end = c(2021,12))
lines(Y, col = "red")

Y <- ts(df$Dx_6_R500[1:60], start = c(2016,1), frequency = 12, end = c(2020,12))
plot(pre1, xlim = c(2020.5, 2022), ylim = c(120000, 230000))
Y <- ts(df$Dx_6_R500[1:72], start = c(2016,1), frequency = 12, end = c(2021,12))
lines(Y, col = "red")

Y <- ts(df$Dx_6_R500[1:60], start = c(2016,1), frequency = 12, end = c(2020,12))
plot(pre2, xlim = c(2020.5, 2022), ylim = c(120000, 230000))
Y <- ts(df$Dx_6_R500[1:72], start = c(2016,1), frequency = 12, end = c(2021,12))
lines(Y, col = "red")
# el segundo y el tercero la misma cosa
Y <- ts(df$Dx_6_R500[1:60], start = c(2016,1), frequency = 12, end = c(2020,12))
plot(pre3, xlim = c(2020.5, 2022), ylim = c(120000, 230000))
Y <- ts(df$Dx_6_R500[1:72], start = c(2016,1), frequency = 12, end = c(2021,12))
lines(Y, col = "red")
# El cuarto se parece al 1
Y <- ts(df$Dx_6_R500[1:60], start = c(2016,1), frequency = 12, end = c(2020,12))
plot(pre4, xlim = c(2020.5, 2022), ylim = c(120000, 230000))
Y <- ts(df$Dx_6_R500[1:72], start = c(2016,1), frequency = 12, end = c(2021,12))
lines(Y, col = "red")

Y <- ts(df$Dx_6_R500[1:60], start = c(2016,1), frequency = 12, end = c(2020,12))
plot(pre5, xlim = c(2020.5, 2022), ylim = c(120000, 230000))
Y <- ts(df$Dx_6_R500[1:72], start = c(2016,1), frequency = 12, end = c(2021,12))
lines(Y, col = "red")
