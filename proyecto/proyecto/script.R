library(tidyverse)
library(forecast)
library(LSTS)
library(splines)



# Variables ---------------------------------------------------------------

df = rio::import("data.xlsx")

lr <- df$Dx_6_L_R
r_500 <- df$Dx_6_R500

Y <- ts(df$Dx_6_L_R[1:72], start = c(2016,1), frequency = 12, end = c(2021,12))
Y2 <- ts(df$Dx_6_L_R[1:36], start = c(2016,1), frequency = 12, end = c(2018,12))
Y3 <- ts(df$Dx_6_L_R[37:72], start = c(2019,1), frequency = 12, end = c(2021,12))

df$tiempo = c(1:72, rep(NA, 108))

library(lmtest)
bptest(lm(Y[1:72]~df$tiempo[1:72])) # No rechazamos la no homocedasticidad
# De igual manera intentamos
# Box cox
par(mfrow = c(1,1))
plot(BoxCox(Y, lambda = "auto"))
library(car)
lambda <- BoxCox.lambda(Y)
boxCox(lm(Dx_6_L_R ~ tiempo, data = df))
lynx.fit <- ar(BoxCox(Y,lambda))
plot(forecast(lynx.fit,h=20,lambda=lambda))
# No se hace transformación ya que contiene al 1

''
# consideramos un arfima y lo descartamos por el acf

''



lr.ts <- ts(lr,start=c(2016,01), end=c(2021,12), frequency = 12)
plot(lr.ts)

par(mfrow = c(1,1))
acf(lr.ts,lag=100)
pacf(lr.ts,lag=100)

r500.ts <- ts(r_500,start=c(2016,01), end=c(2021,12), frequency = 12)
plot(r500.ts)
acf(r500.ts,lag=100)
pacf(r500.ts,lag=100)
par(mfrow = c(1,1))

mod1_lr <- auto.arima(lr.ts) #Todas las variables son significativas
mod1_r500 <- auto.arima(r500.ts) #Todas las variables son significativas


# -------------------------------------------------------------------------

periodogram(Y)

aux = periodogram(diff(Y))

plot(diff(Y))

periodogram(diff(Y))

par(mfrow = c(1,1))
plot(aux$periodogram, type = "l")
abline(v = 12)
abline(v = 12)

aux2 = periodogram(diff(diff(lr.ts),lag = 12, differences = 1))
plot(aux2$periodogram, type = "l")
abline(v = 2)


#diferenciación ----------------------------------------------------------

Y <- ts(df$Dx_6_L_R[1:82], start = c(2016,1), frequency = 12, end = c(2021,12))
Y <- ts(df$Dx_6_L_R[1:70])
# d
forecast::ndiffs(Y)
plot(diff(Y), col = "gray", lwd = 1, type = "l")
acf(diff(Y), na.action = na.pass, lag.max = 60)
pacf(diff(Y), lag.max = 60)
# D

forecast::nsdiffs(diff(Y))
plot(diff(diff(Y), lag = frequency(Y)), col = "gray", lwd = 3)
acf(diff(diff(Y), lag = frequency(Y)), na.action = na.pass, lag.max = 60)
pacf(diff(diff(Y), lag = frequency(Y)), na.action = na.pass, lag.max = 60)

# -------------------------------------------------------------------------
# PRueba

par(mfrow = c(1,1))
library(PSF)
auxiliar <- psf(Y)
summary(auxiliar)

pred <- predict(auxiliar, n.ahead = 12)
plot(auxiliar, pred)

# -------------------------------------------------------------------------

# test de cusum o cussum

cusum(Y)

#exogenas1 = data.frame("Poblacion" = df$Poblacion_comuna[1:72])

exogenas1 = data.frame("IMACEC" = df$IMACEC_forecast[1:72],
                      "Poblacion" = df$Poblacion_comuna[1:72],
                      "PIB" = df$PIB_var_an_porc[1:72])
exogenas2 = data.frame("IMACEC" = df$IMACEC_forecast[73:length(df$IMACEC_forecast)],
                      "Poblacion" = df$Poblacion_comuna[73:length(df$IMACEC_forecast)],
                      "PIB" = df$PIB_var_an_porc[73:length(df$PIB_var_an_porc)])


fit <- forecast::auto.arima(Y, max.d = 6, 
                            max.D = 6, max.p = 6, max.q = 6, 
                            max.P = 6, max.Q = 6, lambda = NULL,
                            xreg = as.matrix(exogenas1[,2]))
fit <- forecast::Arima(Y, 
                       order = c(1,0,5), 
                       seasonal = c(2,1,0),
                       include.drift = FALSE,
                       fixed = c(NA,NA,0,0,0,NA,NA,NA,NA),
                       xreg = as.matrix(exogenas1[,3]))


summary(fit)
TS.diag(fit$res)




par(mfrow = c(1,1))
plot(fit)


colnames(df)

# del 1 al 48:


fit <- forecast::auto.arima(Y, d = 1, D = 1, max.p = 6, max.q = 5, max.P = 6, 
                            max.Q = 6)
summary(fit)

fit <- forecast::Arima(Y, 
                       order = c(1,0,0), 
                       seasonal = c(2,1,1),
                       include.drift = TRUE,
                       xreg = as.matrix(exogenas1[,2]))
autoplot(fit, xlim = c(-5,5), ylim = c(-5,5))

summary(fit)


fit
TS.diag(fit$res)


tsdiag(fit)
LSTS::ts.diag(fit$res)
source("TS.diag.R")
TS.diag(fit$res)


par(mfrow = c(2,1))
acf(fit$residuals)
pacf(fit$residuals)


forecast::ndiffs(fit$residuals)
forecast::nsdiffs(fit$residuals)
fit2 = forecast::auto.arima(fit$residuals)
fit2

shapiro.test(fit2$residuals) # Los residuos no son normales, osea, rechazamos la hipotesis de normalidad



ks.test(fit2$residuals, pnorm) # Los residuos no son normales, osea, rechazamos la hipotesis de normalidad

library(nortest)
lillie.test(fit2$residuals)


# predicción --------------------------------------------------------------
# Predicción

pre <- forecast::forecast(fit, level = 0.95, xreg = as.matrix(exogenas2[,2]))
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre)

lines(Y, col = "red")



# -------------------------------------------------------------------------
# MODELO 1: Con variables exogenas = Densidad poblacional estimada por comuna INE

df = rio::import("data.xlsx")

Y <- ts(df$Dx_6_L_R[1:72], start = c(2016,1), frequency = 12, end = c(2021,12))

exogenas1 = data.frame("IMACEC" = df$IMACEC_forecast[1:72],
                       "Poblacion" = df$Poblacion_comuna[1:72],
                       "PIB" = df$PIB_var_an_porc[1:72])
exogenas2 = data.frame("IMACEC" = df$IMACEC_forecast[73:length(df$IMACEC_forecast)],
                       "Poblacion" = df$Poblacion_comuna[73:length(df$IMACEC_forecast)],
                       "PIB" = df$PIB_var_an_porc[73:length(df$PIB_var_an_porc)])

fit <- forecast::auto.arima(Y, max.d = 6, 
                            max.D = 6, max.p = 6, max.q = 6, 
                            max.P = 6, max.Q = 6, lambda = NULL,
                            xreg = as.matrix(exogenas1[,2]))

TS.diag(fit$res)

fit <- forecast::Arima(Y, 
                        order = c(1,0,5), 
                        seasonal = c(2,1,0),
                        include.drift = FALSE,
                        fixed = c(NA,NA,0,0,0,NA,NA,NA,NA),
                       xreg = as.matrix(exogenas1[,2]))

TS.diag(fit$res)

library(nortest)
lillie.test(fit$residuals)

pre <- forecast::forecast(fit, level = 0.95, xreg = as.matrix(exogenas2[,2]))
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre)

lines(Y, col = "red")


# -------------------------------------------------------------------------
# Modelo 2: Sin variables exogenas:

fit2 <- forecast::auto.arima(Y, max.d = 6, 
                            max.D = 6, max.p = 6, max.q = 6, 
                            max.P = 6, max.Q = 6, lambda = NULL)

TS.diag(fit2$res)

fit2 <- forecast::Arima(Y, 
                       order = c(1,0,5), 
                       seasonal = c(2,1,0),
                       include.drift = TRUE,
                       fixed = c(NA,NA,0,0,0,NA,NA,NA,NA))
TS.diag(fit2$res)
library(nortest)
lillie.test(fit2$residuals)

pre <- forecast::forecast(fit2, h = 108, level = 0.95)
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre, xlim = c(2016,2030), ylim = c(190000, 310000), main = "", n = 1, bty = "las")


auxiliar2 = pre$mean

lines(Y, col = "red")

# -------------------------------------------------------------------------

## CROSS VALIDATION MOD 1

Y <- ts(df$Dx_6_L_R[1:60], start = c(2016,1), frequency = 12, end = c(2020,12))

exogenas1 = data.frame("IMACEC" = df$IMACEC_forecast[1:60],
                       "Poblacion" = df$Poblacion_comuna[1:60],
                       "PIB" = df$PIB_var_an_porc[1:60])
exogenas2 = data.frame("IMACEC" = df$IMACEC_forecast[61:length(df$IMACEC_forecast)],
                       "Poblacion" = df$Poblacion_comuna[61:length(df$IMACEC_forecast)],
                       "PIB" = df$PIB_var_an_porc[61:length(df$PIB_var_an_porc)])
TS.diag(fit$res)

fit <- forecast::Arima(Y, 
                       order = c(1,0,0), 
                       seasonal = c(2,1,0),
                       include.drift = FALSE,
                       xreg = as.matrix(exogenas1[,2]))

TS.diag(fit$res)

library(nortest)
lillie.test(fit$residuals)

pre <- forecast::forecast(fit, level = 0.95, xreg = as.matrix(exogenas2[,2]))
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre, xlim = c(2016,2030), ylim = c(190000, 310000))
lines(as.vector(time(Y))[1:60], as.vector(fit$fitted), col = "blue")

pre$fitted[1:12]
Y[61:72]

val_cruzada = sum(abs(pre$fitted[1:12]-Y[61:72]))/12

Y <- ts(df$Dx_6_L_R[1:72], start = c(2016,1), frequency = 12, end = c(2021,12))
lines(Y, col = "red")

#############
# modelo 2 ##
#############


Y <- ts(df$Dx_6_L_R[1:60], start = c(2016,1), frequency = 12, end = c(2020,12))

fit2 <- forecast::Arima(Y, 
                        order = c(1,0,5), 
                        seasonal = c(2,1,0),
                        include.drift = TRUE,
                        fixed = c(NA,NA,0,0,0,NA,NA,NA,NA))
TS.diag(fit2$res)
library(nortest)
lillie.test(fit2$residuals)

pre2 <- forecast::forecast(fit2, h = 100, level = 0.95)
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre, xlim = c(2016,2030), ylim = c(190000, 310000))
lines(as.vector(time(Y))[1:60], as.vector(fit2$fitted), col = "blue")

Y <- ts(df$Dx_6_L_R[1:72], start = c(2016,1), frequency = 12, end = c(2021,12))
lines(Y, col = "red")

val_cruzada2 = sum(abs(pre2$fitted[1:12]-Y[61:72]))/12






