library(tidyverse)
library(forecast)
library(LSTS)
library(splines)



# Variables ---------------------------------------------------------------

df = rio::import("data.xlsx")

lr <- df$Dx_6_L_R
r_500 <- df$Dx_6_R500

Y <- ts(df$Dx_6_R500, start = c(2016,1), frequency = 12, end = c(2021,12))

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

periodogram(lr.ts)

aux = periodogram(diff(lr.ts))

plot(diff(lr.ts))

periodogram(diff(lr.ts))

par(mfrow = c(1,1))
plot(aux$periodogram, type = "l")
abline(v = 30)
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

exogen1 = data.frame("PIB" = df$PIB_var_an_porc[1:72],
                    "IPC" = df$IPC_var_anual_porc_prom[1:72],
                    "poblacion" = df$Poblacion_chile[1:72])
exogen2 = data.frame("PIB" = df$PIB_var_an_porc[73:length(df$PIB_var_an_porc)],
                     "IPC" = df$IPC_var_anual_porc_prom[73:length(df$PIB_var_an_porc)],
                     "poblacion" = df$Poblacion_chile[73:length(df$PIB_var_an_porc)])
plot(fit)
fit <- forecast::auto.arima(Y, max.d = 6, max.D = 6, max.p = 6, max.q = 6, 
                            max.P = 6, max.Q = 6, lambda = NULL)

fit <- forecast::auto.arima(Y, max.d = 6, max.D = 6, max.p = 6, max.q = 6, 
                            max.P = 6, max.Q = 6, lambda = NULL,
                            xreg = as.matrix(exogen))

summary(fit)

colnames(df)

# del 1 al 48:


fit <- forecast::auto.arima(Y, d = 1, D = 1, max.p = 6, max.q = 5, max.P = 6, 
                            max.Q = 6)
summary(fit)

fit <- forecast::Arima(Y, 
                       order = c(1,0,0), 
                       seasonal = c(2,1,0),
                       include.drift = TRUE)
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

