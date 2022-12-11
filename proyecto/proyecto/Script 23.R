#####################
## Script Clase 23 ##
#####################

## Modelo SARIMA(p,d,q)(P,D,Q)[s]

Data <- rio::import("IMACEC_NM.xlsx")
head(Data)
Y <- ts(Data$IMACEC_NM, start = c(2013,1), frequency = 12, end = c(2021,12))

plot(Y, col = "gray", lwd = 3)

Y[time(Y) >= 2020 & time(Y) < 2021 + (4-1)/12] = NA

par(mfrow = c(1,1), bty = "n", las = 1)
plot(Y, col = "gray", lwd = 3)
par(mfrow = c(1,2), bty = "n", las = 1)
acf(Y, na.action = na.pass, lag.max = 48, ylim = c(-1,+1), main = "")
pacf(Y, na.action = na.pass, lag.max = 100, ylim = c(-1,+1), main = "")

## ¿d? ##
forecast::ndiffs(Y)
plot(diff(Y), col = "gray", lwd = 3)
acf(diff(Y), na.action = na.pass, lag.max = 60)

## ¿D? ##
forecast::nsdiffs(diff(Y))
plot(diff(diff(Y), lag = frequency(Y)), col = "gray", lwd = 3)
acf(diff(diff(Y), lag = frequency(Y)), na.action = na.pass, lag.max = 60)


fit <- forecast::auto.arima(Y, d = 1, D = 1, max.p = 3, max.q = 2, max.P = 2, max.Q = 1, lambda = NULL)
fit

tsdiag(fit)
LSTS::ts.diag(fit$res)
source("TS.diag.R")
TS.diag(fit$res)


fit <- forecast::Arima(Y, order = c(0,1,3), seasonal = c(0,1,1), fixed = c(NA,0,NA,NA))
fit
TS.diag(fit$res)

# Aqui tenemos que el lag 3 del ljung-box esta muy bajo entonces lo ajusta con un arma(0,1,3) para subirlo


# Predicción

pre <- forecast::forecast(fit, h = 9, level = 0.95)
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre)

Y <- ts(Data$IMACEC_NM, start = c(2013,1), frequency = 12)
lines(Y, col = "red")

## Eliminemos 2020 y 2021

Y <- ts(Data$IMACEC_NM, start = c(2013,1), frequency = 12, end = c(2021,12))
Y[trunc(time(Y))>= 2020] = NA

plot(Y, col = "gray", lwd = 3)

fit <- forecast::auto.arima(Y, d = 1, D = 1, max.p = 3, max.q = 2, max.P = 2, max.Q = 1, lambda = NULL)
fit
TS.diag(fit$res)

fit <- forecast::Arima(Y, order = c(0,1,10), 
                       seasonal = c(0,1,1), fixed = c(NA,NA,0,0,0,0,0,0,0,NA, NA), lambda = NULL)

pre <- forecast::forecast(fit, h = 60, level = 0.95)
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre, xlim = c(2010,2025), ylim = c(70,130))
Y <- ts(Data$IMACEC_NM, start = c(2013,1), frequency = 12)
lines(Y, col = "red")

