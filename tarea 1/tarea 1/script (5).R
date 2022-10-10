#install.packages("rio")
library(rio)
library(tidyverse)
library(splines)
library(MASS)
library(lmtest)
library(forecast)
library(LSTS)


MAPE = function(Y, Y_pred){
  x = 0
  for (i in 1:length(Y)){
    x = x + abs((Y[i]-Y_pred[i])/(Y[i]))
  }
  resp = x/length(Y)
  return(resp)}

datos = rio::import("data.xlsx")
head(datos)

datos$drift = c(1:length(datos$FECHA))
acf(datos$Venta[c(length(datos$Venta)-8:length(datos$Venta))], lag.max = 20, ylim = c(-1,+1))
acf((datos$Venta[c(length(datos$Venta)-8:length(datos$Venta))])^2, lag.max = 20, ylim = c(-1,+1))


# -------------------------------------------------------------------------

pred_IPC = lm(Venta~ IPC_mensual + drift, data = datos)
datos$fitted1 = c(pred_IPC$fitted.values, rep(NA,8))

par(bty = "n", las = 1)
plot(datos$Venta ~ datos$drift, type = "l")
lines(fitted1~drift, data = datos, col = 2)

# Con el IPC queda pesimo, probaremos con otra variable macroeconomica

colnames(datos)


# -------------------------------------------------------------------------

pred_dolar = lm(Venta~ Apertura_dolar + drift, data = datos)
datos$fitted2 = c(pred_dolar$fitted.values, rep(NA,8))

par(bty = "n", las = 1)
#plot(datos$Venta ~ datos$drift, type = "l")
lines(fitted2~drift, data = datos, col = 3)

# la apertura del dolar se ve algo mejor que el IPC,
# aunque tampoco creemos que sea la variable macroecon?mica correcta
# probaremos con la variaci?n
# del precio del dolar, a ver si deja mejor el modelo

colnames(datos)

# -------------------------------------------------------------------------

pred_dolar2 = lm(Venta~ PORC_VAR_DOLAR + drift, data = datos)
datos$fitted3 = c(pred_dolar2$fitted.values, rep(NA,8))

par(bty = "n", las = 1)
#plot(datos$Venta ~ datos$drift, type = "l")
lines(fitted3~drift, data = datos, col = 4)

# peor que la apertura del dolar, ni si quiera
# modela alguna tendencia, probaremos con otra variable

colnames(datos)

# -------------------------------------------------------------------------

pred_imacec = lm(Venta~ IMACEC + drift, data = datos)
datos$fitted4 = c(pred_imacec$fitted.values, rep(NA,8))

par(bty = "n", las = 1)
plot(datos$Venta ~ datos$drift, type = "l")
lines(fitted4~drift, data = datos, col = 2)

# Esta variable al menos marca la caida del final, podr?amos seguir
# trabajando con esta, agregando otras variables que modelen el resto.

colnames(datos)


# -------------------------------------------------------------------------



pred_2 = lm(Venta~IMACEC+MOBILITY + drift, data = datos)
datos$fitted5 = c(pred_2$fitted.values, rep(NA,8))
#plot(datos$Venta ~ datos$drift, type = "l")
lines(fitted5~drift, data = datos, col = 3)
#view(datos)

# sumado a lo que hace el imacec solo, agregar la movilidad
# ayuda a modelar un poco la tendencia



# -------------------------------------------------------------------------

pred_3 = lm(Venta~IMACEC+MOBILITY + as.factor(MES) + drift, data = datos)
datos$fitted6 = c(pred_3$fitted.values, rep(NA,8))
plot(datos$Venta ~ datos$FECHA, type = "l")
lines(fitted6~FECHA, data = datos, col = 4)

# agregando los meses como factores, se modela bastante mejor las variaciones
# y ya nos acercamos m?s a lo deseado, probaremos si agregando algunas variables
# logramos mejorar m?s esto, probaremos primero con el estallido

# -------------------------------------------------------------------------

pred_5 = lm(Venta~IMACEC+MOBILITY + as.factor(MES) + as.factor(ESTALLIDO) 
            + drift, data = datos)
datos$fitted5 = c(pred_5$fitted.values, rep(NA,8))
#plot(datos$Venta ~ datos$FECHA, type = "l")
lines(fitted5~FECHA, data = datos, col = 8)

# vemos como que algo mejora el final del 2019, probemos si haciendo el
# drift m?s smooth, se modela mejor

# -------------------------------------------------------------------------

pred_6 = lm(Venta~IMACEC+MOBILITY + as.factor(MES) + as.factor(ESTALLIDO) +
              bs(drift), data = datos)
datos$fitted6 = c(pred_6$fitted.values, rep(NA,8))
#plot(datos$Venta ~ datos$FECHA, type = "l")
lines(fitted6~FECHA, data = datos, col = 3)

# pareciera que modela mejor en general, pero veamos medidas de error, para 
# ver con cual drift nos quedamos.

summary(pred_5)
summary(pred_6)

AIC(pred_5)
AIC(pred_6)

# vemos que en el modelo con el drift m?s suave tenemos un mayor R cuadrado
# y un menor AIC, esto nos indica mejor ajuste, pero veamos la medida de
# error que se nos indic? en el enunciado: el MAPE.

MAPE(datos$Venta[1:96],datos$fitted5[1:96])
MAPE(datos$Venta[1:96],datos$fitted6[1:96])

# El MAPE es menor en el modelo con el smooth drift, por ?ltimo, veremos si agregar
# los retiros de las afp sirve para mejorar el modelo, aunque ya se ve bastante
# bien.


# -------------------------------------------------------------------------

pred_8 = lm(Venta~ IMACEC+ MOBILITY + as.factor(MES) + as.factor(ESTALLIDO) + 
              bs(drift) + as.factor(RETIROS), data = datos)
datos$fitted8 = c(pred_8$fitted.values, rep(NA,8))
#plot(datos$Venta ~ datos$FECHA, type = "l")
lines(fitted8~FECHA, data = datos, col = 2)

# se ve muy similar, a simple vista, creo que es mejor, pero veremos nuevamente
# medidas de bondad de ajuste.

summary(pred_6)
summary(pred_8)

AIC(pred_6)
AIC(pred_8)

MAPE(datos$Venta[1:96],datos$fitted6[1:96])
MAPE(datos$Venta[1:96],datos$fitted8[1:96])

## muy similares las medidas, aunque uno podr?a tender a inclinarse por el
# modelo sin los retiros. Probar? tratando los retiros de manera separada
# siendo una dummy distinta cada retiro.

# -------------------------------------------------------------------------


pred_9 = lm(Venta~ IMACEC+ MOBILITY + as.factor(MES) + as.factor(ESTALLIDO) + 
              bs(drift) + 
              as.factor(Retiro1) + as.factor(Retiro2) + as.factor(Retiro3), data = datos)
datos$fitted9 = c(pred_9$fitted.values, rep(NA,8))
#plot(datos$Venta ~ datos$FECHA, type = "l")
lines(fitted9~FECHA, data = datos, col = 4)

summary(pred_8)

# Nuevamente, pareciera ajustar mejor, pero probemos medidas de bondad de ajuste:

summary(pred_6)
summary(pred_9)

AIC(pred_6)
AIC(pred_9)

MAPE(datos$Venta[1:96],datos$fitted6[1:96])
MAPE(datos$Venta[1:96],datos$fitted9[1:96])

# Este modelo parece ajustar mejor. De todas maneras,
# probaremos otra cosa. Tomar por separado el mes del retiro, el segundo
# mes del retiro y el mes post retiro, pues, la gente probablemente
# tiene menos dinero posterior al retiro, gracias a gastarlo en los
# meses del retiro en s?.

# -------------------------------------------------------------------------


pred_10 = lm(Venta~ IMACEC+ MOBILITY + as.factor(MES) + as.factor(ESTALLIDO) + 
               bs(drift) +
              as.factor(RetiroA) + as.factor(RetiroB) +
               as.factor(Post_retiro), data = datos)
datos$fitted10 = c(pred_10$fitted.values, rep(NA,8))
plot(datos$Venta ~ datos$FECHA, type = "l")
lines(fitted10~FECHA, data = datos, col = 3)

summary(pred_9)
summary(pred_10)

AIC(pred_9)
AIC(pred_10)

MAPE(datos$Venta[1:96],datos$fitted9[1:96])
MAPE(datos$Venta[1:96],datos$fitted10[1:96])

# ambos modelos tienen medidas de bondad de ajuste muy similares

# Por ultimo, probaremos agregando la temperatura y luego veremos si alguna variable
# no es significativa.

# -------------------------------------------------------------------------


pred_11 = lm(Venta~ IMACEC+ MOBILITY + as.factor(MES) + as.factor(ESTALLIDO) + 
               bs(drift) + as.factor(Retiro1) + as.factor(Retiro2) +
               as.factor(Retiro3) + temperatura, data = datos)
datos$fitted11 = c(pred_11$fitted.values, rep(NA,8))
#plot(datos$Venta ~ datos$FECHA, type = "l")
lines(fitted11~FECHA, data = datos, col = 2)


pred_12 = lm(Venta~ IMACEC+ MOBILITY + as.factor(MES) + as.factor(ESTALLIDO) + 
               bs(drift) +
               as.factor(RetiroA) + as.factor(RetiroB) +
               as.factor(Post_retiro) + temperatura, data = datos)
datos$fitted12 = c(pred_12$fitted.values, rep(NA,8))
#plot(datos$Venta ~ datos$FECHA, type = "l")
lines(fitted12~FECHA, data = datos, col = 4)



summary(pred_9)
summary(pred_10)
summary(pred_11)
summary(pred_12)

AIC(pred_9)
AIC(pred_10)
AIC(pred_11)
AIC(pred_12)

MAPE(datos$Venta[1:96],datos$fitted9[1:96])
MAPE(datos$Venta[1:96],datos$fitted10[1:96])
MAPE(datos$Venta[1:96],datos$fitted11[1:96])
MAPE(datos$Venta[1:96],datos$fitted12[1:96])

# Pareciera que los modelos 10 y 11 ajustan mejor,el 11 en base al MAPE y AIC
# el 10 en base a r^2 ajustado, ver? ahora si alguna variable
# no es significativa y el tema de si una transformaci?n de boxcox nos ayuda

summary(pred_10)
summary(pred_11)


# parece ser que la temperatura, el IMACEC y la parte cubica del spline no son
# significativos, ir? eliminando de a 1 en ambos modelos y viendo que pasa

pred_10_a = lm(Venta~ MOBILITY + as.factor(MES) + as.factor(ESTALLIDO) + 
               bs(drift) +
               as.factor(RetiroA) + as.factor(RetiroB) +
               as.factor(Post_retiro), data = datos)
datos$fitted10_a = c(pred_10_a$fitted.values, rep(NA,8))
plot(datos$Venta ~ datos$FECHA, type = "l")
lines(fitted10_a~FECHA, data = datos, col = 3)

pred_11_a = lm(Venta~ MOBILITY + as.factor(MES) + as.factor(ESTALLIDO) + 
               bs(drift) + as.factor(Retiro1) + as.factor(Retiro2) +
               as.factor(Retiro3) + temperatura, data = datos)
datos$fitted11_a = c(pred_11_a$fitted.values, rep(NA,8))
#plot(datos$Venta ~ datos$FECHA, type = "l")
lines(fitted11_a~FECHA, data = datos, col = 2)

summary(pred_10_a)
summary(pred_11_a)

AIC(pred_10_a)
AIC(pred_11_a)

MAPE(datos$Venta[1:96],datos$fitted10_a[1:96])
MAPE(datos$Venta[1:96],datos$fitted11_a[1:96])

#Todas las medidas de error siguen bastante bien. Eliminaremos el grado 3
#del bs ahora

pred_10_b = lm(Venta~ MOBILITY + as.factor(MES) + as.factor(ESTALLIDO) + 
                 bs(drift, degree = 2) +
                 as.factor(RetiroA) + as.factor(RetiroB) +
                 as.factor(Post_retiro), data = datos)
datos$fitted10_b = c(pred_10_b$fitted.values, rep(NA,8))
plot(datos$Venta ~ datos$FECHA, type = "l")
lines(fitted10_b~FECHA, data = datos, col = 3)

pred_11_b = lm(Venta~ MOBILITY + as.factor(MES) + as.factor(ESTALLIDO) + 
                 bs(drift, degree = 2) + as.factor(Retiro1) + as.factor(Retiro2) +
                 as.factor(Retiro3) + temperatura, data = datos)
datos$fitted11_b = c(pred_11_b$fitted.values, rep(NA,8))
#plot(datos$Venta ~ datos$FECHA, type = "l")
lines(fitted11_b~FECHA, data = datos, col = 2)

summary(pred_10_b)
summary(pred_11_b)

AIC(pred_10_b)
AIC(pred_11_b)

MAPE(datos$Venta[1:96],datos$fitted10_b[1:96])
MAPE(datos$Venta[1:96],datos$fitted11_b[1:96])

# empeoran algo las medidas de bondad de ajuste, pero probemos seguir eliminando
# por parsimonia. Ahora eliminar? a cada modelo las dummy de retiros que parecen,
# menos relevantes.

pred_10_c = lm(Venta~ MOBILITY + as.factor(MES) + as.factor(ESTALLIDO) + 
                 bs(drift, degree = 2) + as.factor(RetiroB), data = datos)
datos$fitted10_c = c(pred_10_c$fitted.values, rep(NA,8))
plot(datos$Venta ~ datos$FECHA, type = "l")
lines(fitted10_c~FECHA, data = datos, col = 3)

pred_11_c = lm(Venta~ MOBILITY + as.factor(MES) + as.factor(ESTALLIDO) + 
                 bs(drift, degree = 2) + as.factor(Retiro2) + temperatura, data = datos)
datos$fitted11_c = c(pred_11_c$fitted.values, rep(NA,8))
#plot(datos$Venta ~ datos$FECHA, type = "l")
lines(fitted11_c~FECHA, data = datos, col = 2)

summary(pred_10_c)
summary(pred_11_c)

AIC(pred_10_c)
AIC(pred_11_c)

MAPE(datos$Venta[1:96],datos$fitted10_c[1:96])
MAPE(datos$Venta[1:96],datos$fitted11_c[1:96])

#Siguen bastante bien los ajustes, por ultimo eliminar? la temperatura del segundo
#modelo y ver? qu? pasa

pred_11_d = lm(Venta~ MOBILITY + as.factor(MES) + as.factor(ESTALLIDO) + 
                 bs(drift, degree = 2) + as.factor(Retiro2), data = datos)
datos$fitted11_d = c(pred_11_d$fitted.values, rep(NA,8))
#plot(datos$Venta ~ datos$FECHA, type = "l")
lines(fitted11_d~FECHA, data = datos, col = 4)

summary(pred_10_c)
summary(pred_11_d)

AIC(pred_10_c) #segundo modelo
AIC(pred_11_d) #segundo modelo

MAPE(datos$Venta[1:96],datos$fitted10_c[1:96])
MAPE(datos$Venta[1:96],datos$fitted11_d[1:96])

aux <- boxcox(pred_10_c) #verosimilitud de los Lambda
aux$x[aux$y == max(aux$y)] #el con mayor ver
#data$Y <- (dataa$venta^lambda-1)/lambda #transformaci?n boxcox con ese lambda

#El intervalo confianza incluye al 1, de hecho, el con mayor verosimilitud es
#0.7878 que es muy cercano a 1, probablemente no necesitemos ninguna transformaci?n

aux <- boxcox(pred_11_d) #verosimilitud de los Lambda
aux$x[aux$y == max(aux$y)] #el con mayor ver
#data$Y <- (dataa$venta^lambda-1)/lambda #transformaci?n boxcox con ese lambda

#Este tambi?n incluye el 1 en el intervalo de confianza y el lambda con
#mayor verosimilitud es 1.03 por lo que tampoco le aplicar?a una 
#transformaci?n de boxcox

#Probar? predecir con los modelos as?, luego jugar? con los residuos.

new_data = datos[97:104,]
predi_1 = c(rep(NA,95), datos$Venta[96] ,predict(pred_10_c, newdata = new_data))
predi_2 = c(rep(NA,95), datos$Venta[96] ,predict(pred_11_d, newdata = new_data))
par(mfrow = c(2,1),bty = "n", las = 1, main = "IMG1")
plot(datos$Venta ~ datos$drift, type = "l", xlab = "Tiempo", ylab = "", main = "Modelo 2")
lines(predi_1, col = "4")
plot(datos$Venta ~ datos$drift, type = "l", xlab = "Tiempo", ylab = "", main = "Modelo 1")
lines(predi_2, col = "2")

#ambas predicciones se ven gr?ficamente "razonables", pero sigamos trabajando.

#Ahora veremos los residuos de ambos modelos y les haremos test de blancura.

par(mfrow = c(2,1),bty = "n", las = 1)
plot(pred_10_c$residuals, type = "l")
plot(pred_11_d$residuals, type = "l")

 #A simple vista, los residuos de ninguno de los modelos parecen ruido blanco.
#pero veamos su acf
acf(pred_10_c$residuals)
pacf(pred_10_c$residuals)
acf(pred_11_d$residuals)
pacf(pred_11_d$residuals)
'
Modelo MA2
'
#Solo viendo estos gr?ficos, dir?a que es muy dif?cil que los residuos sean
#ruido blanco, pero veamos con Box-Ljung:

source("TS.diag.R")
TS.diag(pred_10_c$residuals)
TS.diag(pred_11_d$residuals)

#No son ruido blanco, as? que ver? si puedo ajustar algun modelo ARMA
#por los gr?ficos de ACF y PACF aposta?a por un modelo MA(1) o MA(2)
#pero veamos qu? sugiere el comando auto.arima():

arima1 = auto.arima(pred_10_c$residuals)
arima2 = auto.arima(pred_11_d$residuals)

#para los residuos de ambos modelos nos sugiere un AR1, veamos que tal estos modelos.
par(mfrow = c(2,1),bty = "n", las = 1)
plot(pred_10_c$residuals, type = "l")
lines(arima1$fitted, col = 2)
plot(pred_11_d$residuals, type = "l")
lines(arima2$fitted, col = 2)

#gr?ficamente, se ve bien, pero veamos medidas de bondad de ajuste:

summary(arima1)
summary(arima2)

MAPE(pred_10_c$residuals,arima1$fitted)
MAPE(pred_11_d$residuals,arima2$fitted)

#tienen AIC muy similar, aunque el MAPE del segundo es mucho m?s bajo,
#me inclinar?a por seguir el camino del segundo modelo.

# Sin embargo, veamos los residuos de los residuos, para ver si son
# ruido blanco.

plot(arima1$residuals)
plot(arima2$residuals)

#estos residuos parecen m?s ruido blanco, posiblemente el modelo AR(1)
#propuesto es apropiado. Aunque, a simple vista, los residuos del segundo
#AR(1) parecen m?s ruido blanco, de todas formas, grafiquemos ACF y PACF:

acf(arima1$residuals)
pacf(arima1$residuals)
acf(arima2$residuals)
pacf(arima2$residuals)

#en los gr?ficos de ambos ACF como que se pasa un poco cerca del lag 10, pero
#veamos test de Box-Ljung

TS.diag(arima1$residuals)
TS.diag(arima2$residuals)

#ambos se caen un poco cerca al lag 18, Pero me parece suficientemente bien.
#de todas maneras probar? auto.arima() diferenciando:

arima3 = auto.arima(pred_10_c$residuals, d = 1)
  arima4 = auto.arima(pred_11_d$residuals, d = 1)

TS.diag(arima3$residuals)
TS.diag(arima4$residuals)

#vemos que el nuevo modelo arima 4 que consiste en un modelo MA(3) con una diferenciaci?n
#(o sea, ARIMA(0,1,3)), nos da unos residuos que son a?n m?s similares a un ruido
#blanco, as? que nos quedamos con este.
#Juntando todo, nos quedamos con el modelo original pred_11_d y con el modelo
#arima4 para sus residuos. Esto ser?a quedarnos como modelo original como
#un lm con variables mobilidad, el mes como factor, el estallido como factor
#el drift suavizado a grado 2 y el segundo retiro de las afp como una dummy.
#Adem?s, como los residuos de este modelo no quedaron como ruido blanco, 
#los modelamos con un modelo ARIMA(0,1,3).
#Ahora falta predecir con ambos modelos, como nuestro modelo usa variables que
#a?n no sabemos su valor hasta diciembre (mobility), haremos la predicci?n hasta
#donde tenemos datos a la fecha de hoy. Estamos en septiembre por lo que
#no tenemos los datos completos de septiembre, haremos la predicci?n hasta Agosto.
a = forecast(arima4,8)
new_data = datos[97:104,]
predi_final = predict(pred_11_d, newdata = new_data)+a$mean[1:8]
predi_final_plot = c(rep(NA,95), datos$Venta[96] ,predi_final)
par(mfrow = c(1,1),bty = "n", las = 1)
plot(datos$Venta ~ datos$drift, type = "l")
lines(predi_final_plot, col = "4")

#Luce razonable.
# En resumen, el modelo es MOBILITY + as.factor(MES) + as.factor(ESTALLIDO) 
# + bs(drift, degree = 2) + as.factor(Retiro2):

summary(pred_11_d)

#todas sus variables son significativas a un nivel del 0.05, excepto el mes de
#diciembre como factor.
#Su r^2 ajustado es de 0.5446
AIC(pred_11_d)
#su AIC es de 2012.908 muy similar a los otros modelos, los cuales ten?an un 
#AIC  de 1900 o m?s.
MAPE(datos$Venta[1:96],datos$fitted11_d[1:96])
#y un MAPE de 11.8% que no est? mal. La predicci?n de las ventas
#para enero-agosto 2022 son las siguientes:
predi_final
#Y gr?ficamente se ve as?:
par(mfrow = c(1,1),bty = "n", las = 1)
plot(datos$Venta ~ datos$drift, type = "l")
lines(predi_final_plot, col = "4")
