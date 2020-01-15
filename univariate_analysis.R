rm(list = ls()) # clean environment

# packages and functions ----------------------------------------------------------------

library(xts)
library(lubridate)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(forecast)
library(tseries)
library(MLmetrics)
library(urca)
library(Hmisc)
library(plotly)
library(TSA)


acfpacf <- function(x, max.lag=36){
  require(forecast)
  par(mfrow = c(2,1))
  Acf(x, max.lag)
  Pacf(x, max.lag)
  par(mfrow = c(1,1))
}

# import data --------------------------------------------------------------

data <- read.csv("data/output/df_final2010-2018.csv", header = T)
lunar_motion <- read.csv("moon_distance/moon_distances.csv", header = T)

head(data)
tail(data)
str(data)
describe(data)

head(lunar_motion)
tail(lunar_motion)
str(lunar_motion)
describe(lunar_motion)

anyNA(data)
anyNA(lunar_motion)

# inspections -------------------------------------------------------------

data_ts <- xts(order.by = as.POSIXct(data$datetime),
               x = data$level,
               frequency = 24,
               tzone = "Europe/Berlin")

plot(data_ts,
     main = "Venice tides") # intera time series

ggtsdisplay(data_ts["2015-12-31 23:00:00/2018-12-31 23:00:00"],
            main = "Venice tides 2016 2018",
            lag.max = 72,
            theme = theme_bw()) # 2016-2018

rm(data_ts)

# check for stationarity in variance ----------------------------------------

# convertiamo in df solo per verificare stazionarietà in varianza

data %>%
  add_column(date = date(data$datetime)) %>%
  group_by(date) %>% 
  mutate(mean_day = mean(level), sd_day = sd(level)) %>%
  select(date, mean_day, sd_day) %>% 
  distinct(date, mean_day, sd_day) %>%
  ggplot(aes(mean_day, sd_day)) + 
  geom_point() + 
  ggtitle("Mean per day vs Std per day") +
  labs(x = "Day mean",
       y = "Day std") +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("label", label = "Seems to be stationary", x = 90, y = 26, size = 5) +
  theme_bw() -> pl

ggplotly(pl)

# sembrerebbe esserci un certo trend crescente ma dovrebbe essere trascurabile, assumiamo che sia stazionaria
# in varianza


# verifichiamo la stazionarietà in media

data %>% 
  ggplot(aes(y = level, x = seq(1:nrow(data)))) +
  geom_line() +
  geom_hline(yintercept = mean(data$level),
             linetype = "dashed",
             color = "darkred",
             size = 1)  +
  annotate("label",
           label = paste0("Mean = ",
                          round(mean(data$level), 2)),
           y = 150,
           x = 3500,#as.POSIXct("2010-01-01 00:00:00"),
           size = 5,
           colour = "black") +
  labs(title = "Tides data for 2010-2018 years with mean (in red)",
       x = "Datetime",
       y = "Level") +
  theme_bw() ->pl2

ggplotly(pl2)

# a prima vista sembrerebbe essere stazionaria ma possiamo testare questa impressione


test_stat <- ur.df(data$level,
                   type = "drift",
                   lags = 24,
                   selectlags = "AIC")
summary(test_stat)

rm(list = c("test_stat", "pl", "pl2"))

# anche il test conferma essere stazionaria

data %>%
  ggplot(aes(x = level, y = ..density..)) +
  geom_histogram(colour = "black",
                 fill = "lightblue",
                 bins = 50) + 
  geom_density(fill = "red",
               alpha = .2) +
  geom_vline(xintercept = mean(data$level),
             linetype = "dashed",
             color = "darkred",
             size = 1) +
  labs(x = "Level",
       y = "Density",
       title = "Density plot with histogram for tides level distribution") +
  annotate("label",
           label = "Seems to be normally distributed",
           x = 120,
           y = 0.012,
           size = 5) +
  theme_bw() -> pl

ggplotly(pl)

# il fenomeno sembrerebbe distribuirsi normalmente

# define train and test set ed integrare ciclo lunare -----------------------------------------------
data$l_motion <- lunar_motion$dists.Km.
rm(lunar_motion)

data[data$datetime=="2018-07-01 00:00:00",]

trn <- data[74473:(nrow(data)-168),] # train sono 3 anni meno 1 settimana
head(trn)
tail(trn)
nrow(trn)
tst <- data[(nrow(data)-167):nrow(data),] # test 1 settimana
head(tst, 1)
tail(tst, 1)
nrow(tst)

ggtsdisplay(trn$level,
            main = "Venice tides 01/2018 12/2018",
            lag.max = 72,
            theme = theme_bw())

ggtsdisplay(tst$level,
            main = "Venice tides last week 2018",
            lag.max = 72,
            theme = theme_bw())

# a giudicare dai grafici ACF e PACF sembrerebbe trattarsi di un modello MA stagionale
# poichè PACF decresce geometricamente mentre ACF presenta dei picchi stagionali

p <- periodogram(data$level)
dd <- data.frame(freq=p$freq, spec=p$spec, time=1/p$freq)
order <- dd[order(-dd$spec),]
top10 <- head(order, 10)
top10

rm(list = c("p", "dd", "order", "top10"))

# è palese da ciò come siano presenti due stagionalità, una su 12 lag e una 24

# linear models -----------------------------------------------------------
# è necessario verificare l'impatto delle variabili esterne sulla predizione del livello delle maree
# prima di procedere a realizzare un modello ARMA o ARIMA

# solo variabili meteo additive con ciclo lunare normalizzato

rain_norm <- as.vector(scale(trn$rain))
vel_wind_norm <- as.vector(scale(trn$vel_wind))
dir_wind_norm <- as.vector(scale(trn$dir_wind))
l_motion_norm <- as.vector(scale(trn$l_motion))
trn_norm <- data.frame(level = trn$level, rain = rain_norm, vel_wind = vel_wind_norm,
                       dir_wind = dir_wind_norm,
                       l_motion = l_motion_norm)

rm(list = c("trn", "rain_norm", "vel_wind_norm", "dir_wind_norm", "l_motion_norm"))

mod1 <- lm(formula = level ~ rain + 
             vel_wind +
             dir_wind + 
             l_motion,
           data = trn_norm)
summary(mod1) # R2 4.8%

plot(trn_norm$level, type = "l")
lines(mod1$fitted.values, col = "red")
abline(h = mean(trn_norm$level), col = "blue", lty = 2, lwd = 2)

acfpacf(mod1$residuals)

rain_norm_test <- as.vector(scale(tst$rain))
vel_wind_norm_test <- as.vector(scale(tst$vel_wind))
dir_wind_norm_test <- as.vector(scale(tst$dir_wind))
l_motion_norm_test <- as.vector(scale(tst$l_motion))
tst_norm <- data.frame(level = tst$level, rain = rain_norm_test, vel_wind = vel_wind_norm_test,
                       dir_wind = dir_wind_norm_test,
                   l_motion = l_motion_norm_test)

rm(list = c("tst", "rain_norm_test", "vel_wind_norm_test", "dir_wind_norm_test", "l_motion_norm_test"))
head(tst_norm)
tail(tst_norm)
nrow(tst_norm)

pred <- predict.lm(mod1, newdata = tst_norm, interval = 'prediction')
nrow(pred)
head(pred)

plot(tst_norm$level, type = "l")
lines(pred[,1], col = "red")
abline(h = mean(tst_norm$level), col = "blue", lty = 2, lwd = 2)

# variabili meteo e ciclo lunare con interazioni fittato con metodo stepwise basato su AIC

mod2 <- lm(level ~ ., data = trn_norm)
final_model <- step(mod2, scope = . ~ .^2, direction = 'both')
summary(final_model) #R2 5.8%

pred <- predict.lm(final_model, newdata = tst_norm, interval = 'prediction')
nrow(pred)
head(pred)


plot(tst_norm$level, type = "l")
lines(pred[,1], col = "red")
abline(h = mean(tst_norm$level), col = "blue", lty = 2, lwd = 2)

rm(list = c("data", "final_model", "mod1", "mod2", "pred"))
## al netto di queste prove possiamo vedere come le covariate non sembrino spiegare molto.. proviamo con modelli ARIMA


# ARIMA -------------------------------------------------------------------

# model 1

# i regressori se inseriti all'interno del modello ARIMA sembrerebbero non essere proprio
# significativi

acfpacf(trn_norm$level, max.lag = 100)
# decom <- stl(ts(trn_norm$level, frequency = 24), s.window = 'periodic')
# p <- seasadj(decom)
# ggplotly(autoplot(cbind(trn_norm$level, seasonallyAdjusted = p)))
##TODO applicare lm ai residui di arima
xreg <- matrix(c(trn_norm$rain, trn_norm$vel_wind,
         trn_norm$dir_wind, trn_norm$l_motion), ncol = 4)
col_names <- c("rain", "vel_wind", "dir_wind", "l_motion")
colnames(xreg) <- col_names
#(3,1,3)()[24]
mod1_ar <- Arima(trn_norm$level, xreg = xreg,
                 include.drift = T,
                 c(3,1,3)
                 , list(order = c(1,1,3), period = 24)
                )
summary(mod1_ar)
acfpacf(mod1_ar$residuals, max.lag = 100)
Box.test(mod1_ar$residuals, type="Ljung-Box")

xreg_test <- matrix(c(tst_norm$rain, tst_norm$vel_wind,
                 tst_norm$dir_wind, tst_norm$l_motion), ncol = 4)
colnames(xreg_test) <- col_names

pred <- forecast(mod1_ar, h = 168, xreg = xreg_test)
ggplotly(autoplot(pred))

plot(tst_norm$level, type = "l")
lines(as.numeric(pred$mean), col = "red")

# model auto
mod_auto <- auto.arima(ts(trn_norm$level, frequency = 24), xreg = xreg, stepwise = T,
                       max.p = 5, max.q = 5, max.d = 5,
                       max.P = 5, max.Q = 5, max.D = 5,
                       lambda = 'auto', trace = T,
                       nmodels = 100)
summary(mod_auto)
# auto arima trova modello (1,0,5)(0,1,2)[24]
#                          (2,0,3)(1,1,2)[12]
#                          (4,1,0)(2,0,2)[6]
#                          (5,1,1)()[744]
acfpacf(mod_auto$residuals, max.lag = 72)
Box.test(mod_auto$residuals, type = "Ljung-Box")
pred <- forecast(mod_auto, h = 168, xreg = xreg_test)
ggplotly(autoplot(pred))

plot(tst_norm$level, type = "l")
lines(as.numeric(pred$mean), col = "red")

# UCM ---------------------------------------------------------------------



                        #########################################################
                        #  ############# DA QUI IN POI DEPRECATO ############## #
                        #########################################################

plot(forecast(mod1, h = 168))
pred <- forecast(mod1, h = 24)
plot(pred)
plot(pred, xlim = c(1460,1464))

plot(ddf$level[2:48], type = "l")

## occorre approfondire meglio l'argomento stagionalità perchè sembrerebbe piuttosto contorto,
#infatti scombussola andamento



#plot(density(ddf$level))

# data %>%
#   add_column(date = date(data$datetime)) %>%
#   group_by(date) %>% 
#   mutate(mean_day = mean(level), sd_day = sd(level)) %>%
#   select(date, mean_day, sd_day) %>% 
#   distinct(date, mean_day, sd_day) %>%
#   ggplot(aes(mean_day, sd_day)) + 
#   geom_point() + 
#   ggtitle("Mean per day vs Std per day") +
#   labs(x = "Day mean",
#        y = "Day std") +
#   geom_smooth(method = "lm", se = FALSE)+
#   theme_bw()


# Osservando questo plot sembrerebbe che la serie sia stazionaria in varianza.
# Ciò potrebbe avere senso considerando che si tratta di un fenomeno naturale e non di un evento umano soggetto
# alla crescita economica o demografica

str(data)

print(which(data$datetime=="2017-01-01 01:00:00"))
print(which(data$datetime == "2018-12-01 00:00:00"))


data[306817,]
data[314832,]


# rownames(data) <- data$datetime
# data$datetime <- NULL
# head(data)

## 11 mesi di train e 1 di test

train <- ts(data$level[298057:314831,drop=FALSE])
train
test <- ts(data$level[314832:315575, drop=FALSE])
test



plot(data_ts["2018-12-01/"])



# test for stationarity in mean
adf.test(train)


frequency(train)
acfpacf(train)
acfpacf(test)

# models

model_1 <- arima(train, order = c(5,0,0))
model_1
acfpacf(model_1$residuals, 240)

model_2 <- arima(train, order = c(5,0,0), seasonal = list(order=c(0,1,0), period=24))
model_2
acfpacf(model_2$residuals, 240)

model_3 <- arima(train, order = c(5,1,3), seasonal = list(order=c(0,1,1), period=24))
model_3
acfpacf(model_3$residuals, 240)

# model evaluation

preds <- predict(model_3, length(test))$pred
plot(train, xlim=c(1,(length(train)+length(test))), xaxt="n", ylab="", xlab="", main="Predictions SARIMA")
lines((length(train)+1):(length(train)+length(test)), as.vector(test), col="blue")
lines((length(train)+1):(length(train)+length(test)), ts(preds), col="red")


mse_mod <- MSE(preds, as.vector(test))
y_zero <- rep(mean(train), length(test))
mse_zero <- MSE(y_zero, as.vector(test))
mse_mod/mse_zero



# regressione con variabili meteo e sinusoidi deterministiche

freq  <- outer(1:nrow(data), 1:16) * 2 * pi / 365.25*12
sinu365  <- cbind(cos(freq), sin(freq))
colnames(sinu365) <- paste0("sinu365.", seq(1,ncol(sinu365)))
freq  <- outer(1:nrow(data), 1:6) * 2 * pi / 12*24
sinu24  <- cbind(cos(freq), sin(freq))
colnames(sinu24) <- paste0("sinu24.", seq(1,ncol(sinu24)))


# dati con variabili meteo
data <- read.csv("data/output/venezia_tides_weather.csv")
head(data)
print(which(data$datetime == "2002-12-01 00:00:00"))
print(which(data$datetime == "2002-12-02 00:00:00"))



mod1 <- lm(level~
             dir_wind*vel_wind + rain + rain:dir_wind:vel_wind 
           #+ sinu24
             #lag(level)
             #lag(level, 24)+
             #lag(level,12)
           #+ sinu365
           , data = data)

summary(mod1)

# sqrt(var(data$level))
# mean(data$level)

# valutazione grafica del modello

plot(data$level[174556:174629], type = 'l',
     main = "linear model",
     xlab = "Time (hours)",
     ylab = "Level")
lines(mod1$fitted[174556:174629], col = 'blue')
legend(1, 110, legend=c("Ground Truth", "Linear"),
       col=c("black", "blue"), lty=1, cex=0.8)

acfpacf(mod1$residuals)




reg <- cbind(data$dir_wind, data$vel_wind, data$rain)
mod_ar <- arima(data$level, order = c(1,0,0), xreg = reg)
mod_ar

arimax_fitted <- mod_ar$residuals + data$level
plot(data$level[174556:174629], type = 'l',
     main = "linear model vs arimax",
     ylim = c(-1, 130),
     xlab = "Time (hours)",
     ylab = "Level")
lines(mod1$fitted.values[174556:174629], col = 'blue')
lines(arimax_fitted[174556:174629], col = 'green')
legend(1, 130, legend=c("Ground Truth", "Linear", "Arimax"),
       col=c("black", "blue", 'green'), lty=1, cex=0.8)


mod_auto <- auto.arima(data$level, xreg = reg)
auto_arimax_fitted <- mod_auto$residuals+data$level

plot(data$level[174556:174629], type = 'l',
     main = 'linear model vs arimax vs auto_arimax',
     ylim = c(-1, 130),
     xlab = "Time (hours)",
     ylab = "Level")
lines(mod1$fitted.values[174556:174629], col = 'blue')
lines(arimax_fitted[174556:174629], col = 'green')
lines(auto_arimax_fitted[174556:174629], col = 'red')
legend(1, 133, legend=c("Ground Truth", "Linear", "Arimax", "Auto Arimax"),
       col=c("black", "blue", 'green', 'red'), lty=1, cex=0.8)


acfpacf(mod_auto$residuals)
