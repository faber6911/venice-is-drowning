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


acfpacf <- function(x, max.lag=36){
  require(forecast)
  par(mfrow = c(2,1))
  Acf(x, max.lag)
  Pacf(x, max.lag)
  par(mfrow = c(1,1))
}

# import data --------------------------------------------------------------

data <- read.csv("data/output/venezia.csv", header = T)
head(data)
str(data)
#data <- as.data.frame(data)
#data$datetime <- as.character(data$datetime)
#head(data)
#anyNA(data$datetime)

#data$datetime <- as.POSIXct(data$datetime, "%Y-%m-%d %H:%M:%OS", tz =  "Europe/Berlin")
#head(data)

# inspections -------------------------------------------------------------


date <- seq(from=as.POSIXct(data$datetime[1],
                            format = "%Y-%m-%d %H:%M:%OS",
                            tz = "Europe/Berlin"),
            length.out = nrow(data),
            by = "1 hour")
tail(date)

data_ts <- xts(order.by = data$datetime,
               x = data$level,
               frequency = 24,
               tzone = "Europe/Berlin")

plot(data_ts, main = "Venice tides")


plot(data_ts["2016-12-31 23:00:00/2018-12-31 23:00:00"], main = "Venice tides 2017 2018")
df <- data_ts["2016-12-31 23:00:00/2018-12-31 23:00:00"]
length(index(df)) == length(seq(from = start(df), to = end(df), by = "hour"))

length(index(df))
date <- seq(from = start(df),
            length.out = length(index(df)),
            by = "1 hour")

# check for stationarity in variance ----------------------------------------

# convertiamo in df solo per verificare stazionarietà in varianza
ddf <- data.frame(level = coredata(df), datetime = index(df))
head(ddf)

ddf %>%
  add_column(date = date(ddf$datetime)) %>%
  group_by(date) %>% 
  mutate(mean_day = mean(level), sd_day = sd(level)) %>%
  select(date, mean_day, sd_day) %>% 
  distinct(date, mean_day, sd_day) %>%
  ggplot(aes(mean_day, sd_day)) + 
  geom_point() + 
  ggtitle("Mean per day vs Std per day") +
  labs(x = "Day mean",
       y = "Day std") +
  geom_smooth(method = "lm", se = FALSE)+
  theme_bw()

# sembrerebbe esserci un certo trend crescente ma dovrebbe essere trascurabile, assumiamo che sia stazionaria
# in varianza


# verifichiamo la stazionarietà in media

plot(ddf$level, type = "l")
abline(h = mean(coredata(df)), col = "red", lty = 2, lwd = 0.8)

# a prima vista sembrerebbe essere stazionaria ma possiamo testare questa impressione


library(urca)
test_stat <- ur.df(ddf$level, "drift", lags = 24, selectlags = "AIC")
summary(test_stat)
# anche il test conferma essere stazionaria

hist(ddf$level, prob = TRUE, col = "lightblue", main = "Tides density plot", xlab = "Level")
lines(density(ddf$level), col = "red", lwd = 2, lty = 2)
abline(v = mean(ddf$level), col = "blue", lwd = 2, lty = 2)


# il fenomeno sembrerebbe distribuirsi normalmente

mod1 <- auto.arima(ts(ddf$level, frequency = 12), stepwise = F)
summary(mod1)
mod2 <- auto.arima(ddf$level)
summary(mod2)


acfpacf(mod1$residuals)
acfpacf(mod2$residuals)
############# DA QUI IN POI DEPRECATO ##############

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


# SARIMA ------------------------------------------------------------------

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



# regression --------------------------------------------------------------
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


# linear model ------------------------------------------------------------

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



# ARIMAX ------------------------------------------------------------------

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

# Auto ARIMAX -------------------------------------------------------------

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
