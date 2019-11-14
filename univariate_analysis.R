rm(list = ls()) # clean environment

# packages ----------------------------------------------------------------

library(xts)
library(lubridate)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(forecast)
library(tseries)
library(MLmetrics)

# import data --------------------------------------------------------------

data <- read.table("data/output/venezia_xts.csv", sep = " ", header = T)
head(data)
str(data)
#data <- as.data.frame(data)
data$datetime <- as.character(data$datetime)
#head(data)
#anyNA(data$datetime)

# inspections -------------------------------------------------------------


date <- seq(from=as.POSIXct(data$datetime[1],
                            format = "%Y-%m-%d %H:%M:%OS",
                            tz = "Europe/Berlin"),
            length.out = nrow(data),
            by = "1 hour")
tail(date)

data_ts <- xts(order.by = date,
               x = data$level,
               frequency = 24,
               tzone = "Europe/Berlin")

plot(data_ts, main = "Venice tides")

# check for stationarity in variance ----------------------------------------

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
  geom_smooth(method = "lm", se = FALSE)+
  theme_bw()


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

acfpacf <- function(x, max.lag=36){
  require(forecast)
  par(mfrow = c(2,1))
  Acf(x, max.lag)
  Pacf(x, max.lag)
  par(mfrow = c(1,1))
}

adf.test(train)
frequency(train)
acfpacf(train)
acfpacf(test)

# models

model_1 <- arima(train, order = c(5,0,0))
model_1
acfpacf(model_1$residuals, 240)


# model_auto <- auto.arima(train, seasonal = F)
# model_auto
# acfpacf(model_auto$residuals, 240)

model_2 <- arima(train, order = c(5,0,0), seasonal = list(order=c(0,1,0), period=24))
model_2
acfpacf(model_2$residuals, 240)



model_3 <- arima(train, order = c(5,1,3), seasonal = list(order=c(0,1,1), period=24))
model_3
acfpacf(model_3$residuals, 240)

#model evaluation

preds <- predict(model_3, length(test))$pred
plot(train, xlim=c(1,(length(train)+length(test))), xaxt="n", ylab="", xlab="", main="Predictions SARIMA")
lines((length(train)+1):(length(train)+length(test)), as.vector(test), col="blue")
lines((length(train)+1):(length(train)+length(test)), ts(preds), col="red")


mse_mod <- MSE(preds, as.vector(test))
y_zero <- rep(mean(train), length(test))
mse_zero <- MSE(y_zero, as.vector(test))
mse_mod/mse_zero
