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
library(oce)
library(SWMPr)
library(KFAS)


acfpacf <- function(x, max.lag=36){
  require(forecast)
  par(mfrow = c(2,1))
  Acf(x, max.lag)
  Pacf(x, max.lag)
  par(mfrow = c(1,1))
}

# import data --------------------------------------------------------------

data <- read.csv("../data/output/df_final2010-2018.csv", header = T)
lunar_motion <- read.csv("../moon_distance/moon_distances.csv", header = T)

head(data)
tail(data)
str(data)
Hmisc::describe(data)

head(lunar_motion)
tail(lunar_motion)
str(lunar_motion)
Hmisc::describe(lunar_motion)

anyNA(data)
anyNA(lunar_motion)

jpeg("../report/imgs/data_plot.jpg", width = 600, height = 500, quality = 100)
ggtsdisplay(data$level, lag.max = 72, main = "Venice tides with autocorrelation plots", theme = theme_bw())
dev.off()
# definire train e test set ed integrare ciclo lunare -----------------------------------------------
data$l_motion <- 1/lunar_motion$dists.Km.^2
#rm(lunar_motion)

data[data$datetime=="2018-07-01 00:00:00",]
data <- data[74473:nrow(data),]
head(data)

trn <- data[1:(nrow(data)-168),] # train sono 6 mesi meno 1 settimana
head(trn)
tail(trn)
nrow(trn)
tst <- data[(nrow(data)-167):nrow(data),] # test 1 settimana
head(tst, 1)
tail(tst, 1)
nrow(tst)

ggtsdisplay(trn$level,
            main = "Venice tides 07/2018 12/2018",
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


# inspections -------------------------------------------------------------

ggplotly(autoplot(ts(data$level))) # utile per fare ispezione su valori anomali

# check for stationarity in variance ----------------------------------------

# convertiamo in df solo per verificare stazionarietà in varianza
jpeg(filename = "../report/imgs/var_stationary.jpg", width = 600, height = 500, quality = 100)
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
  theme_bw()
dev.off()

#ggplotly(pl)

# sembrerebbe esserci un certo trend crescente ma dovrebbe essere trascurabile, assumiamo che sia stazionaria
# in varianza


# verifichiamo la stazionarietà in media
jpeg("../report/imgs/mean_stationary.jpg", width = 600, height = 500, quality = 100)
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
           x = 3900,#as.POSIXct("2010-01-01 00:00:00"),
           size = 5,
           colour = "black") +
  labs(title = "Tides data for 2010-2018 years with mean (in red)",
       x = "Datetime",
       y = "Level") +
  theme_bw()
dev.off()
#ggplotly(pl2)

# a prima vista sembrerebbe essere stazionaria ma possiamo testare questa impressione


test_stat <- ur.df(data$level,
                   type = "drift",
                   lags = 24,
                   selectlags = "AIC")
summary(test_stat)

rm(list = c("test_stat", "pl", "pl2"))

# anche il test conferma essere stazionaria

jpeg("../report/imgs/dsitribution.jpg", width = 600, height = 500, quality = 100)
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
           x = 110,
           y = 0.012,
           size = 5) +
  theme_bw()
dev.off()

#ggplotly(pl)

# il fenomeno sembrerebbe distribuirsi normalmente

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
summary(mod1) # R2 4.7%
acfpacf(mod1$residuals)
plot(trn_norm$level, type = "l")
lines(mod1$fitted.values, col = "red")
abline(h = mean(trn_norm$level), col = "blue", lty = 2, lwd = 2)

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

pred <- predict(mod1, newdata = tst_norm, interval = 'prediction')
nrow(pred)
head(pred)

plot(tst_norm$level, type = "l")
lines(pred[,1], col = "red")
abline(h = mean(tst_norm$level), col = "blue", lty = 2, lwd = 2)

# variabili meteo e ciclo lunare con interazioni fittato con metodo stepwise basato su AIC

mod2 <- lm(level ~ ., data = trn_norm)
final_model <- step(mod2, scope = . ~ .^2, direction = 'both')
summary(final_model) #R2 5.7%
acfpacf(mod2$residuals)

pred2 <- predict(final_model, newdata = tst_norm, interval = 'prediction')
nrow(pred2)
head(pred2)


plot(tst_norm$level, type = "l")
lines(pred2[,1], col = "red")
abline(h = mean(tst_norm$level), col = "blue", lty = 2, lwd = 2)

rm(list = c("final_model", "mod1", "mod2", "pred", "pred2"))

## al netto di queste prove possiamo vedere come le covariate non sembrino spiegare molto.. proviamo con modelli ARIMA

# ARIMA -------------------------------------------------------------------

# model 1

# i regressori se inseriti all'interno del modello ARIMA sembrerebbero non essere proprio
# significativi

acfpacf(trn_norm$level, max.lag = 100)
xreg <- matrix(c(trn_norm$rain, trn_norm$vel_wind,
         trn_norm$dir_wind, trn_norm$l_motion), ncol = 4)

col_names <- c("rain", "vel_wind", "dir_wind", "l_motion")
colnames(xreg) <- col_names
#(3,1,3)(1,1,3)[24]
mod1_ar <- Arima(trn_norm$level+0.01, xreg = xreg,
                 include.drift = T,
                 c(3,1,3)
                 , list(order = c(1,1,3), period = 24))
summary(mod1_ar)
acfpacf(mod1_ar$residuals, max.lag = 100)
Box.test(mod1_ar$residuals, type="Ljung-Box")

xreg_test <- matrix(c(tst_norm$rain, tst_norm$vel_wind,
                 tst_norm$dir_wind, tst_norm$l_motion), ncol = 4)
colnames(xreg_test) <- col_names

pred1_ar <- forecast(mod1_ar, h = 168, xreg = xreg_test)
#ggplotly(autoplot(pred1_ar))

plot(tst_norm$level, type = "l")
lines(as.numeric(pred1_ar_168$mean), col = "red")

pander::pandoc.table(matrix(c(RMSE(pred1_ar$mean[1], tst_norm$level[1]),
MSE(pred1_ar$mean[1], tst_norm$level[1]),
MAE(pred1_ar$mean[1], tst_norm$level[1]),
MAPE(pred1_ar$mean[1]+0.01, tst_norm$level[1]+0.01),
RMSE(mod1_ar$fitted, trn_norm$level),
MSE(mod1_ar$fitted, trn_norm$level),
MAE(mod1_ar$fitted, trn_norm$level),
MAPE(mod1_ar$fitted+0.01, trn_norm$level+0.01)), ncol = 2, nrow = 4, dimnames = list(c("RMSE", "MSE", "MAE",
                                                                              "MAPE"),
                                                                          c("Test", "Train"))), caption = "1 step ahead prediction")



pander::pandoc.table(matrix(c(RMSE(pred1_ar$mean[1:24], tst_norm$level[1:24]),
                        MSE(pred1_ar$mean[1:24], tst_norm$level[1:24]),
                        MAE(pred1_ar$mean[1:24], tst_norm$level[1:24]),
                        MAPE(pred1_ar$mean[1:24]+0.01, tst_norm$level[1:24]+0.01),
                        RMSE(mod1_ar$fitted, trn_norm$level),
                        MSE(mod1_ar$fitted, trn_norm$level),
                        MAE(mod1_ar$fitted, trn_norm$level),
                        MAPE(mod1_ar$fitted+0.01, trn_norm$level+0.01)), ncol = 2, nrow = 4, dimnames = list(c("RMSE", "MSE", "MAE",
                                                                                                           "MAPE"),
                                                                                                         c("Test", "Train"))),
                     caption = "24 steps ahead prediction")


pander::pandoc.table(matrix(c(RMSE(pred1_ar$mean, tst_norm$level),
                        MSE(pred1_ar$mean, tst_norm$level),
                        MAE(pred1_ar$mean, tst_norm$level),
                        MAPE(pred1_ar$mean+0.01, tst_norm$level+0.01),
                        RMSE(mod1_ar$fitted, trn_norm$level),
                        MSE(mod1_ar$fitted, trn_norm$level),
                        MAE(mod1_ar$fitted, trn_norm$level),
                        MAPE(mod1_ar$fitted+0.01, trn_norm$level+0.01)), ncol = 2, nrow = 4, dimnames = list(c("RMSE", "MSE", "MAE",
                                                                                                           "MAPE"),
                                                                                                         c("Test", "Train"))),
                     caption = "168 steps ahead prediction")

################# oce & SWMPr -------------
# Direi di escludere le variabili meteo e ciclo perchè appena subentrano gli AR diventano non significative

data <- read.csv("data/output/df_final2010-2018.csv", header = T)
datsl <- as.sealevel(data$level/100)

plot(datsl)

# tidal components to estimate
# M2 principal lunar semi-diurnal
# S2 "" solar
# N2 periodo lunare ellittico
# K2 inclinazione sole-luna
# K1 inclinazione sole-luna 
constituents <- c('M2', 'S2', 'N2', 'K2', 'K1', 'O1', 'SA', 'P1')

# loop through tidal components, predict each with tidem
preds <- sapply(constituents, function(x){
  
  mod <- tidem(t = datsl, constituent = x)
  pred <- predict(mod)
  pred - mean(pred)
  
}) 

# combine prediction, sum, add time data
predall <- rowSums(preds) + mean(datsl[['elevation']])
preds <- data.frame(time = datsl[['time']], preds, Estimated = predall) 

head(preds)

plot(data$level[1:168]/100, type = "l")
lines(preds$Estimated[1:168], col = "red")

data$M2 <- preds$M2
data$S2 <- preds$S2
data$N2 <- preds$N2
data$K2 <- preds$K2
data$K1 <- preds$K1
data$O1 <- preds$O1
data$SA <- preds$SA
data$P1 <- preds$P1


head(data)

mod1 <- lm(level/100 ~ M2+
             S2+
             N2+
             K2+K1+
             O1+
             SA+
             P1+
             as.vector(scale(rain))+
             as.vector(scale(vel_wind))+
             as.vector(scale(dir_wind))+
             as.vector(scale(l_motion)),
           data = data)
summary(mod1) #R2 75,5%
acfpacf(mod1$residuals) # AR2 with 1D
trn <- data[74473:(nrow(data)-168),]
tst <- data[(nrow(data)-167):nrow(data),]
tail(trn)
head(tst)

xreg <- matrix(c(trn$M2, trn$S2, trn$N2, trn$K2, trn$K1, trn$O1, trn$SA, trn$P1), ncol = 8)

col_names <- c("M2", "S2", "N2","K2", "K1", "O1", "SA", "P1")
colnames(xreg) <- col_names

#(3,0,1)(0,0,0)[24]
mod2_ar <- Arima((trn$level/100)+0.001, xreg = xreg,
                 include.drift = T,
                 c(3,1,0)
                 , list(order = c(0,0,0), period = 24)
)

summary(mod2_ar)
acfpacf(mod2_ar$residuals)
Box.test(mod2_ar$residuals, type="Ljung-Box")

xreg_test <- matrix(c(tst$M2, tst$S2, tst$N2, tst$K2, tst$K1, tst$O1, tst$SA, tst$P1), ncol = 8)
colnames(xreg_test) <- col_names

pred2_ar <- forecast(mod2_ar, h = 168, xreg = xreg_test)
ggplotly(autoplot(pred2_ar))

plot(tst$level, type = "l")
lines(as.numeric(pred2_ar$mean)*100, col = "red")
lines(as.numeric(pred1_ar$mean), col = "blue")

pander::pandoc.table(matrix(c(RMSE(pred2_ar$mean[1]*100, tst$level[1]),
         MSE(pred2_ar$mean[1]*100, tst$level[1]),
         MAE(pred2_ar$mean[1]*100, tst$level[1]),
         MAPE((pred2_ar$mean[1]*100)+0.01, tst$level[1]+0.01),
         RMSE(mod2_ar$fitted*100, trn$level),
         MSE(mod2_ar$fitted*100, trn$level),
         MAE(mod2_ar$fitted*100, trn$level),
         MAPE((mod2_ar$fitted*100)+0.01, trn$level+0.01)),
       ncol = 2, nrow = 4, dimnames = list(c("RMSE", "MSE", "MAE", "MAPE"),
                                           c("Test", "Train"))),
       caption = "1 step ahead prediction")

pander::pandoc.table(matrix(c(RMSE(pred2_ar$mean[1:24]*100, tst$level[1:24]),
                              MSE(pred2_ar$mean[1:24]*100, tst$level[1:24]),
                              MAE(pred2_ar$mean[1:24]*100, tst$level[1:24]),
                              MAPE((pred2_ar$mean[1:24]*100)+0.01, tst$level[1:24]+0.01),
                              RMSE(mod2_ar$fitted*100, trn$level),
                              MSE(mod2_ar$fitted*100, trn$level),
                              MAE(mod2_ar$fitted*100, trn$level),
                              MAPE((mod2_ar$fitted*100)+0.01, trn$level+0.01)),
                            ncol = 2, nrow = 4, dimnames = list(c("RMSE", "MSE", "MAE", "MAPE"),
                                                                c("Test", "Train"))),
                     caption = "24 step ahead prediction")

pander::pandoc.table(matrix(c(RMSE(pred2_ar$mean*100, tst$level),
                              MSE(pred2_ar$mean*100, tst$level),
                              MAE(pred2_ar$mean*100, tst$level),
                              MAPE((pred2_ar$mean*100)+0.01, tst$level+0.01),
                              RMSE(mod2_ar$fitted*100, trn$level),
                              MSE(mod2_ar$fitted*100, trn$level),
                              MAE(mod2_ar$fitted*100, trn$level),
                              MAPE((mod2_ar$fitted*100)+0.01, trn$level+0.01)),
                            ncol = 2, nrow = 4, dimnames = list(c("RMSE", "MSE", "MAE", "MAPE"),
                                                                c("Test", "Train"))),
                     caption = "168 step ahead prediction")

