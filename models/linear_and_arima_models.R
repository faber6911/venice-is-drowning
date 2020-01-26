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
library(patchwork)
library(zoo)

acfpacf <- function(x, max.lag=36){
  require(forecast)
  par(mfrow = c(1,2))
  Acf(x, max.lag)
  Pacf(x, max.lag)
  par(mfrow = c(1,1))
}

ggplot.corr <- function(data, lag.max = 24, ci = 0.95, large.sample.size = TRUE, horizontal = TRUE,...) {
  
  require(ggplot2)
  require(dplyr)
  require(cowplot)
  
  if(horizontal == TRUE) {numofrow <- 1} else {numofrow <- 2}
  
  list.acf <- acf(data, lag.max = lag.max, type = "correlation", plot = FALSE)
  N <- as.numeric(list.acf$n.used)
  df1 <- data.frame(lag = list.acf$lag, acf = list.acf$acf)
  df1$lag.acf <- dplyr::lag(df1$acf, default = 0)
  df1$lag.acf[2] <- 0
  df1$lag.acf.cumsum <- cumsum((df1$lag.acf)^2)
  df1$acfstd <- sqrt(1/N * (1 + 2 * df1$lag.acf.cumsum))
  df1$acfstd[1] <- 0
  df1 <- select(df1, lag, acf, acfstd)
  
  list.pacf <- acf(sunspot.year, lag.max = lag.max, type = "partial", plot = FALSE)
  df2 <- data.frame(lag = list.pacf$lag,pacf = list.pacf$acf)
  df2$pacfstd <- sqrt(1/N)
  
  if(large.sample.size == TRUE) {
    plot.acf <- ggplot(data = df1, aes( x = lag, y = acf)) +
      geom_area(aes(x = lag, y = qnorm((1+ci)/2)*acfstd), fill = "#B9CFE7") +
      geom_area(aes(x = lag, y = -qnorm((1+ci)/2)*acfstd), fill = "#B9CFE7") +
      geom_col(fill = "#4373B6", width = 0.7) +
      scale_x_continuous(breaks = seq(0,max(df1$lag),6)) +
      scale_y_continuous(name = element_blank(), 
                         limits = c(min(df1$acf,df2$pacf),1)) +
      ggtitle("ACF") +
      theme_bw()
    
    plot.pacf <- ggplot(data = df2, aes(x = lag, y = pacf)) +
      geom_area(aes(x = lag, y = qnorm((1+ci)/2)*pacfstd), fill = "#B9CFE7") +
      geom_area(aes(x = lag, y = -qnorm((1+ci)/2)*pacfstd), fill = "#B9CFE7") +
      geom_col(fill = "#4373B6", width = 0.7) +
      scale_x_continuous(breaks = seq(0,max(df2$lag, na.rm = TRUE),6)) +
      scale_y_continuous(name = element_blank(),
                         limits = c(min(df1$acf,df2$pacf),1)) +
      ggtitle("PACF") +
      theme_bw()
  }
  else {
    plot.acf <- ggplot(data = df1, aes( x = lag, y = acf)) +
      geom_col(fill = "#4373B6", width = 0.7) +
      geom_hline(yintercept = qnorm((1+ci)/2)/sqrt(N), 
                 colour = "sandybrown",
                 linetype = "dashed") + 
      geom_hline(yintercept = - qnorm((1+ci)/2)/sqrt(N), 
                 colour = "sandybrown",
                 linetype = "dashed") + 
      scale_x_continuous(breaks = seq(0,max(df1$lag),6)) +
      scale_y_continuous(name = element_blank(), 
                         limits = c(min(df1$acf,df2$pacf),1)) +
      ggtitle("ACF") +
      theme_bw()
    
    plot.pacf <- ggplot(data = df2, aes(x = lag, y = pacf)) +
      geom_col(fill = "#4373B6", width = 0.7) +
      geom_hline(yintercept = qnorm((1+ci)/2)/sqrt(N), 
                 colour = "sandybrown",
                 linetype = "dashed") + 
      geom_hline(yintercept = - qnorm((1+ci)/2)/sqrt(N), 
                 colour = "sandybrown",
                 linetype = "dashed") + 
      scale_x_continuous(breaks = seq(0,max(df2$lag, na.rm = TRUE),6)) +
      scale_y_continuous(name = element_blank(),
                         limits = c(min(df1$acf,df2$pacf),1)) +
      ggtitle("PACF") +
      theme_bw()
  }
  cowplot::plot_grid(plot.acf, plot.pacf, nrow = numofrow)
}

# import data --------------------------------------------------------------

data <- read.csv("../data/output/df_final2010-2018.csv", header = T)
lunar_motion <- read.csv("../moon_distance/moon_distances.csv", header = T)

# lunar_plot <- ggplot(aes(y = dists.Km., x = as.POSIXct(datetime, tz = "UTC")), data = lunar_motion) +
#   geom_line() + theme_bw()+ labs(x = "Time", y = "Dist Km", title = "Lunar motion from 01-2010 to 12-2018")
# pl <- ggplotly(lunar_plot, width = 600, height = 300)
# htmlwidgets::saveWidget(as_widget(pl), "test.html", )


# dummy_rain <- read.csv("../data/df_final_processed.csv", header = T)
# 
# head(data)
# tail(data)
# str(data)
# Hmisc::describe(data)
# 
# head(lunar_motion)
# tail(lunar_motion)
# str(lunar_motion)
# Hmisc::describe(lunar_motion)
# 
# anyNA(data)
# anyNA(lunar_motion)
# p1 <- ggplot(aes(y = level, x = as.POSIXct(datetime, tz = "UTC")), data = data[74305:(nrow(data)-336),]) +
#   geom_line() +
#   theme_bw() +
#   labs(x = "datetime", y = "", title = "Linear models insample (06/2018 - 12/2018)")
# p2 <- ggplot(aes(y = level, x = as.POSIXct(datetime, tz = "UTC")), data = data[1:(nrow(data)-336),]) +
#   geom_line() +
#   theme_bw() +
#   labs(x = "datetime", y = "level", title = "Machine learning insample (01/2010 - 12/2018)")
# 
# label <- numeric(336)
# label[1:167] <- 0
# label[168:length(label)] <- 1
# p3 <- ggplot(aes(y = level, x = as.POSIXct(datetime, tz = "UTC"), color = factor(label)), data = data[(nrow(data)-335):nrow(data),]) +
#   geom_line() + theme_bw() + labs(x = "datetime", y = "level", title = "Common test set (17/12/2018 - 31/12/2018)") +
#   scale_colour_manual(values=c("darkgrey", "darkred"), name = "", labels = c("validation", "test"))
# 
# jpeg("../report/imgs/train_test.jpg", width = 1200, height = 500, quality = 100)
# (p2 | p1)/p3
# dev.off()
# 
# jpeg("../report/imgs/data_plot.jpg", width = 600, height = 500, quality = 100)
# ggtsdisplay(data$level, lag.max = 72, main = "Venice tides with autocorrelation plots", theme = theme_bw())
# dev.off()

# definire train e test set ed integrare ciclo lunare -----------------------------------------------
data$l_motion <- 1/lunar_motion$dists.Km.^2
#data$dummy_rain <- dummy_rain$X0
#rm(lunar_motion)

data[data$datetime=="2018-06-24 00:00:00",]
#data <- data[74305:nrow(data),]
#head(data)

trn <- data[74305:(nrow(data)-336),] # train sono 6 mesi meno 1 settimana
head(trn)
tail(trn)
nrow(trn)
tst <- data[(nrow(data)-335):nrow(data),] # test 1 settimana
head(tst, 1)
tail(tst, 1)
nrow(tst)

ggtsdisplay(trn$level,
            main = "Venice tides 06/2018 12/2018",
            lag.max = 72,
            theme = theme_bw())

ggtsdisplay(tst$level,
            main = "Venice tides last two weeks 2018",
            lag.max = 72,
            theme = theme_bw())

# a giudicare dai grafici ACF e PACF sembrerebbe trattarsi di un modello MA stagionale
# poichè PACF decresce geometricamente mentre ACF presenta dei picchi stagionali

#jpeg(filename = "../report/imgs/periodogram.jpg", width = 500, height = 300, quality = 100)
p <- periodogram(data$level)
#dev.off()
dd <- data.frame(freq = p$freq, spec = p$spec, time = 1/p$freq)
order <- dd[order(-dd$spec),]
top10 <- head(order, 10)
top10

#rm(list = c("p", "dd", "order", "top10"))

# è palese da ciò come siano presenti due stagionalità, una su 12 lag e una 24


# inspections -------------------------------------------------------------

ggplotly(autoplot(ts(data$level))) # utile per fare ispezione su valori anomali

# check for stationarity in variance ----------------------------------------

# convertiamo in df solo per verificare stazionarietà in varianza
# jpeg(filename = "../report/imgs/var_stationary.jpg", width = 600, height = 500, quality = 100)
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
# dev.off()

#ggplotly(pl)

# sembrerebbe esserci un certo trend crescente ma dovrebbe essere trascurabile, assumiamo che sia stazionaria
# in varianza


# verifichiamo la stazionarietà in media

#jpeg("../report/imgs/mean_stationary.jpg", width = 600, height = 500, quality = 100)
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
#dev.off()
#ggplotly(pl2)

# a prima vista sembrerebbe essere stazionaria ma possiamo testare questa impressione


test_stat <- ur.df(data$level,
                   type = "drift",
                   lags = 24,
                   selectlags = "AIC")
summary(test_stat)

#rm(list = c("test_stat", "pl", "pl2"))

# anche il test conferma essere stazionaria

#jpeg("../report/imgs/dsitribution.jpg", width = 600, height = 500, quality = 100)
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
#dev.off()

#ggplotly(pl)

# il fenomeno sembrerebbe distribuirsi normalmente

# linear models -----------------------------------------------------------
# è necessario verificare l'impatto delle variabili esterne sulla predizione del livello delle maree
# prima di procedere a realizzare un modello ARMA o ARIMA

# solo variabili meteo additive con ciclo lunare normalizzato

#trn <- data[1:(nrow(data)-168),]
#tst <- data[(nrow(data)-167):nrow(data),]

# rain_norm <- as.vector(scale(trn$rain))
# vel_wind_norm <- as.vector(scale(trn$vel_wind))
# dir_wind_norm <- as.vector(scale(trn$dir_wind))
# l_motion_norm <- as.vector(scale(trn$l_motion))
# trn_norm <- data.frame(level = trn$level, rain = rain_norm, vel_wind = vel_wind_norm,
#                        dir_wind = dir_wind_norm,
#                        l_motion = l_motion_norm)
# 
# rm(list = c("trn", "rain_norm", "vel_wind_norm", "dir_wind_norm", "l_motion_norm"))
# 
# mod1 <- lm(formula = level ~ rain + 
#              vel_wind +
#              dir_wind + 
#              l_motion,
#            data = trn_norm)
# summary(mod1) # R2 4.7%
# acfpacf(mod1$residuals)
# plot(trn_norm$level, type = "l")
# lines(mod1$fitted.values, col = "red")
# abline(h = mean(trn_norm$level), col = "blue", lty = 2, lwd = 2)
# 
# rain_norm_test <- as.vector(scale(tst$rain))
# vel_wind_norm_test <- as.vector(scale(tst$vel_wind))
# dir_wind_norm_test <- as.vector(scale(tst$dir_wind))
# l_motion_norm_test <- as.vector(scale(tst$l_motion))
# tst_norm <- data.frame(level = tst$level, rain = rain_norm_test, vel_wind = vel_wind_norm_test,
#                        dir_wind = dir_wind_norm_test,
#                    l_motion = l_motion_norm_test)
# 
# rm(list = c("tst", "rain_norm_test", "vel_wind_norm_test", "dir_wind_norm_test", "l_motion_norm_test"))
# head(tst_norm)
# tail(tst_norm)
# nrow(tst_norm)
# 
# pred <- predict(mod1, newdata = tst_norm, interval = 'prediction')
# nrow(pred)
# head(pred)
# 
# plot(tst_norm$level, type = "l")
# lines(pred[,1], col = "red")
# abline(h = mean(tst_norm$level), col = "blue", lty = 2, lwd = 2)
# 
# # variabili meteo e ciclo lunare con interazioni fittato con metodo stepwise basato su AIC
# 
# mod2 <- lm(level ~ ., data = trn_norm)
# final_model <- step(mod2, scope = . ~ .^2, direction = 'both')
# summary(final_model) #R2 5.7%
# acfpacf(mod2$residuals)
# 
# pred2 <- predict(final_model, newdata = tst_norm, interval = 'prediction')
# nrow(pred2)
# head(pred2)
# 
# 
# plot(tst_norm$level, type = "l")
# lines(pred2[,1], col = "red")
# abline(h = mean(tst_norm$level), col = "blue", lty = 2, lwd = 2)
# 
# rm(list = c("final_model", "mod1", "mod2", "pred", "pred2"))

## al netto di queste prove possiamo vedere come le covariate non sembrino spiegare molto.. proviamo con modelli ARIMA

# ARIMA -------------------------------------------------------------------

# model 1

# i regressori se inseriti all'interno del modello ARIMA sembrerebbero non essere proprio
# significativi

acfpacf(trn$level, max.lag = 100)

xreg <- matrix(c(as.vector(scale(trn$rain)),
                 as.vector(scale(trn$vel_wind)),
                 as.vector(scale(trn$dir_wind)),
                 as.vector(scale(trn$l_motion))), ncol = 4)


# freq <- outer(1:nrow(trn_norm), 1:6)*2*pi/7
# 
# cs   <- cos(freq)                   
# colnames(cs) <- paste("cos", 1:6)
# si   <- sin(freq)                   
# colnames(si) <- paste("sin", 1:6)
# 
# more_reg <- as.matrix(cbind(cs,si))
# 
# xreg <- cbind(xreg, more_reg) 

col_names <- c("rain", "vel_wind", "dir_wind", "l_motion")
colnames(xreg) <- col_names

xreg_test <- matrix(c(as.vector(scale(tst$rain)),
                      as.vector(scale(tst$vel_wind)),
                      as.vector(scale(tst$dir_wind)),
                      as.vector(scale(tst$l_motion))), ncol = 4)

colnames(xreg_test) <- col_names

xreg <- rbind(xreg, xreg_test)

#(3,1,3)(1,1,3)[24]
# 
# mod_ar <- Arima(trn_norm$level+0.001, xreg = xreg,
#                 include.drift = T,
#                 c(0,0,0), list(order = c(0,0,0), period = 24))
# 
# summary(mod_ar)
# acfpacf(mod_ar$residuals)
#trn[trn$level<0 & trn$level>=(-1),]
mod1_ar <- Arima(trn$level+0.1, xreg = xreg[1:4248,,drop=FALSE],
                 include.drift = F,
                 c(3,1,3), list(order = c(1,1,3), period = 24))

summary(mod1_ar)
acfpacf(mod1_ar$residuals, max.lag = 100)
p1 <- ggplot.corr(mod1_ar$residuals, max.lag = 100, large.sample.size = T, horizontal = T)
p1
Box.test(mod1_ar$residuals, type="Ljung-Box")
p2 <- autoplot(ts(trn$level), series = "true")+theme_bw()+labs(y = "level")+autolayer(ts(mod1_ar$fitted), series = "fitted")
p2

#jpeg("../report/imgs/ar1_2_acfpacf.jpg", width = 600, height = 300, quality = 100)
#p1/p2
#dev.off()

y <- rbind(trn, tst)
y <- y$level
#y <- c(y, rep(NA, 168)) # add NA's for data we cannot compare

# 1 step ahead predictions
score <- numeric(168)
for (i in 1:168){
  mod_prod <- Arima(y[i:(4247+i)], xreg = xreg[i:(4247+i),,drop=F], model = mod1_ar)
  pred <- forecast(mod_prod, xreg = xreg[(4248+i):(4248+i),,drop=FALSE], h = 1)$mean
  score[i] <- MAPE(pred, y[(4248 + i):(4248 + i)]+0.1)
}
pred_1step1 <- mean(score)

# 24 step ahead predictions
score <- numeric(168)
for (i in 1:168){
  mod_prod <- Arima(y[i:(4247+i)], xreg = xreg[i:(4247+i),,drop=F], model = mod1_ar)
  pred <- forecast(mod_prod, xreg = xreg[(4248+i):(4248+i+23),,drop=FALSE], h = 24)$mean
  score[i] <- MAPE(pred, y[(4248 + i):(4248 + i + 23)]+0.1)
}
pred_24step1 <- mean(score)

# 168 steps ahead predictions
score <- numeric(168)
for (i in 1:168){
  mod_prod <- Arima(y[i:(4247+i)], xreg = xreg[i:(4247+i),,drop=F], model = mod1_ar)
  pred <- forecast(mod_prod, xreg = xreg[(4248+i):(4248+i+167),,drop=FALSE], h = 168)$mean
  score[i] <- MAPE(pred, y[(4248 + i):(4248 + i + 167)]+0.1)
}
pred_168step1 <- mean(score)

pander::pandoc.table(matrix(c(pred_1step1, pred_24step1, pred_168step1), nrow = 1, ncol = 3,
                            dimnames = list(c("mod1_ar"), c("1-step", "24-steps", "168-steps"))),
                     style = "simple", caption = "MAPE values")



pred1_ar <- forecast(mod1_ar, h = 168, xreg = xreg[4249:(4249+167),])
#ggplotly(autoplot(pred1_ar))
# 
# plot(tst_norm$level, type = "l")
# lines(as.numeric(pred1_ar_168$mean), col = "red")
# 
pander::pandoc.table(matrix(c(RMSE(pred1_ar$mean[1], y[4249]),
MSE(pred1_ar$mean[1], y[4249]),
MAE(pred1_ar$mean[1], y[4249]),
MAPE(pred1_ar$mean[1], y[4249]+0.1),
RMSE(mod1_ar$fitted, y[1:4248]),
MSE(mod1_ar$fitted, y[1:4248]),
MAE(mod1_ar$fitted, y[1:4248]),
MAPE(mod1_ar$fitted, y[1:4248]+0.1)), ncol = 2, nrow = 4,
dimnames = list(c("RMSE", "MSE", "MAE", "MAPE"), c("Test", "Train"))),
caption = "1 step ahead one shot prediction")

# 
# 
# 

pander::pandoc.table(matrix(c(RMSE(pred1_ar$mean[1:24], y[4249:(4249+23)]),
                        MSE(pred1_ar$mean[1:24], y[4249:(4249+23)]),
                        MAE(pred1_ar$mean[1:24], y[4249:(4249+23)]),
                        MAPE(pred1_ar$mean[1:24], y[4249:(4249+23)]+0.1),
                        RMSE(mod1_ar$fitted, y[1:4248]),
                        MSE(mod1_ar$fitted, y[1:4248]),
                        MAE(mod1_ar$fitted, y[1:4248]),
                        MAPE(mod1_ar$fitted, y[1:4248]+0.1)), ncol = 2, nrow = 4, dimnames = list(c("RMSE", "MSE", "MAE",
                                                                                                           "MAPE"),
                                                                                                         c("Test", "Train"))),
                     caption = "24 steps ahead one shot prediction")
# 
# 
pander::pandoc.table(matrix(c(RMSE(pred1_ar$mean, y[4249:(4249+167)]),
                        MSE(pred1_ar$mean, y[4249:(4249+167)]),
                        MAE(pred1_ar$mean, y[4249:(4249+167)]),
                        MAPE(pred1_ar$mean, y[4249:(4249+167)]+0.1),
                        RMSE(mod1_ar$fitted, y[1:4248]),
                        MSE(mod1_ar$fitted, y[1:4248]),
                        MAE(mod1_ar$fitted, y[1:4248]),
                        MAPE(mod1_ar$fitted, y[1:4248]+0.1)), ncol = 2, nrow = 4, dimnames = list(c("RMSE", "MSE", "MAE",
                                                                                                           "MAPE"),
                                                                                                         c("Test", "Train"))),
                     caption = "168 steps ahead one shot prediction")



################# oce & SWMPr -------------

# Direi di escludere le variabili meteo e ciclo perchè appena subentrano gli AR diventano non significative

data <- read.csv("../data/output/df_final2010-2018.csv", header = T)
lunar_motion <- read.csv("../moon_distance/moon_distances.csv", header = T)
#dummy_rain <- read.csv("../data/df_final_processed.csv", header = T)
data$l_motion <- 1/lunar_motion$dists.Km.^2
#data$dummy_rain <- dummy_rain$dummy_rain
#data$dummy_wind <- dummy_rain$vel_wind_dummy
#pad <- rep(0, 168)
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

#head(preds)

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

# 
# head(data)
# mod1 <- lm(level ~ 
#              # M2+
#              # S2+
#              # N2+
#              # K2+K1+
#              # O1+
#              # SA+
#              # P1+
#              as.factor(dummy_rain)*
#              as.vector(scale(rain))+
#              as.factor(dummy_wind)*
#             as.vector(scale(vel_wind))*
#              as.vector(scale(dir_wind))+
#              as.vector(scale(l_motion)),
#            data = data)
# summary(mod1) #R2 75,5%
#acfpacf(mod1$residuals) # AR2 with 1D

#data$dummy_regressor <- as.factor(data$dummy_regressor)
#View(data)

trn <- data[74305:(nrow(data)-336),]
tst <- data[(nrow(data)-335):nrow(data),]

col_names <- c("M2", "S2", "N2","K2", "K1", "O1", "SA", "P1")
xreg <- matrix(c(trn$M2, trn$S2, trn$N2, trn$K2, trn$K1, trn$O1, trn$SA, trn$P1), ncol = 8)
xreg_test <- matrix(c(tst$M2, tst$S2, tst$N2, tst$K2, tst$K1, tst$O1, tst$SA, tst$P1), ncol = 8)
colnames(xreg) <- col_names
colnames(xreg_test) <- col_names


xreg <- rbind(xreg, xreg_test)

# freq <- outer(1:nrow(xreg), 1:10)*2*pi/25
# 
# cs   <- cos(freq)
# colnames(cs) <- paste("cos", 1:10)
# si   <- sin(freq)
# colnames(si) <- paste("sin", 1:10)
# 
# more_reg <- as.matrix(cbind(cs,si))
# xreg <- cbind(xreg, more_reg)

#plot(zoo(xreg))
#(3,0,2)(1,0,0)[24]


acfpacf(trn$level)
mod2_ar <- Arima((trn$level/100)+0.001, xreg = xreg[1:4248,,drop=F],
                 include.drift = T,
                 c(3,0,2)
                 , list(order = c(1,0,0), period = 24))

summary(mod2_ar)
acfpacf(mod2_ar$residuals)
p1 <- ggplot.corr(mod2_ar$residuals, lag.max = 100, large.sample.size = T, horizontal = T)
p1
acfpacf(mod2_ar$residuals, max.lag = 72)
#ggplotly(autoplot(ts(mod2_ar$residuals)))
#ggplotly(autoplot(ts(trn$level)))
p2 <- autoplot(ts(trn$level/100), series = "real") +theme_bw()+labs(y="level")+ autolayer(mod2_ar$fitted, series = "fitted")
p2
#jpeg("../report/imgs/ar2_2_fit.jpg", width = 600, height = 150, quality = 100)
p1/p2
#dev.off()

Box.test(mod2_ar$residuals, type="Ljung-Box")
autoplot(mod2_ar)
#Mod(1/polyroot(c(1,-mod2_ar$coef[1:3])))

y <- rbind(trn, tst)
y <- y$level/100
#y <- c(y, rep(NA, 168)) # add NA's for data we cannot compare

# 1 step ahead predictions
score <- numeric(168)
for (i in 1:168){
  mod_prod <- Arima(y[i:(4247+i)], xreg = xreg[i:(4247+i),,drop=F], model = mod2_ar)
  pred <- forecast(mod_prod, xreg = xreg[(4248+i):(4248+i),,drop=FALSE], h = 1)$mean
  score[i] <- MAPE(pred, y[(4248 + i):(4248 + i)]+0.001)
}
pred_1step2 <- mean(score, na.rm = T)
pred_1step2

# 24 step ahead predictions
score <- numeric(168)
for (i in 1:168){
  mod_prod <- Arima(y[i:(4247+i)], xreg = xreg[i:(4247+i),,drop=F], model = mod2_ar)
  pred <- forecast(mod_prod, xreg = xreg[(4248+i):(4248+i+23),,drop=FALSE], h = 24)$mean
  score[i] <- MAPE(pred, y[(4248 + i):(4248 + i + 23)]+0.001)
}
pred_24step2 <- mean(score)
pred_24step2

# 168 steps ahead predictions
score <- numeric(168)
for (i in 1:168){
  mod_prod <- Arima(y[i:(4247+i)], xreg = xreg[i:(4247+i),,drop=F], model = mod2_ar)
  pred <- forecast(mod_prod, xreg = xreg[(4248+i):(4248+i+167),,drop=FALSE], h = 168)$mean
  score[i] <- MAPE(pred, y[(4248 + i):(4248 + i + 167)]+0.001)
}
pred_168step2 <- mean(score)
pred_168step2

pander::pandoc.table(matrix(c(pred_1step2, pred_24step2, pred_168step2), nrow = 1, ncol = 3,
                            dimnames = list(c("mod2_ar"), c("1-step", "24-steps", "168-steps")), byrow = T),
                     style = "simple", caption = "MAPE values")

pander::pandoc.table(matrix(c(pred_1step1, pred_24step1, pred_168step1,
  pred_1step2, pred_24step2, pred_168step2), nrow = 2, ncol = 3,
  dimnames = list(c("mod1_ar", "mod2_ar"), c("1-step", "24-steps", "168-steps")), byrow = T),
  style = "simple", caption = "MAPE values")

#ggplotly(autoplot(ts(mod2_ar$residuals)))

# p <- plot_ly(orientation = "h", width = 700, height = 350, as.data.frame(xreg_test), x = ~as.POSIXct(tst$datetime, tz = "UTC"), y = ~M2,
#              name = 'M2', type = 'scatter', mode = 'lines') %>%
#   add_trace(y = ~S2, name = 'S2', mode = 'lines') %>%
#   add_trace(y = ~N2, name = 'N2', mode = 'lines') %>%
#   add_trace(y = ~K2, name = "K2", mode = 'lines') %>%
#   add_trace(y = ~K1, name = "K1", mode = 'lines') %>%
#   add_trace(y = ~O1, name = "O1", mode = 'lines') %>%
#   add_trace(y = ~SA, name = "SA", mode = 'lines') %>%
#   add_trace(y = ~P1, name = "P1", mode = 'lines') %>%
#   layout(yaxis = list(title = "Amplitude"), xaxis = list(title = "Datetime"))
# p
# htmlwidgets::saveWidget(as_widget(p), "components.html", )

pred2_ar <- forecast(mod2_ar, h = 168, xreg = xreg[4249:(4249+167),])
# ggplotly(autoplot(pred2_ar))
# 
# plot(tst$level, type = "l")
# lines(as.numeric(pred2_ar$mean)*100, col = "red")
# lines(as.numeric(pred1_ar$mean), col = "blue")
# 
pander::pandoc.table(matrix(c(RMSE(pred2_ar$mean[1]*100, y[4249]*100),
         MSE(pred2_ar$mean[1]*100, y[4249]*100),
         MAE(pred2_ar$mean[1]*100, y[4249]*100),
         MAPE((pred2_ar$mean[1]*100), (y[4249]*100)+0.1),
         RMSE(mod2_ar$fitted*100, y[1:4248]*100),
         MSE(mod2_ar$fitted*100, y[1:4248]*100),
         MAE(mod2_ar$fitted*100, y[1:4248]*100),
         MAPE((mod2_ar$fitted*100)+0.01, (y[1:4248]*100)+0.1)),
       ncol = 2, nrow = 4, dimnames = list(c("RMSE", "MSE", "MAE", "MAPE"),
                                            c("Test", "Train"))),
        caption = "1 step ahead one shot prediction")
# 
pander::pandoc.table(matrix(c(RMSE(pred2_ar$mean[1:24]*100, y[4249:(4249+23)]*100),
                              MSE(pred2_ar$mean[1:24]*100, y[4249:(4249+23)]*100),
                              MAE(pred2_ar$mean[1:24]*100, y[4249:(4249+23)]*100),
                              MAPE((pred2_ar$mean[1:24]*100), (y[4249:(4249+23)]*100)+0.1),
                              RMSE(mod2_ar$fitted*100, y[1:4248]*100),
                              MSE(mod2_ar$fitted*100, y[1:4248]*100),
                              MAE(mod2_ar$fitted*100, y[1:4248]*100),
                              MAPE((mod2_ar$fitted*100), (y[1:4248]*100)+0.1)),
                            ncol = 2, nrow = 4, dimnames = list(c("RMSE", "MSE", "MAE", "MAPE"),
                                                                c("Test", "Train"))),
                     caption = "24 step ahead one shot prediction")

pander::pandoc.table(matrix(c(RMSE(pred2_ar$mean*100, y[4249:(4249+167)]*100),
                              MSE(pred2_ar$mean*100, y[4249:(4249+167)]*100),
                              MAE(pred2_ar$mean*100, y[4249:(4249+167)]*100),
                              MAPE((pred2_ar$mean*100), (y[4249:(4249+167)]*100)+0.1),
                              RMSE(mod2_ar$fitted*100, y[1:4248]*100),
                              MSE(mod2_ar$fitted*100, y[1:4248]*100),
                              MAE(mod2_ar$fitted*100, y[1:4248]*100),
                              MAPE((mod2_ar$fitted*100), (y[1:4248]*100)+0.1)),
                            ncol = 2, nrow = 4, dimnames = list(c("RMSE", "MSE", "MAE", "MAPE"),
                                                                c("Test", "Train"))),
                     caption = "168 step ahead one shot prediction")
# 
# plot(y[4249:(4249+167)], type = "l")
# lines(as.numeric(pred2_ar$mean), col = "red")
