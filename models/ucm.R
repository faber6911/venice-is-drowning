
# packages and functions --------------------------------------------------

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


# import data -------------------------------------------------------------

data <- read.csv("../data/output/df_final2010-2018.csv", header = T)
lunar_motion <- read.csv("../moon_distance/moon_distances.csv", header = T)
data$l_motion <- 1/lunar_motion$dists.Km.^2
datsl <- as.sealevel(data$level/100)
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

data <- data[74305:nrow(data),]

# sinu<-TRUE
# if (sinu){
#   xreg <- matrix(c(data$M2, data$S2, data$N2, data$K2, data$K1, data$O1, data$SA, data$P1), ncol = 8)
# 
#   col_names <- c("M2", "S2", "N2","K2", "K1", "O1", "SA", "P1")
#   colnames(xreg) <- col_names
#   xreg <- as.data.frame(xreg)
# }else{
#   xreg <- matrix(c(as.vector(scale(data$rain)), as.vector(scale(data$dir_wind)), as.vector(scale(data$vel_wind))
#                    , as.vector(scale(data$l_motion))), ncol = 4)
#   
#   col_names <- c("rain", "dir_wind","vel_wind","l_motion")
#   colnames(xreg) <- col_names
#   xreg <- as.data.frame(xreg)
# }
# 
# xreg
#xreg[(nrow(xreg)-167):nrow(xreg),] <- NA

# UCM ---------------------------------------------------------------------

mod1 <- SSModel(level/100 ~ 0 +
                  SSMtrend(1, NA)+
                  M2+
                  S2+
                  N2+
                  K2+
                  K1+
                  O1+
                  SA+
                  P1,
                H = NA,
                data = data[1:(nrow(data)-336),])
mod1$Q
mod1$T
mod1$Z

mod1$P1inf <- mod1$P1inf * 0
vary <- var(data$level[1:(nrow(data)-336)], na.rm = TRUE)
diag(mod1$P1) <- log(vary)
mod1$a1[9] <- mean(data$level[1:(nrow(data)-336)], na.rm = T)
mod1$a1
mod1$P1

pars <- numeric(5)
vary/100
pars[1] <- log(vary/100)
pars[2] <- log(vary/100)
pars[3] <- log(vary/100)
pars[4] <- log(vary/100)
pars[5] <- log(0.01)
exp(pars)
dim(mod1$Q)

updt <- function(pars, model) {
  model$Q[1, 1, 1] <- exp(pars[1])
  model$H[1, 1, 1] <- exp(pars[4])
  model
}
# }else{
#   mod1 <- SSModel(y/100 ~ 0 +
#                     #SSMregression(~V1+V2+V3+V4, Q = diag(NA, 4), data = reg_norm)+
#                     SSMtrend(1, NA)+
#                     SSMregression(~rain+dir_wind+vel_wind+l_motion, Q = diag(NA, 4), data = xreg)+
#                     SSMseasonal(12, NA, "trig")+
#                     SSMseasonal(24, NA, "dummy"),
#                   H = NA)
#   mod1$Q
#   mod1$T
#   mod1$Z
#   mod1$P1inf <- mod1$P1inf * 0
#   vary <- var(y, na.rm = TRUE)
#   diag(mod1$P1) <- log(vary)
#   mod1$a1[1] <- mean(y, na.rm = T)
#   mod1$a1
#   mod1$P1
#   
#   pars <- numeric(5)
#   vary
#   pars[1] <- log(vary/100)
#   pars[2] <- log(vary/100)
#   pars[3] <- log(vary/100)
#   pars[4] <- log(vary/100)
#   pars[5] <- log(0.01)
#   exp(pars)
#   dim(mod1$Q)
#   
#   updt <- function(pars, model) {
#     model$Q[1, 1, 1] <- exp(pars[1])
#     #model$Q[5, 5, 1] <- exp(pars[1])
#     #model$Q[2, 2, 1] <- exp(pars[2])
#     diag(model$Q[2:5, 2:5, 1]) <- exp(pars[5])
#     diag(model$Q[6:16, 6:16, 1]) <- exp(pars[2])
#     #model$Q[6, 6, 1] <- exp(pars[2])
#     #diag(model$Q[17:39, 17:39, 1]) <- exp(pars[3])
#     model$Q[17, 17, 1] <- exp(pars[3])
#     model$H[1, 1, 1] <- exp(pars[4])
#     model
#   }
# }

fit1 <- fitSSM(mod1, pars, updt, control = list(maxit = 1000))
fit1$optim.out$convergence


# 1 step ahead
score <- numeric(168)
for (i in 1:168){
  lev <- c(data$level[4248:(4247+i)]/100, rep(NA, 1))
  lev
  temp_mod <- SSModel(lev ~ 0 +
                        SSMtrend(1, fit1$model$Q[1,1,1])+
                        M2+
                        S2+
                        N2+
                        K2+
                        K1+
                        O1+
                        SA+
                        P1,
                      H = fit1$model$H,
                      data = data[(4247+1):(4247+i+1),])
  
  ucm_pred <- predict(fit1$model, newdata = temp_mod, n.ahead = 1)[(i+1):(i+1)]
  
  score[i] <- MAPE(ucm_pred, ((data$level[4248+i]/100)+0.001))
}
step1_ucm1 <- mean(score)
step1_ucm1

# 24 steps ahead
score <- numeric(168)
for (i in 1:168){
  lev <- c(as.numeric(data$level[4248:(4247+i)]/100), rep(NA, 24))
  temp_mod <- SSModel(lev ~ 0 +
                        SSMtrend(1, fit1$model$Q[1,1,1])+
                        M2+
                        S2+
                        N2+
                        K2+
                        K1+
                        O1+
                        SA+
                        P1,
                      H = fit1$model$H,
                      data = data[(4247+1):(4247+i+24),])
  
  ucm_pred <- predict(fit1$model, newdata = temp_mod, n.ahead = 24)[(i+1):(i+24)]
  
  score[i] <- MAPE(ucm_pred, (data$level[(4248+i):(4248+i+23)]/100)+0.001)
}
step24_ucm1 <- mean(score)
step24_ucm1


# 168 steps ahead
score <- numeric(168)
for (i in 1:168){
  lev <- c(as.numeric(data$level[4248:(4247+i)]/100), rep(NA, 168))
  temp_mod <- SSModel(lev ~ 0 +
                        SSMtrend(1, fit1$model$Q[1,1,1])+
                        M2+
                        S2+
                        N2+
                        K2+
                        K1+
                        O1+
                        SA+
                        P1,
                      H = fit1$model$H,
                      data = data[(4247+1):(4247+i+168),])

  ucm_pred <- predict(fit1$model, newdata = temp_mod, n.ahead = 168)[(i+1):(i+168)]
  
  score[i] <- MAPE(ucm_pred, (data$level[(4248+i):(4248+i+167)]/100)+0.001)
}
step168_ucm1 <- mean(score)
step168_ucm1

pander::pandoc.table(matrix(c(step1_ucm1, step24_ucm1, step168_ucm1), nrow = 1, ncol = 3,
                            dimnames = list(c("mod_ucm1"), c("1-step", "24-steps", "168-steps"))), style = "simple")


# one shot predictions

lev <- c(rep(NA, 336))
temp_mod <- SSModel(lev ~ 0 +
                      SSMtrend(1, fit1$model$Q[1,1,1])+
                      M2+
                      S2+
                      N2+
                      K2+
                      K1+
                      O1+
                      SA+
                      P1,
                    H = fit1$model$H,
                    data = data[(4248+1):(4248+336),])


y_true <- data$level[4249:(4249+335)]
y_pred <- as.numeric(predict(fit1$model, newdata = temp_mod, n.ahead = 336))*100
plot(y_true, type = "l")
lines(y_pred, col = "red")

# 1 step
MAPE(y_pred[1], y_true[1]+0.1)
# 24 steps
MAPE(y_pred[1:23], y_true[1:23]+0.1)
# 168 steps
MAPE(y_pred[1:length(y_pred)], y_true[1:length(y_true)]+0.1)



#############

y <- data$level[1:(nrow(data)-168)]
y[4249:length(y)] <- NA

mod1 <- SSModel(y/100 ~ 0 +
                  SSMtrend(1, NA)+
                  M2+
                  S2+
                  N2+
                  K2+
                  K1+
                  O1+
                  SA+
                  P1,
                H = NA,
                data = data[1:(nrow(data)-168),])
mod1$Q
mod1$T
mod1$Z
mod1$P1inf <- mod1$P1inf * 0
vary <- var(data$level[1:(nrow(data)-168)], na.rm = TRUE)
diag(mod1$P1) <- log(vary)
mod1$a1[1] <- mean(data$level[1:(nrow(data)-168)], na.rm = T)
mod1$a1
mod1$P1

pars <- numeric(5)
vary
pars[1] <- log(vary/100)
pars[2] <- log(vary/100)
pars[3] <- log(vary/100)
pars[4] <- log(vary/100)
pars[5] <- log(0.01)
exp(pars)
dim(mod1$Q)

updt <- function(pars, model) {
  model$Q[1, 1, 1] <- exp(pars[1])
  model$H[1, 1, 1] <- exp(pars[4])
  model
}
fit1 <- fitSSM(mod1, pars, updt, control = list(maxit = 1000))
fit1$optim.out$convergence

smo1 <- KFS(fit1$model, smoothing = c("signal"))

MAPE(smo1$muhat[4249]*100, data$level[4249]+0.1)
MAPE(smo1$muhat[4249:(4249+23)]*100, data$level[4249:(4249+23)]+0.1)
MAPE(smo1$muhat[4249:(4249+167)]*100, data$level[4249:(4249+167)]+0.1)


smo1 <- KFS(fit1$model, smoothing = c("signal", "disturbance"))
 
autoplot(ts(data$level[1:4248]), series = "true") + theme_bw() + labs(y = "level") + autolayer(smo1$muhat*100, series = "pred")


##############

data2 <- data[1:(nrow(data)-336),]
mod2 <- SSModel(level/100 ~ 0+M2+
                  S2+
                  N2+
                  K2+
                  K1+
                  O1+
                  SA+
                  P1+
                  SSMtrend(1, NA),
                #SSMregression(~M2+S2+N2+K2+K1+O1+SA+P1, Q = diag(NA, 8), data = xreg),
                #SSMseasonal(12, NA, "dummy")+
                #SSMseasonal(24, NA, "dummy"),
                H = NA,
                data = data2)
mod2$Q
mod2$T
mod2$Z
mod2$P1inf <- mod2$P1inf * 0
vary <- var(data$level, na.rm = TRUE)
diag(mod1$P1) <- log(vary)
mod2$a1
mod2$a1[9] <- mean(data$level, na.rm = T)
mod2$a1
mod2$P1

pars <- numeric(5)
vary
pars[1] <- log(vary/100)
pars[2] <- log(vary/100)
pars[3] <- log(vary/100)
pars[4] <- log(vary/100)
pars[5] <- log(0.01)
exp(pars)
dim(mod2$Q)

updt <- function(pars, model) {
  model$Q[1, 1, 1] <- exp(pars[1])
  #model$Q[5, 5, 1] <- exp(pars[1])
  #model$Q[2, 2, 1] <- exp(pars[2])
  #diag(model$Q[2:9, 2:9, 1]) <- exp(pars[5])
  #diag(model$Q[10:20, 10:20, 1]) <- exp(pars[2])
  #model$Q[10, 10, 1] <- exp(pars[2])
  #diag(model$Q[11:33, 11:33, 1]) <- exp(pars[3])
  #model$Q[11, 11, 1] <- exp(pars[3])
  model$H[1, 1, 1] <- exp(pars[4])
  model
}

fit2 <- fitSSM(mod2, pars, updt, control = list(maxit = 1000))
fit2$optim.out$convergence


temp_mod2 <- SSModel(plh ~ 0+
                  M2+
                  S2+
                  N2+
                  K2+
                  K1+
                  O1+
                  SA+
                  P1+
                  SSMtrend(1, fit2$model$Q[1,1,1]),
                H = fit2$model$H,
                data = data[(nrow(data)-335):nrow(data),])


predict(fit2$model, newdata = temp_mod2, n.ahead = 336)



smo2 <- KFS(fit2$model,
                        smoothing = c(#"state",
                          #  "disturbance",
                          "signal"))
plot(y_true/100, type = "l")
lines(smo2$muhat[(nrow(data)-335:nrow(data))], col = "red")

length(data$level)-335
length(smo2$muhat)
# smo1 <- KFS(fit1$model,
#             smoothing = c(#"state",
#               #  "disturbance",
#               "signal"))
# 
# #fit1$model$Q
# plot(data$level[(nrow(data)-167):nrow(data)], type = "l")
# lines(smo1$muhat[(nrow(data)-167):nrow(data)]*100, col="red")
# 
# plot(data$level, type = "l")
# lines(smo1$muhat*100, col="red")
# 
# pred_train <- smo1$muhat[1:(nrow(data)-168)]*100
# pred <- smo1$muhat[(nrow(data)-167):nrow(data)]*100
# y_true <- data$level[(nrow(data)-167):nrow(data)]
# y_true_train <- data$level[1:(nrow(data)-168)]

# evaluate model ----------------------------------------------------------

pander::pandoc.table(matrix(c(RMSE(pred[1], y_true[1]),
                              MSE(pred[1], y_true[1]),
                              MAE(pred[1], y_true[1]),
                              MAPE((pred[1])+0.01, y_true[1]+0.01),
                              RMSE(pred_train, y_true_train),
                              MSE(pred_train, y_true_train),
                              MAE(pred_train, y_true_train),
                              MAPE(pred_train+0.01, y_true_train+0.01)),
                            ncol = 2, nrow = 4, dimnames = list(c("RMSE", "MSE", "MAE", "MAPE"),
                                                                c("Test", "Train"))),
                     caption = "1 step ahead prediction")



pander::pandoc.table(matrix(c(RMSE(pred[1:24], y_true[1:24]),
                              MSE(pred[1:24], y_true[1:24]),
                              MAE(pred[1:24], y_true[1:24]),
                              MAPE((pred[1:24])+0.01, y_true[1:24]+0.01),
                              RMSE(pred_train, y_true_train),
                              MSE(pred_train, y_true_train),
                              MAE(pred_train, y_true_train),
                              MAPE(pred_train+0.01, y_true_train+0.01)),
                            ncol = 2, nrow = 4, dimnames = list(c("RMSE", "MSE", "MAE", "MAPE"),
                                                                c("Test", "Train"))),
                     caption = "24 steps ahead prediction")



pander::pandoc.table(matrix(c(RMSE(pred, y_true),
                              MSE(pred, y_true),
                              MAE(pred, y_true),
                              MAPE((pred)+0.01, y_true+0.01),
                              RMSE(pred_train, y_true_train),
                              MSE(pred_train, y_true_train),
                              MAE(pred_train, y_true_train),
                              MAPE(pred_train+0.01, y_true_train+0.01)),
                            ncol = 2, nrow = 4, dimnames = list(c("RMSE", "MSE", "MAE", "MAPE"),
                                                                c("Test", "Train"))),
                     caption = "168 steps ahead prediction")


#best trend+xreg+seas_trig12+seas_dummy24
#new best trend+xreg

# deprecated --------------------------------------------------------------


vary <- var(y, na.rm = T)
mod1$P1inf <- mod1$P1inf * 0
mod1$a1[1] <- mean(y, na.rm = T)
diag(mod1$P1) <- vary

# Initial values for the variances we have to estimate
init <- numeric(3)
init[1] <- log(vary/10) # log-var(dist.rw)
init[2] <- log(vary/100)# log-var(dist.seas)
init[3] <- log(vary/10) # log-var(err.oss.)

# Estimate
fit1 <- fitSSM(mod1, init)
fit1$optim.out$convergence

# Smoothing
smo1 <- KFS(fit1$model, smoothing = c("state", "signal"))
smo1$alphahat
smo1_seas <- rowSums(smo1$alphahat[((nrow(data)-167):nrow(data)), seq(3, 11, 2)])
plot(data$level[(nrow(data)-167):nrow(data)], type = "l")
plot(smo1$muhat[(nrow(data)-167):nrow(data)],col="red", type="l")


lines(smo1$alphahat[((nrow(data)-167):nrow(data)), "level"], col = "red")
lines(smo1$alphahat[((nrow(data)-167):nrow(data)),"sea_dummy1"], col = "blue")
lines(smo1$alphahat[((nrow(data)-167):nrow(data)), "level"]+
        smo1$alphahat[((nrow(data)-167):nrow(data)),"sea_dummy1"], col="green")
lines(smo1$alphahat[((nrow(data)-167):nrow(data)), "level"]+
        smo1$alphahat[((nrow(data)-167):nrow(data)),"sea_dummy1"]+
        smo1_seas, col="yellow")


lines(smo1$alphahat[,"level"])
nrow(smo1$alphahat)
lines(smo1$alphahat[1:365, "level"] +
        smo1_seas, col = "blue")
lines(smo1$alphahat[1:365, "level"] +
        smo1$alphahat[1:365, "sea_dummy1"] +
        smo1_seas[1:365], col = "green")

smo1_seas[1:24]
