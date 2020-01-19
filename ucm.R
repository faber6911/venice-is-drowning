# UCM ---------------------------------------------------------------------

y <- data$level
y[(nrow(data)-167):nrow(data)] <- NA
reg_norm <- as.data.frame(matrix(c(as.vector(scale(data$rain)),
                                   as.vector(scale(data$dir_wind)),
                                   as.vector(scale(data$vel_wind)),
                                   as.vector(scale(data$l_motion))), ncol=4))


rm(list = c("mod1", "smo1", "fit1"))
#SSMregression()
mod1 <- SSModel(y ~ 0 +
                  #SSMregression(~V1+V2+V3+V4, Q = diag(NA, 4), data = reg_norm)+
                  SSMtrend(1, NA)+
                  SSMseasonal(12, NA, "trig")+
                  SSMseasonal(24, NA, "trig"),
                H = NA)
mod1$Q
mod1$T
mod1$Z
mod1$P1inf <- mod1$P1inf * 0
diag(mod1$P1) <- log(vary)
mod1$a1[1] <- mean(y, na.rm = T)
mod1$a1
mod1$P1

vary <- var(y, na.rm = TRUE)
pars <- numeric(4)
vary
pars[1] <- log(vary/100)
pars[2] <- log(vary/100)
pars[3] <- log(vary/100)
pars[4] <- log(vary/100)
#pars[5] <- log(0.01)
exp(pars)
dim(mod1$Q)

updt <- function(pars, model) {
  model$Q[1, 1, 1] <- exp(pars[1])
  #model$Q[5, 5, 1] <- exp(pars[1])
  #model$Q[2, 2, 1] <- exp(pars[2])
  diag(model$Q[2:12, 2:12, 1]) <- exp(pars[2])
  diag(model$Q[13:35, 13:35, 1]) <- exp(pars[3])
  model$H[1, 1, 1] <- exp(pars[4])
  model
}

fit1 <- fitSSM(mod1, pars, updt)
fit1$optim.out$convergence

smo1 <- KFS(fit1$model,
            smoothing = c(#"state",
              #  "disturbance",
              "signal"))

fit1$model$Q
plot(data$level[(nrow(data)-167):nrow(data)], type = "l")
lines(smo1$muhat[(nrow(data)-167):nrow(data)], col="red")

plot(data$level, type = "l")
lines(smo1$muhat, col="red")

cor(data$level[1:(nrow(data)-167)], smo1$muhat[1:(nrow(data)-167)])^2 *100
cor(data$level[(nrow(data)-167):nrow(data)], smo1$muhat[(nrow(data)-167):nrow(data)])^2 *100

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
