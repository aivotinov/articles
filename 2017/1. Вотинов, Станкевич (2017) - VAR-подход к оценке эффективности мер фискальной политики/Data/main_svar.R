library(vars)
setwd("~/Dropbox/Статьи/2017/Мультипликатор государственных расходов/Data")

df <- data.frame()

for(i in 3:8){
  data_q <- read.csv("dataFin_q.csv")[,-1]
  dat <- data_q[,c(1,2,i,9)]
  names(dat) <- c("y", "t", "g", "oil")
  dat <- log(dat)
  dat <- ts(dat, start = 2003.75, freq = 4)
  plot(dat[,1:4])
  exog <- dat[,4]
  names(exog) <- names(dat)[,4]
  
  #VARselect(dat[,1:3], exogen = exog, lag.max = 4, type = "both")
  obj <- VAR(dat[,c(1:3)], exogen = exog, p = 3, type = "both")
  #summary(obj)
  s <- summary(obj)
  #s$roots
  #logLik(obj)
  #arch.test(obj)
  #normality.test(obj)
  #serial.test(obj)
  # plot(stability(obj))
  # plot(serial.test(obj))
  #obj <- restrict(obj, thresh = 1)
  #summary(obj)
  #summary(obj)$roots
  # взаимодействие между структурными шоками
  Bm <- matrix(0, 3, 3)
  diag(Bm) <- 1
  # G на T (если T на G, то нужно наоборот делать)
  Bm[3,c(2)] <- 0
  Bm[2,c(3)] <- NA
  # взаимодействие между наблюдаемыми = влияние структурных на наблюдаемые
  Am <- matrix(0, 3, 3)
  diag(Am) <- 1
  Am[c(2),c(1)] <- -1
  Am[c(3),c(1)] <- 0
  Am[1,c(2,3)] <- NA
  sobj <- SVAR(obj,  Amat = Am, Bmat = Bm, estmethod = "direct", max.iter = 10000)
  #sobj
  #round(diag(3)-sobj$A,3)
  resid <- data.frame(residuals(obj))
  cfit <- lm(y ~ t + g - 1, data = resid)
  #summary(cfit)
  sobj$A[1,2] <- -coef(cfit)[1]
  sobj$A[1,3] <- -coef(cfit)[2]
  cfit <- lm( I(t-y) ~  g - 1, data = resid)
  sobj$B[2,3] <- coef(cfit)
  cfit <- lm(y ~ t - 1, data = resid)
  #summary(cfit)
  sobj$A[1,2] <- -coef(cfit)[1]
  sobj$A[1,3] <- 0
  #sobj
  srf <- irf(sobj, impulse = "g", boot = T, ortho = FALSE, cumulative = T, n.ahead = 12,  ci = 0.9,
             runs = 10)
  #plot(srf)
  srfForGov <- srf$irf$g/100
  gInY <- mean((aggregate(exp(dat[,c(1,3)]), nfreq = 1)[,2]/aggregate(exp(dat[,c(1,3)]), nfreq = 1)[,1])[2:13])
  tInY <- mean((aggregate(exp(dat[,c(1,2)]), nfreq = 1)[,2]/aggregate(exp(dat[,c(1,3)]), nfreq = 1)[,1])[2:13])
  res <- (exp(dat[nrow(dat),1:3] + srfForGov[13,])-exp(dat[nrow(dat),1:3]) )
  
  curD <- data.frame(nam = names(data_q)[i], mult = round(res[1]/res[3],2))
  
  df <- rbind(df, curD)
  
  
}
View(df)








################
# V raznostyah #
################






library(vars)
setwd("~/Dropbox/Статьи/2017/Мультипликатор государственных расходов/Data")


df <- data.frame()

for(i in 3:8){
  data_q <- read.csv("dataFin_q.csv")[,-1]
  dat <- data_q[,c(1,2,i,9)]
  names(dat) <- c("y", "t", "g", "oil")
  dat <- (dat[2:54,]-dat[1:53,])/dat[1:53,]*100
  dat <- ts(dat, start = 2004, freq = 4)
  plot(dat[,1:4])
  exog <- dat[,4]
  names(exog) <- names(dat)[,4]
  
  VARselect(dat[,1:3], exogen = exog, lag.max = 4, type = "const")
  obj <- VAR(dat[,c(1:3)], exogen = exog, p = 1, type = "const")
  #summary(obj)
  s <- summary(obj)
  #s$roots
  #logLik(obj)
  #arch.test(obj)
  #normality.test(obj)
  #serial.test(obj)
  # plot(stability(obj))
  # plot(serial.test(obj))
  #obj <- restrict(obj, thresh = 1)
  #summary(obj)
  #summary(obj)$roots
  # взаимодействие между структурными шоками
  Bm <- matrix(0, 3, 3)
  diag(Bm) <- 1
  # G на T (если T на G, то нужно наоборот делать)
  Bm[3,c(2)] <- 0
  Bm[2,c(3)] <- NA
  # взаимодействие между наблюдаемыми = влияние структурных на наблюдаемые
  Am <- matrix(0, 3, 3)
  diag(Am) <- 1
  Am[c(2),c(1)] <- -1
  Am[c(3),c(1)] <- 0
  Am[1,c(2,3)] <- NA
  
  sobj <- SVAR(obj,  Amat = Am, Bmat = Bm, estmethod = "direct", max.iter = 10000)
  #sobj
  round(diag(3)-sobj$A,3)
  resid <- data.frame(residuals(obj))
  cfit <- lm(y ~ t + g - 1, data = resid)
  #summary(cfit)
  sobj$A[1,2] <- -coef(cfit)[1]
  sobj$A[1,3] <- -coef(cfit)[2]
  cfit <- lm( I(t-y) ~  g - 1, data = resid)
  sobj$B[2,3] <- coef(cfit)
  cfit <- lm(y ~ t - 1, data = resid)
  #summary(cfit)
  sobj$A[1,2] <- -coef(cfit)[1]
  sobj$A[1,3] <- 0
  #sobj
  
  srf <- irf(sobj, impulse = "g", boot = T, ortho = FALSE, cumulatjive = F, n.ahead = 12,  ci = 0.9,
             runs = 10)
  plot(srf)
  srfForGov <- srf$irf$g
  
  dat <- data_q[,c(1,2,i,9)]
  dat <- dat[nrow(dat),]
  names(dat) <- c("y", "t", "g", "oil")
  srfForGov <- (1+srfForGov/100)
  
  m <- c(1,1,1)
  for(j in 1:13){
    m <- m*srfForGov[j,]
  }
  
  dat[2,1:3] <- dat[1,1:3]*m
  dat <- dat[,1:3]
  
  curD <- data.frame(nam = names(data_q)[i], mult = round((dat[2,] - dat[1,])[1]/(dat[2,] - dat[1,])[3],2))
  
  df <- rbind(df, curD)
}

View(df)










############
# общие расходы и часть 
############


df <- data.frame()

for(i in 4:8){
  data_q <- read.csv("dataFin_q.csv")[,-1]
  dat <- data_q[,c(1,2,3,i,9)]
  dat[,3] <- dat[,3] - dat[,4]
  names(dat) <- c("y", "t", "g", "g2",  "oil")
  dat <- log(dat)
  dat <- ts(dat, start = 2003.75, freq = 4)
  plot(dat[,1:4])
  exog <- dat[,5]
  names(exog) <- names(dat)[,5]
  
  #VARselect(dat[,1:3], exogen = exog, lag.max = 4, type = "both")
  obj <- VAR(dat[,c(1:4)], exogen = exog, p = 3, type = "both")
  #summary(obj)
  s <- summary(obj)
  #s$roots
  #logLik(obj)
  #arch.test(obj)
  #normality.test(obj)
  #serial.test(obj)
  # plot(stability(obj))
  # plot(serial.test(obj))
  #obj <- restrict(obj, thresh = 1)
  #summary(obj)
  #summary(obj)$roots
  # взаимодействие между структурными шоками
  Bm <- matrix(0, 4, 4)
  diag(Bm) <- 1
  # G на T (если T на G, то нужно наоборот делать)
  Bm[3,c(2)] <- 0
  Bm[2,c(3)] <- NA
  # взаимодействие между наблюдаемыми = влияние структурных на наблюдаемые
  Am <- matrix(0, 4, 4)
  diag(Am) <- 1
  Am[c(2),c(1)] <- -1
  Am[c(3),c(1)] <- 0
  Am[1,c(2,3,4)] <- NA
  Am[3,4] <- NA
  sobj <- SVAR(obj,  Amat = Am, Bmat = Bm, estmethod = "direct", max.iter = 10000)
  #sobj
  #round(diag(3)-sobj$A,3)
  resid <- data.frame(residuals(obj))
  cfit <- lm(y ~ t + g + g2 - 1, data = resid)
  #summary(cfit)
  sobj$A[1,2] <- -coef(cfit)[1]
  sobj$A[1,3] <- -coef(cfit)[2]
  sobj$A[1,4] <- -coef(cfit)[3]
  cfit <- lm( I(t-y) ~  g - 1, data = resid)
  sobj$B[2,3] <- coef(cfit)
  cfit <- lm(g ~ g2- 1, data = resid)
  #summary(cfit)
  sobj$A[3,4] <- -coef(cfit)[1]
  
  #sobj
  srf <- irf(sobj, impulse = "g2", boot = T, ortho = FALSE, cumulative = T, n.ahead = 12,  ci = 0.9,
             runs = 10)
  #plot(srf)
  srfForGov <- srf$irf$g2/100
  
  res <- (exp(dat[nrow(dat),1:4] + srfForGov[13,])-exp(dat[nrow(dat),1:4]) )
  
  curD <- data.frame(nam = names(data_q)[i], mult = round(res[1]/res[4],2))
  
  df <- rbind(df, curD)
}

View(df)































































#############################
# Drugie kategorii rashodov #
#############################


library(vars)
setwd("~/Dropbox/Статьи/2017/Мультипликатор государственных расходов/Data")

data_q <- read.csv("dataFin_q.csv")[,-1]
dat <- data_q[,c(1,2,4,9)]
names(dat) <- c("y", "t", "g", "oil")
dat <- log(dat)
dat <- ts(dat, start = 2003.75, freq = 4)
plot(dat[,1:4])
exog <- dat[,4]
names(exog) <- names(dat)[,4]

VARselect(dat[,1:3], exogen = exog, lag.max = 4, type = "both")
obj <- VAR(dat[,c(1:3)], exogen = exog, p = 3, type = "both")
summary(obj)
s <- summary(obj)
s$roots
logLik(obj)
arch.test(obj)
normality.test(obj)
serial.test(obj)
# plot(stability(obj))
# plot(serial.test(obj))
#obj <- restrict(obj, thresh = 1)
summary(obj)
summary(obj)$roots
# взаимодействие между структурными шоками
Bm <- matrix(0, 3, 3)
diag(Bm) <- 1
# G на T (если T на G, то нужно наоборот делать)
Bm[3,c(2)] <- 0
Bm[2,c(3)] <- NA
# взаимодействие между наблюдаемыми = влияние структурных на наблюдаемые
Am <- matrix(0, 3, 3)
diag(Am) <- 1
Am[c(2),c(1)] <- -1
Am[c(3),c(1)] <- 0
Am[1,c(2,3)] <- NA
sobj <- SVAR(obj,  Amat = Am, Bmat = Bm, estmethod = "direct", max.iter = 10000)
sobj
round(diag(3)-sobj$A,3)
resid <- data.frame(residuals(obj))
cfit <- lm(y ~ t + g - 1, data = resid)
summary(cfit)
sobj$A[1,2] <- -coef(cfit)[1]
sobj$A[1,3] <- -coef(cfit)[2]
cfit <- lm( I(t-y) ~  g - 1, data = resid)
sobj$B[2,3] <- coef(cfit)

sobj
srf <- irf(sobj, impulse = "g", boot = T, ortho = FALSE, cumulative = T, n.ahead = 12,  ci = 0.9,
           runs = 10)
plot(srf)
srfForGov <- srf$irf$g/100
gInY <- mean((aggregate(exp(dat[,c(1,3)]), nfreq = 1)[,2]/aggregate(exp(dat[,c(1,3)]), nfreq = 1)[,1])[2:13])
tInY <- mean((aggregate(exp(dat[,c(1,2)]), nfreq = 1)[,2]/aggregate(exp(dat[,c(1,3)]), nfreq = 1)[,1])[2:13])
res <- (exp(dat[nrow(dat),1:3] + srfForGov[13,])-exp(dat[nrow(dat),1:3]) )
res[1]/res[3]







################
# V raznostyah #
################






library(vars)
setwd("~/Dropbox/Статьи/2017/Мультипликатор государственных расходов/Data")

data_q <- read.csv("dataFin_q.csv")[,-1]
dat <- data_q[,c(1,2,3,9)]
names(dat) <- c("y", "t", "g", "oil")
dat <- (dat[2:54,]-dat[1:53,])/dat[1:53,]*100
dat <- ts(dat, start = 2004, freq = 4)
plot(dat[,1:4])
exog <- dat[,4]
names(exog) <- names(dat)[,4]

VARselect(dat[,1:3], exogen = exog, lag.max = 4, type = "const")
obj <- VAR(dat[,c(1:3)], exogen = exog, p = 3, type = "const")
summary(obj)
s <- summary(obj)
s$roots
logLik(obj)
arch.test(obj)
normality.test(obj)
serial.test(obj)
# plot(stability(obj))
# plot(serial.test(obj))
#obj <- restrict(obj, thresh = 1)
summary(obj)
summary(obj)$roots
# взаимодействие между структурными шоками
Bm <- matrix(0, 3, 3)
diag(Bm) <- 1
# G на T (если T на G, то нужно наоборот делать)
Bm[3,c(2)] <- 0
Bm[2,c(3)] <- NA
# взаимодействие между наблюдаемыми = влияние структурных на наблюдаемые
Am <- matrix(0, 3, 3)
diag(Am) <- 1
Am[c(2),c(1)] <- -1
Am[c(3),c(1)] <- 0
Am[1,c(2,3)] <- NA

sobj <- SVAR(obj,  Amat = Am, Bmat = Bm, estmethod = "direct", max.iter = 10000)
sobj
round(diag(3)-sobj$A,3)
resid <- data.frame(residuals(obj))
cfit <- lm(y ~ t + g - 1, data = resid)
summary(cfit)
sobj$A[1,2] <- -coef(cfit)[1]
sobj$A[1,3] <- -coef(cfit)[2]
cfit <- lm( I(t-y) ~  g - 1, data = resid)
sobj$B[2,3] <- coef(cfit)
cfit <- lm(y ~ t - 1, data = resid)
summary(cfit)
sobj$A[1,2] <- -coef(cfit)[1]
sobj$A[1,3] <- 0
sobj

srf <- irf(sobj, impulse = "g", boot = T, ortho = FALSE, cumulative = T, n.ahead = 12,  ci = 0.9,
           runs = 10)
plot(srf)
srfForGov <- srf$irf$g/100

gInY <- mean((aggregate(exp(dat[,c(1,3)]), nfreq = 1)[,2]/aggregate(exp(dat[,c(1,3)]), nfreq = 1)[,1])[2:13])
tInY <- mean((aggregate(exp(dat[,c(1,2)]), nfreq = 1)[,2]/aggregate(exp(dat[,c(1,3)]), nfreq = 1)[,1])[2:13])

srfForGov[13,1]/(srfForGov[13,3]*0.3)
dat <- data_q[,c(1,2,3,9)]
names(dat) <- c("y", "t", "g", "oil")
dat <- log(dat)
res <- (exp(dat[nrow(dat),1:3] + srfForGov[13,])-exp(dat[nrow(dat),1:3]) )
res[1]/res[3]

