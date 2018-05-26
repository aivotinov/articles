setwd("~/Dropbox/Статьи/2017/Мультипликатор государственных расходов/Data")

library(openxlsx)
library(vars)
library(seasonal)

dat <- read.xlsx("Data.xlsx")

dat <- ts(dat[,2:7], start = 2003.75, freq = 4)

dat[,1:4] <- log(dat[,1:4])

dat[2:nrow(dat),1] <- diff(dat[,1])*100
dat[2:nrow(dat),2] <- diff(dat[,2])*100
dat[2:nrow(dat),3] <- diff(dat[,3])*100
dat[2:nrow(dat),4] <- diff(dat[,4])*100


dat <- dat[-c(1),]

dat <- ts(dat, start = 2004, freq = 4)

plot(dat)

for(i in 1:3){
  ff <- seas(dat[,i], x11 = "")
  #plot(series(ff, "b11"))
  #dat[,i] <- series(ff, "b11")
  dat[,i] <- final(ff)
  rm(ff)
}

rm(i)

plot(dat)



VARselect(dat[,1:3], exogen = dat[,4:6], lag.max = 4, type = "const")
obj <- VAR(dat[,c(1:3)], exogen = dat[,4:6], p = 4, type = "const")
summary(obj)
s <- summary(obj)
s$roots
logLik(obj)
arch.test(obj)
normality.test(obj)
serial.test(obj)
# plot(stability(obj))
# plot(serial.test(obj))
# obj <- restrict(obj, thresh = 0.75)
summary(obj)
summary(obj)$roots



Bm <- matrix(0, 3, 3)
diag(Bm) <- 1

# G на T (если T на G, то нужно наоборот делать)
Bm[1,c(2)] <- NA
Bm[2,c(1)] <- 0 

Am <- matrix(0, 3, 3)
diag(Am) <- 1
Am[1,c(3)] <- -1
Am[2,c(3)] <- NA
Am[3,c(1,2)] <- NA






sobj <- SVAR(obj,  Amat = Am, Bmat = Bm, estmethod = "direct", max.iter = 10000)
sobj
round(diag(3)-sobj$A,3)

srf <- irf(sobj, impulse = "G", boot = T, ortho = FALSE, cumulative = T, n.ahead = 12,  ci = 0.9,
           runs = 10)
plot(srf)
srf$irf$G[,3]*3.4
