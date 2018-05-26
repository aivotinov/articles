setwd("~/Dropbox/Статьи/2017/Мультипликатор государственных расходов/Data")

data_q <- read.csv("data_q_sa4.csv")[,-1]

data_q <- data_q[, c(2,21,27,28,29,30,31,32,19) ]

plot(data_q)

data_q[,2:8] <- data_q[,2:8]/10^9

write.csv(data_q, "dataFin_q.csv")

dat <- ts(data_q, start = 2003.75, freq = 4)

plot(dat)
plot(dat[,c(1,2,3)])

dat <- data.frame(dat)
dat <- dat[,c(1,2,3)]

for(i in 1:3){
  dat[2:54,i] <- diff(dat[,i])
}

dat <- dat[-1,]
plot(ts(dat, start = 2004, freq = 4))

library(corrgram)

corrgram(dat)
ccf(x=dat[,1], y = dat[,2], main = "gdp tax", lag.max = 12)
ccf(x=dat[,1], y = dat[,3], main = "gdp exp", lag.max = 12)
ccf(x=dat[,2], y = dat[,3], main = "tax exp", lag.max = 12)
