library(vars)
setwd("~/Dropbox/Статьи/2017/Мультипликатор государственных расходов/Data")
data_q <- read.csv("dataFin_q.csv")[,-1]




data_q <- data_q[c(1,2,3)]

names(data_q) <- c("ВВП", "Доходы", "Расходы")

data_q <- ts(data_q, start = 2003.75, freq = 4)
plot(data_q, main = "", xlab = "Год", lwd = 3)






data_q <- data_q[,c(3,5,6,7,8)]

data_q <- data_q/data_q[,1]*100

data_q <- data_q[,-1]
names(data_q) <- c("Оборона", "Эк-ка", "Образ", "Соц. сфера")

data_q <- ts(data_q, start = 2003.75, freq = 4)



plot(data_q, main = "", xlab = "Год", lwd = 3)


