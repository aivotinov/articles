library(seasonal)

data_q <- read.csv("data_q.csv", sep = ";")
data_q_sa <- data_q

#data_q[1:5, 12] <- NA

for (d in 2:ncol(data_q)){
  temp <- data_q[,d]
  temp <- ts(temp, start = c(2003, 4), frequency = 4)
  s <- seas(temp, na.action = na.exclude)
  data_q_sa[,d] <- final(s)
}

write.csv(data_q_sa, "data_q_sa.csv")


data_m <- read.csv("data_m.csv", sep = ";")
data_m_sa <- data_m

#data_m[1:16, 28] <- NA

for (d in 2:ncol(data_m)){
  temp <- data_m[,d]
  temp <- ts(temp, start = c(2003, 9), frequency = 12)
  s <- seas(temp, na.action = na.exclude)
  data_m_sa[,d] <- final(s)
}

write.csv(data_m_sa, "data_m_sa.csv")


temp <- temp + 100000000000000
s <- seas(temp, na.action = na.exclude, transform.function = "log")
temp <- final(s) - 100000000000000
data_m_sa[,42] <- temp
