setwd("~/Desktop/Data")
library(foreign)
library(data.table)

n <- length(dir("135"))
n
k <- dir("135")




df <- data.frame()
t <- Sys.time()

for(i in 1:24){
  cur <- read.dbf(paste("135",k[i],dir(paste("135",k[i],sep = "/"))[3],sep="/"))
  cur[,2] <- gsub("\x8d","H",cur[,2])
  cur <- cur[grep("H", cur[,2]),]
  cur <- data.table(cbind(Date = k[i]),cur)
  df <- rbind(df,cur)
}

Sys.time() - t

write.csv(df,"135.csv")
