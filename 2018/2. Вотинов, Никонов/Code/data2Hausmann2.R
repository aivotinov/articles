setwd("C:/Users/avotinov/Dropbox/Статьи/2018/Статья - Armenia Complexity/Расчет")
library(reshape2)
dir()

dat <- read.table(file = 'hs02.tsv', sep = '\t', header = TRUE)

dat <- dat[dat$year < 2008, ]

dat <- dat[,-c(5,7)]

dat <- dat[complete.cases(dat),]
dat$export_val <- as.numeric(as.character(dat$export_val))
dat$export_rca <- as.numeric(as.character(dat$export_rca))
dat[is.na(dat)] <- 0

countries <- read.csv("countries.csv")[,3]
countries <- countries[-c(9,12,60,69,74)]


dat <- dat[dat$origin %in% countries, ]

names(dat)[2] <- "country"

for(year in 2007:2003){
  df <- data.frame()
  cdat <- dat[dat$year == year, ]
  
  
  
  df4 <- cdat
  rm(df)
  
  df4Rca <- df4[,c(2,3,5)]
  df4Val <- df4[,c(2,3,4)]
  
  print("stage 1 Done")
  
  rm(df4,df6)
  
  df4Rca <- dcast(data = df4Rca,  country ~ hs02, value.var = "export_rca")
  df4Val <- dcast(data = df4Val,  country ~ hs02, value.var = "export_val")

  
  df4Rca[is.na(df4Rca)] <- 0
  df4Val[is.na(df4Val)] <- 0

  
  print("stage 2 Done, writing")
  
  write.csv(df4Rca, paste("datFinal4/", year, "Rca.csv", sep = ""  ))
  write.csv(df4Val, paste("datFinal4/", year, "Val.csv", sep = ""  ))
  rm(df4Val, df4Rca, df6Val, df6Rca)
  gc()
}

