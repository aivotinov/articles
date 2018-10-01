setwd("C:/Users/avotinov/Dropbox/Статьи/2018/Статья - Armenia Complexity/Расчет")
setwd("~/Dropbox/Статьи/2018/Статья - Armenia Complexity/Расчет")
library(Matrix)
library(lattice)


dat2 <- read.csv("datFinal4/2014Rca.csv")[,-1]
rownames(dat2) <- dat2[,1]
dat2 <- dat2[,-1]


dat2 <- as.matrix(dat2)
dat2 <- (dat2 > 1)



kp0 <- as.matrix(colSums(dat2))
kc0 <- as.matrix(rowSums(dat2))

kp <- kp0
kc <- kc0

dim(dat2)
dim(kc)
dim(kp)


matC <-  (dat2 %*% (diag(as.numeric(kp0)^(-1)) ) %*%t(dat2)) 
for(i in 1:dim(dat2)[1]){
  matC[i,] <- matC[i,]/kc0[i]
}

eig <- eigen(matC)
resc <- eig$vectors[,2]
names(resc) <- rownames(dat2)
rescc <- (resc - mean(resc))/sd(resc)

ECI <- sort(-rescc)

# 
# matP <- matrix(0, dim(dat2)[2], dim(dat2)[2])
# for(p in 1:dim(dat2)[2]){
#   for(pp in 1:dim(dat2)[2]){
#     print(paste(p,pp))
#     s <- 0
#     for(c in 1:dim(dat2)[1]){
#       s <- s+ dat2[c,p]*dat2[c,pp]/(kc0[c]*kp0[p])
#     }
#     matP[p,pp] <- s
#   }
# }


matP <-  (t(dat2) %*% (diag(as.numeric(kc0)^(-1)) ) %*% (dat2)) 
for(i in 1:dim(dat2)[2]){
  matP[,i] <- matP[,i]/kp0[i]
}
eig <- eigen(matP)
resp <- eig$vectors[,2]
resp <- (resp - mean(resp))/sd(resp)
names(resp) <- colnames(dat2)
PCI <- (resp)


prox <- matrix(0,dim(dat2)[2],dim(dat2)[2])
for(j in 1:dim(dat2)[2]){
  print( paste(round(j/dim(dat2)[2]*100), "prox" ))
  for(i in 1:dim(dat2)[2]){
    prox[j,i] <- sum(dat2[,j] * dat2[,i])/kp0[i]
  }
}
colnames(prox) <- colnames(dat2)
rownames(prox) <- colnames(dat2)




dcp <- matrix(0,dim(dat2)[1],dim(dat2)[2])
for(j in 1:dim(dat2)[1]){
  print( paste(round(j/dim(dat2)[1]*100), "dcp" ))
  for(i in 1:dim(dat2)[2]){
    dcp[j,i] <- 1-sum((dat2[j,])*prox[i,])/sum(prox[i,])
  }
}
rownames(dcp) <- rownames(dat2)
colnames(dcp) <- colnames(dat2)

#levelplot(dcp)


oppValCP <- matrix(0,dim(dat2)[1],1)
for(j in 1:dim(dat2)[1]){
  print(round(j/dim(dat2)[1]*100))
  oppValCP[j] <- sum(   Re((1-dcp[j,])*(1-dat2[j,])*PCI  ) )
}
rownames(oppValCP) <- rownames(dat2)

plot(rescc,(oppValCP) )













dat3 <- dat2
for(i in 1:nrow(dat2)){
  dat3[i,] <- dat3[i,] * PCI
}

rowMeans(dat3)

datFrame <- data.frame(oppValCP, rownames(oppValCP))

datFrame <- datFrame[order(oppValCP),]






################
# ECI dynamics #
################

for(year in 2015:2015){
  nameOfFile <- paste("datFinal4/",year,"Rca.csv", sep = "")
  
  dat2 <- read.csv(nameOfFile)[,-1]
  rownames(dat2) <- dat2[,1]
  dat2 <- dat2[,-1]
  
  dat2 <- dat2[!(rownames(dat2) %in% c("irq", "tcd", "mac")),]
  
  dat2 <- as.matrix(dat2)
  dat2 <- (dat2 > 1)
  
  kp0 <- as.matrix(colSums(dat2))
  kc0 <- as.matrix(rowSums(dat2))
  
  dat2 <- dat2[,!(kp0 == 0)]
  kp0 <- as.matrix(colSums(dat2))
  kc0 <- as.matrix(rowSums(dat2))
  
  matC <-  (dat2 %*% solve(diag(as.numeric(kp0)) ) %*%t(dat2)) 
  for(i in 1:dim(dat2)[1]){
    matC[i,] <- matC[i,]/kc0[i]
  }
  
  eig <- eigen(matC)
  resc <- eig$vectors[,2]
  names(resc) <- rownames(dat2)
  rescc <- (resc - mean(resc))/sd(resc)
  rescc <- rescc*sign(rescc["jpn"])
  ECI <- sort(rescc)
  
  write.xlsx(data.frame(country = names(ECI),
                        ECI = ECI, 
                        rang = length(ECI):1),paste("resultECI/",year,".xlsx"))
}





df <- data.frame()
for(year in 2003:2015){
  print(year)
  nameOfFile <- paste("datFinal4/",year,"Rca.csv", sep = "")
  
  dat2 <- read.csv(nameOfFile)[,-1]
  rownames(dat2) <- dat2[,1]
  dat2 <- dat2[,-1]
  
  dat2 <- dat2[!(rownames(dat2) %in% c("irq", "tcd", "mac")),]
  
  dat2 <- as.matrix(dat2)
  dat2 <- (dat2 > 1)
  
  kp0 <- as.matrix(colSums(dat2))
  kc0 <- as.matrix(rowSums(dat2))
  
  dat2 <- dat2[,!(kp0 == 0)]
  kp0 <- as.matrix(colSums(dat2))
  kc0 <- as.matrix(rowSums(dat2))
  
  matC <-  (dat2 %*% solve(diag(as.numeric(kp0)) ) %*%t(dat2)) 
  for(i in 1:dim(dat2)[1]){
    matC[i,] <- matC[i,]/kc0[i]
  }
  
  eig <- eigen(matC)
  resc <- eig$vectors[,2]
  names(resc) <- rownames(dat2)
  rescc <- (resc - mean(resc))/sd(resc)
  rescc <- rescc*sign(rescc["jpn"])
  ECI <- sort(rescc)
  
  
  dat <- data.frame(country = names(ECI),
             ECI = ECI, 
             rang = length(ECI):1, year = year)
  dat <- dat[dat$country %in% c("arm", "blr", "kaz", "kgz", "rus"), ]
  
  df <- rbind(df,dat)
}

df <- dcast(country ~ year, value.var = "rang", data = df)

write.xlsx(df,paste("resultECI/dynamics.xlsx"))










#####################
# Opportunity value #
#####################

dat2 <- read.csv("datFinal4/2015Rca.csv")[,-1]
rownames(dat2) <- dat2[,1]
dat2 <- dat2[,-1]


dat2 <- as.matrix(dat2)
dat2 <- (dat2 > 1)

kp0 <- as.matrix(colSums(dat2))
kc0 <- as.matrix(rowSums(dat2))

matC <-  (dat2 %*% solve(diag(as.numeric(kp0)) ) %*%t(dat2)) 
for(i in 1:dim(dat2)[1]){
  matC[i,] <- matC[i,]/kc0[i]
}

eig <- eigen(matC)
resc <- eig$vectors[,2]
names(resc) <- rownames(dat2)
resc <- sort(resc)
resc <- resc*sign(resc["jpn"])
rescc <- (resc - mean(resc))/sd(resc)
ECI <- sort(rescc)

# "arm" = 8
# oppVal <- matrix(0,dim(dat2)[1],dim(dat2)[2])
# for(country in 1:1:dim(dat2)[1]){
#   currentCountryNames <- rownames(dat2)[country]
#   for(prod in 1:dim(dat2)[2]){
#     print(paste(round(country/dim(dat2)[1]*100,2),round(prod/dim(dat2)[2]*100,2)))
#     if(dat2[country,prod] == 1){
#       next
#     }
#     cdat2 <- dat2
#     cdat2[country,prod] <- 1
#     ckp0 <- as.matrix(colSums(cdat2))
#     ckc0 <- as.matrix(rowSums(cdat2))
#     cmatC <-  (cdat2 %*% solve(diag(as.numeric(ckp0)) ) %*%t(cdat2)) 
#     for(i in 1:dim(dat2)[1]){
#       cmatC[i,] <- cmatC[i,]/ckc0[i]
#     }
#     eig <- eigen(cmatC)
#     cresc <- eig$vectors[,2]
#     names(cresc) <- rownames(dat2)
#     cresc <- cresc*sign(cresc["jpn"])
#     oppVal[country,prod] <- cresc[currentCountryNames]
#     gc()
#   }
# }
# 
# write.csv(oppVal,"oppVal.csv")

dat <- read.csv("oppVal.csv")
row.names(dat) <-row.names(dat2)  
dat <- dat[row.names(dat) %in% c("arm", "blr", "kaz", "kgz", "rus"),]



for(country in c(53,83)){
  currentCountryNames <- rownames(dat2)[country]
  for(prod in 1:dim(dat2)[2]){
    print(paste(round(country/dim(dat2)[1]*100,2),round(prod/dim(dat2)[2]*100,2)))
    if(dat2[country,prod] == 1){
      next
    }
    cdat2 <- dat2
    cdat2[country,prod] <- 1
    ckp0 <- as.matrix(colSums(cdat2))
    ckc0 <- as.matrix(rowSums(cdat2))
    cmatC <-  (cdat2 %*% solve(diag(as.numeric(ckp0)) ) %*%t(cdat2)) 
    for(i in 1:dim(dat2)[1]){
      cmatC[i,] <- cmatC[i,]/ckc0[i]
    }
    eig <- eigen(cmatC)
    cresc <- eig$vectors[,2]
    names(cresc) <- rownames(dat2)
    cresc <- cresc*sign(cresc["jpn"])
    dat[   (country == 53)*4 +(country == 83)*5   ,prod] <- cresc[currentCountryNames]
    gc()
  }
}


dat <- dat[,-1]

write.csv(dat,"oppValForEAU.csv")

for(j in 1:5){
  dat[j, dat[j,] != 0  ] <- dat[j, dat[j,] != 0  ] - resc[row.names(dat)[j]]
}

prox <- matrix(0,dim(dat2)[2],dim(dat2)[2])
for(j in 1:dim(dat2)[2]){
  print( paste(round(j/dim(dat2)[2]*100), "prox" ))
  for(i in 1:dim(dat2)[2]){
    prox[j,i] <- sum(dat2[,j] * dat2[,i])/kp0[i]
  }
}

colnames(prox) <- colnames(dat2)
rownames(prox) <- colnames(dat2)

dcp <- matrix(0,dim(dat2)[1],dim(dat2)[2])
for(j in 1:dim(dat2)[1]){
  print( paste(round(j/dim(dat2)[1]*100), "dcp" ))
  for(i in 1:dim(dat2)[2]){
    dcp[j,i] <- 1-sum((dat2[j,])*prox[i,])/sum(prox[i,])
  }
}
rownames(dcp) <- rownames(dat2)


for(j in 1:5){
  currentCountry <- rownames(dat)[j]
  dat[j,] <- dat[j,] * (1- dcp[currentCountry,])
}

resulting <- rowSums(dat)


write.xlsx(resulting/sd(resc), "oppValResult.xlsx")














#####################
# Opportunity value #
#####################

library(Rcpp)
sourceCpp("test.cpp")

dat2 <- read.csv("datFinal4/2013Rca.csv")[,-1]
rownames(dat2) <- dat2[,1]
dat2 <- dat2[,-1]


dat2 <- as.matrix(dat2)
dat2 <- (dat2 > 1)

kp0 <- as.matrix(colSums(dat2))
kc0 <- as.matrix(rowSums(dat2))

matC <-  (dat2 %*% solve(diag(as.numeric(kp0)) ) %*%t(dat2)) 
for(i in 1:dim(dat2)[1]){
  matC[i,] <- matC[i,]/kc0[i]
}

eig <- eigen(matC)
resc <- eig$vectors[,2]
names(resc) <- rownames(dat2)
resc <- sort(resc)
resc <- resc*sign(resc["jpn"])
rescc <- (resc - mean(resc))/sd(resc)
ECI <- sort(rescc)

dat <- matrix(0, 8, dim(dat2)[2])
rownames(dat) <- c("arm", "blr", "kaz", "kgz", "rus", "aze", "tur", "geo")


for(country in rownames(dat)){
  currentCountryIndex <- which(rownames(dat2) == country)
  for(prod in 1:dim(dat2)[2]){
    print(paste(country,round(prod/dim(dat2)[2]*100,2)))
    if(dat2[currentCountryIndex,prod] == 1){
      next
    }
    cdat2 <- dat2
    cdat2[currentCountryIndex,prod] <- 1
    ckp0 <- as.matrix(colSums(cdat2))
    ckc0 <- as.matrix(rowSums(cdat2))
    
    cmatC <- eigenMapMatMult(eigenMapMatMult(cdat2, diag(as.numeric(1/ckp0))),t(cdat2))
    cmatC <- t(eigenMapMatMult(t(cmatC),diag(as.numeric(1/ckc0))))
    cresc  <-  Re(eigs(cmatC, k =2)$vectors[,2])
    
    names(cresc) <- rownames(dat2)
    cresc <- cresc*sign(cresc["jpn"])
    dat[  country  ,prod] <- cresc[country]
  }
  gc()
}


for(j in 1:8){
  dat[j, dat[j,] != 0  ] <- dat[j, dat[j,] != 0  ] - resc[row.names(dat)[j]]
}

prox <- matrix(0,dim(dat2)[2],dim(dat2)[2])
for(j in 1:dim(dat2)[2]){
  print( paste(round(j/dim(dat2)[2]*100), "prox" ))
  for(i in 1:dim(dat2)[2]){
    prox[j,i] <- sum(dat2[,j] * dat2[,i])/kp0[i]
  }
}

colnames(prox) <- colnames(dat2)
rownames(prox) <- colnames(dat2)

dcp <- matrix(0,dim(dat2)[1],dim(dat2)[2])
for(j in 1:dim(dat2)[1]){
  print( paste(round(j/dim(dat2)[1]*100), "dcp" ))
  for(i in 1:dim(dat2)[2]){
    dcp[j,i] <- 1-sum((dat2[j,])*prox[i,])/sum(prox[i,])
  }
}
rownames(dcp) <- rownames(dat2)


for(j in 1:8){
  currentCountry <- rownames(dat)[j]
  dat[j,] <- dat[j,] * (1 - dcp[currentCountry,])
}

resulting <- rowSums(dat)

write.xlsx(resulting/sd(resc), "oppValResult2013.xlsx")







####################
# Opportunity gain #
####################

library(Rcpp)

sourceCpp("test.cpp")

dat2 <- read.csv("datFinal4/2015Rca.csv")[,-1]
rownames(dat2) <- dat2[,1]
dat2 <- dat2[,-1]


dat2 <- as.matrix(dat2)
dat2 <- (dat2 > 1)

kp0 <- as.matrix(colSums(dat2))
kc0 <- as.matrix(rowSums(dat2))

matC <-  (dat2 %*% solve(diag(as.numeric(kp0)) ) %*%t(dat2)) 
for(i in 1:dim(dat2)[1]){
  matC[i,] <- matC[i,]/kc0[i]
}

eig <- eigen(matC)
resc <- eig$vectors[,2]
names(resc) <- rownames(dat2)
resc <- sort(resc)
resc <- resc*sign(resc["jpn"])
rescc <- (resc - mean(resc))/sd(resc)
ECI <- sort(rescc)


armenianProducts <- dat2["arm",]

checkMatrix <- matrix(0,length(armenianProducts),length(armenianProducts))
for(i in 1:length(armenianProducts)){
  print(  round(i/length(armenianProducts)*100,2)  )
  if(armenianProducts[i]){
    checkMatrix[i,] <- 1
    checkMatrix[,i] <- 1
  }
}

resultMatrix <- checkMatrix*678

nof <- length(armenianProducts)
t <- Sys.time()
for(i in 1:(nof-1)){
  for(j in (i+1):nof){
    if( (nof*i+j) %% 100 == 1){
      print(  as.difftime((Sys.time()-t)/((nof*i+j)/(nof*(nof-1))) - (Sys.time() - t), format = "%X")  )
      print( (nof*i+j)/(nof*(nof-1))  )
    }
    if(checkMatrix[i,j] == 0){
      cdat2 <- dat2
      cdat2[8,c(i,j)] <- 1
      ckp0 <- as.matrix(colSums(dat2))
      ckc0 <- as.matrix(rowSums(dat2))

      cmatC <- eigenMapMatMult(eigenMapMatMult(cdat2, diag(as.numeric(1/ckp0))),t(cdat2))
      cmatC <- t(eigenMapMatMult(t(cmatC),diag(as.numeric(1/ckc0))))
      cresc  <-  Re(eigs(cmatC, k =2)$vectors[,2])
      
      names(cresc) <- rownames(dat2)
      cresc <- cresc*sign(cresc["jpn"])
      resultMatrix[i,j] <- cresc["arm"]
      resultMatrix[j,i] <- cresc["arm"]
    }
  }
}
write.csv(resultMatrix,"weDidIt.csv")






prox <- matrix(0,dim(dat2)[2],dim(dat2)[2])
for(j in 1:dim(dat2)[2]){
  print( paste(round(j/dim(dat2)[2]*100), "prox" ))
  for(i in 1:dim(dat2)[2]){
    prox[j,i] <- sum(dat2[,j] * dat2[,i])/kp0[i]
  }
}

colnames(prox) <- colnames(dat2)
rownames(prox) <- colnames(dat2)

dcp <- matrix(0,dim(dat2)[1],dim(dat2)[2])
for(j in 1:dim(dat2)[1]){
  print( paste(round(j/dim(dat2)[1]*100), "dcp" ))
  for(i in 1:dim(dat2)[2]){
    dcp[j,i] <- 1-sum((dat2[j,])*prox[i,])/sum(prox[i,])
  }
}
colnames(dcp) <- colnames(dat2)
rownames(dcp) <- rownames(dat2)
plot(dcp[8,])



oppGain <- rep(0, times = length( armenianProducts))
names(oppGain) <- names(armenianProducts)
armVal <- resc["arm"]
sd(resc)

oppGain[armenianProducts == 1] <- NA

for(i in 1:length(armenianProducts)){
  if(armenianProducts[i] == 0){
    results <- resultMatrix[i,]
    results[results == 678] <- 0
    results[results != 0] <- results[results != 0]  - armVal
    results <- results*(1-dcp[8,])
    oppGain[i] <- (sum(results)/sd(resc) - 0.62875)/(dcp[8,i])
  }
}

oppGain <- sort(oppGain)
plot(dcp[8,names(oppGain)],oppGain)



dataForOppGainGroups <- data.frame( gain = oppGain, nam = as.character(names(oppGain)) )
dataForOppGainGroups$nam <- substr(as.character(dataForOppGainGroups$nam), 1,
       nchar(as.character(dataForOppGainGroups$nam))-4)

datForOppGainGroups <- aggregate(data = dataForOppGainGroups, gain ~ nam, FUN = mean)

datForOppGainGroups <- datForOppGainGroups[order(datForOppGainGroups$gain),]

write.xlsx(datForOppGainGroups, "oppGainGrouped.xlsx")

round(tail(oppGain, 4),4)
write.xlsx(oppGain, "oppGain.xlsx")



