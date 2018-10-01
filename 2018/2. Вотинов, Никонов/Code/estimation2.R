setwd("C:/Users/avotinov/Dropbox/Статьи/2018/Статья - Armenia Complexity/Расчет")
setwd("~/Dropbox/Статьи/2018/Статья - Armenia Complexity/Расчет")
setwd("C:/Users/avotinov/Dropbox/������/2018/������ - Armenia Complexity/������")
library(Matrix)
library(lattice)
library(RSpectra)
library(reshape2)

dat <- read.csv("finalWithRca.csv")


pci <- data.frame()
eci <-  data.frame()
oppVal <- data.frame()
oppGGain <- data.frame()

for(yyear in 2000:2016){
  print(yyear)
  
  cdat <- dat[dat$year == yyear, ]
  cdat <- cdat[,c(2,3,5)]
  
  
  cdat <- dcast(data = cdat, location_code ~ product_id, value.var = "rca")
  rownames(cdat) <- cdat$location_code
  cdat <- cdat[,-1]
  
  
  cdat[is.na(cdat)] <- 0
  
  dat2 <- cdat >= 1
  
  
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
  
  resc <- Re(eigs(matC, k =2)$vectors[,2])
  names(resc) <- rownames(dat2)
  rescc <- (resc - mean(resc))/sd(resc)
  
  ECI <- rescc*sign(rescc["JPN"])
  
  print("ECI done, PCI proc")
  
  if(length(names(eci)) != 0){
    eci <- rbind(eci,ECI[names(eci)])
    names(eci) <- names(ECI)
  } 
  if(!length(names(eci))){
    eci <- rbind(eci,ECI)
    names(eci) <- names(ECI)
  } 
  
  
  
  matP <-  (t(dat2) %*% (diag(as.numeric(kc0)^(-1)) ) %*% (dat2)) 
  for(i in 1:dim(dat2)[2]){
    matP[,i] <- matP[,i]/kp0[i]
  }
  
  
  resp <- Re(eigs(matP, k =2)$vectors[,2])
  resp <- (resp - mean(resp))/sd(resp)
  names(resp) <- colnames(dat2)
  PCI <- (resp)*sign(resp["1689"])
  
  if(length(names(pci)) != 0){
    pci <- rbind(pci,PCI[names(pci)])
    names(pci) <- names(PCI)
  } 
  if(!length(names(pci))){
    pci <- rbind(pci,PCI)
    names(pci) <- names(PCI)
  } 
  
  print("PCI done, prox/dcp proc")
  
  # t <- Sys.time()
  # prox <- matrix(0,dim(dat2)[2],dim(dat2)[2])
  # for(j in 1:dim(dat2)[2]){
  #   print( paste(round(j/dim(dat2)[2]*100), "prox" ))
  #   for(i in 1:dim(dat2)[2]){
  #     prox[j,i] <- sum(dat2[,j] * dat2[,i])/max(kp0[i],kp0[j])
  #   }
  # }
  # Sys.time() - t
  # 
  # t <- Sys.time()
  prox2 <- t(dat2) %*% dat2
  for(j in 1:dim(dat2)[2]){
    #print( paste(round(j/dim(dat2)[2]*100), "prox2" ))
    for(i in 1:dim(dat2)[2]){
      prox2[j,i] <- prox2[j,i]/max(kp0[i],kp0[j])
    }
  }
  # Sys.time() - t
  prox <- prox2
  rm(prox2)
  colnames(prox) <- colnames(dat2)
  rownames(prox) <- colnames(dat2)
  
  
  dcp <- matrix(0,dim(dat2)[1],dim(dat2)[2])
  curr <- rowSums(prox)
  dcp <- (dat2 %*% t(prox))
  
  for(i in 1:dim(dat2)[2]){
    dcp[,i] <- 1- dcp[,i]/curr[i]
  }
  
  rownames(dcp) <- rownames(dat2)
  colnames(dcp) <- colnames(dat2)
  
  #levelplot(dcp)
  
  
  print("prox/dcp done, OppVal proc")
  
  ## Opp value
  oppValCP <- matrix(0,dim(dat2)[1],1)
  for(j in 1:dim(dat2)[1]){
    #print(round(j/dim(dat2)[1]*100))
    oppValCP[j] <- sum(   Re((1-dcp[j,])*(1-dat2[j,])*PCI  ) )
  }
  
  oppValCP <- data.frame(t(oppValCP))
  names(oppValCP) <- rownames(dat2)
  
  plot(rescc,(oppValCP) )
  text(x = rescc, y = oppValCP, labels = names(rescc), cex = 0.8)
  
  if(length(names(oppVal)) != 0){
    oppVal <- rbind(oppVal,oppValCP[names(oppVal)])
    names(oppVal) <- names(oppValCP)
  } 
  if(!length(names(oppVal))){
    oppVal <- rbind(oppVal,oppValCP)
    names(oppVal) <- names(oppValCP)
  } 
  
  
  print("OppVal done, OppGain proc")
  
  ## Opp gain
  oppGain <- matrix(0,dim(dat2)[1],dim(dat2)[2])
  Sprox <- colSums(prox)
  
  for(c in 1: dim(dat2)[1]){
    #print( round(c/dim(dat2)[1],2) )
    for(p in 1: dim(dat2)[2]){
      oppGain[c,p] <- (1-dat2[c,p])*(sum((1-dat2[c,])*prox[p,]*PCI/Sprox) - (1 - dcp[c,p])*PCI[p])
    }
  }
  
  rownames(oppGain) <- rownames(dat2)
  colnames(oppGain) <- colnames(dat2)
  
  if(length(names(oppGGain)) != 0){
    oppGGain <- rbind(oppGGain,oppGain["ARM",][names(oppGGain)])
    names(oppGGain) <- names(oppGain["ARM",])
  } 
  if(!length(names(oppGGain))){
    oppGGain <- rbind(oppGGain,oppGain["ARM",])
    names(oppGGain) <- names(oppGain["ARM",])
  } 
  

}
write.csv(cbind(2000:2016, eci), "res2/eci.csv")
write.csv(cbind(2000:2016, pci), "res2/pci.csv")
write.csv(cbind(2000:2016, oppVal), "res2/oppVal.csv")
write.csv(cbind(2000:2016, oppGGain), "res2/oppGGain.csv")


eciorder <- eci
for(i in 1:nrow(eci)){
  eciorder[i,order(as.numeric(eci[i,]), decreasing = T)] <- (1:ncol(eci))
}


write.csv(eciorder, "res2/eciOrder.csv")


