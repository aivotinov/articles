library(BMR)


#IN LEVELS, BIG BVAR

data_q <- read.csv("data_q_sa4.csv")[,-1]
data_m <- read.csv("data_m_sa.csv")[,-1]


#Norm for better convergence
for(d in 2:ncol(data_q)){
  data_q[,d] <- (data_q[,d] - mean(data_q[,d]))/sd(data_q[,d])
}

for(d in 2:ncol(data_m)){
  data_m[,d] <- (data_m[,d] - mean(data_m[,d]))/sd(data_m[,d])
}

#Q

dd <- data.frame(GDP = data_q$GDP_2011, Ex = data_q$Ex_2011, Im = data_q$Im_2011, Brent = data_q$Brent, 
                 IPC = data_q$IPC, Bout = data_q$Budget_out_r)

bvarm <- BVARM(dd,
               p=2, constant=T, irf.periods=12,
               keep=10000, burnin=5000, VType=1)
#plot(bvarm,save=F)
IRF(bvarm,save=F)
#forecast(bvarm,shocks=TRUE,backdata=10,save=FALSE)




#M

dd <- data.frame(Brent = data_m$Brent, Kurs = data_m$USD_RUR, Construct = data_m$Construction_real_index, 
                 IPC = data_m$IPC, B_in = data_m$Budget_in, B_out = data_m$Budget_out)

bvarm <- BVARM(dd,
               p=2, constant=T, irf.periods=20,
               keep=10000, burnin=5000, VType=1)
#plot(bvarm,save=F)
IRF(bvarm,save=F)
#forecast(bvarm,shocks=TRUE,backdata=10,save=FALSE)



###IN LEVELS, SMALL BVAR

for (k in 3:8){
  data_q <- read.csv("dataFin_q.csv")[,-1]
  dat <- data_q[,c(1,2,k,9)]
  dat <- log(dat)
  dat <- ts(dat, start = 2003.75, freq = 4)
  exog <- dat[,4]
  names(exog) <- names(dat)[,4]
  
  bvarm <- BVARM(dat,
                 p=4, constant=T, irf.periods=20,
                 keep=10000, burnin=5000, VType=1, 
                 HP1 = 0.1, HP2 = 0.7, HP3 = 0.5, HP4 = 2)
  
  #IRF(bvarm,save=F)
  
  effect_g_on_y <- vector(length = 12)
  for (per in 1:12){
    effect_g_on_y[per] <- mean(bvarm$IRFs[per,1,,3])/100
  }
  effect_g_on_g <- vector(length = 12)
  for (per in 1:12){
    effect_g_on_g[per] <- mean(bvarm$IRFs[per,3,,3])/100
  }
  res_y <- (exp(dat[nrow(dat),1] + sum(effect_g_on_y))-exp(dat[nrow(dat),1]) )
  res_g <- (exp(dat[nrow(dat),3] + sum(effect_g_on_g))-exp(dat[nrow(dat),3]) )
  mult <- res_y / res_g
  print(paste(colnames(dat)[3], ": ", mult, sep = ""))
}

data_q <- read.csv("dataFin_q.csv")[,-1]
dat <- data_q[,c(1,2,3,9)]
names(dat) <- c("y", "t", "g", "oil")
dat <- log(dat)
dat <- ts(dat, start = 2003.75, freq = 4)
exog <- dat[,4]
names(exog) <- names(dat)[,4]

iter = 1
mults_vec <- vector()
for (HP_1 in seq(0.1, 0.9, by = 0.2)){
  for(HP_2 in seq(0.1, 0.9, by = 0.2)){
    for(HP_3 in seq(0.5, 1.5, by = 0.3)){
      #for(HP_4 in seq(1.5, 2.5, by = 0.3)){
      HP_4 = 2
                bvarm <- BVARM(dat,
                       p=4, constant=T, irf.periods=20,
                       keep=10000, burnin=5000, VType=1, 
                       HP1 = HP_1, HP2 = HP_2, HP3 = HP_3, HP4 = HP_4)
      print(iter)
      iter = iter + 1

      effect_g_on_y <- vector(length = 12)
      for (per in 1:12){
        effect_g_on_y[per] <- mean(bvarm$IRFs[per,1,,3])/100
      }
      effect_g_on_g <- vector(length = 12)
      for (per in 1:12){
        effect_g_on_g[per] <- mean(bvarm$IRFs[per,3,,3])/100
      }
      res_y <- (exp(dat[nrow(dat),1] + sum(effect_g_on_y))-exp(dat[nrow(dat),1]) )
      res_g <- (exp(dat[nrow(dat),3] + sum(effect_g_on_g))-exp(dat[nrow(dat),3]) )
      mult <- res_y / res_g
      mults_vec <- c(mults_vec, mult)
     # print(paste(mult, HP_1, HP_2, HP_3, HP_4, sep = " "))
      
     # }
    }
  }
}

bvarm <- BVARM(dat,
               p=4, constant=T, irf.periods=20,
               keep=10000, burnin=5000, VType=2, 
               HP1 = 0.4, HP2 = 0.7)

#IRF(bvarm,save=F)

effect_g_on_y <- vector(length = 12)
for (per in 1:12){
  effect_g_on_y[per] <- mean(bvarm$IRFs[per,1,,3])/100
}
effect_g_on_g <- vector(length = 12)
for (per in 1:12){
  effect_g_on_g[per] <- mean(bvarm$IRFs[per,3,,3])/100
}
res_y <- (exp(dat[nrow(dat),1] + sum(effect_g_on_y))-exp(dat[nrow(dat),1]) )
res_g <- (exp(dat[nrow(dat),3] + sum(effect_g_on_g))-exp(dat[nrow(dat),3]) )
mult <- res_y / res_g
mult


#All G-s

data_q <- read.csv("dataFin_q.csv")[,-1]
dat <- data_q[,c(1,2,3:8,9)]
dat <- log(dat)
dat <- ts(dat, start = 2003.75, freq = 4)
exog <- dat[,4]
names(exog) <- names(dat)[,4]

bvarm <- BVARM(dat,
               p=3, constant=T, irf.periods=12,
               keep=10000, burnin=5000, VType=1)

#IRF(bvarm,save=F)

for (g_type in 3:8){
  effect_g_on_y <- vector(length = 12)
  for (per in 1:12){
    effect_g_on_y[per] <- mean(bvarm$IRFs[per,1,,g_type])
  }
  effect_g_on_g <- vector(length = 12)
  for (per in 1:12){
    effect_g_on_g[per] <- mean(bvarm$IRFs[per,g_type,,g_type])
  }
  res_y <- (exp(dat[nrow(dat),1] + sum(effect_g_on_y))-exp(dat[nrow(dat),1]) )
  res_g <- (exp(dat[nrow(dat),g_type] + sum(effect_g_on_g))-exp(dat[nrow(dat),g_type]) )
  mult <- res_y / res_g
  print(paste(colnames(dat)[g_type], ": ", mult, sep = ""))
}




#IN DIFFERENCES

for (k in 3:8){
  data_q <- read.csv("dataFin_q.csv")[,-1]
  dat <- data_q[,c(1,2,k,9)]
  dat <- (dat[2:54,]-dat[1:53,])/dat[1:53,]*100
  dat <- ts(dat, start = 2004, freq = 4)
  exog <- dat[,4]
  names(exog) <- names(dat)[,4]
  
  bvarm <- BVARM(dat,
                 p=4, constant=T, irf.periods=12,
                 keep=10000, burnin=5000, VType=1)
  
  #IRF(bvarm,save=F)
  
  dat <- data_q[,c(1,2,k,9)]
  dat <- dat[nrow(dat),]
  
  effect_g_on_y <- vector(length = 12)
  for (per in 1:12){
    effect_g_on_y[per] <- mean(bvarm$IRFs[per,1,,3])/100
  }
  effect_g_on_g <- vector(length = 12)
  for (per in 1:12){
    effect_g_on_g[per] <- mean(bvarm$IRFs[per,3,,3])/100
  }
  
  effect_g_on_g <- 1 + effect_g_on_g
  effect_g_on_y <- 1 + effect_g_on_y
  
  m <- c(1,1)
  for(j in 1:12){
    m[1] <- m[1]*effect_g_on_y[j]
    m[2] <- m[2]*effect_g_on_g[j]
  }
  
  dat[2,1] <- dat[1,1]*m[1]
  dat[2,3] <- dat[1,3]*m[2]
  
  mult <- (dat[2,] - dat[1,])[1]/(dat[2,] - dat[1,])[3]
  print(paste(colnames(dat)[3], ": ", mult, sep = ""))
}


data_q <- read.csv("dataFin_q.csv")[,-1]
dat <- data_q[,c(1,2,3,9)]
names(dat) <- c("y", "t", "g", "oil")
dat <- (dat[2:54,]-dat[1:53,])/dat[1:53,]*100
dat <- ts(dat, start = 2004, freq = 4)
exog <- dat[,4]
names(exog) <- names(dat)[,4]

bvarm <- BVARM(dat,
               p=4, constant=T, irf.periods=12,
               keep=10000, burnin=5000, VType=1)

#IRF(bvarm,save=F)

dat <- data_q[,c(1,2,3,9)]
dat <- dat[nrow(dat),]

effect_g_on_y <- vector(length = 12)
for (per in 1:12){
  effect_g_on_y[per] <- mean(bvarm$IRFs[per,1,,3])/100
}
effect_g_on_g <- vector(length = 12)
for (per in 1:12){
  effect_g_on_g[per] <- mean(bvarm$IRFs[per,3,,3])/100
}

effect_g_on_g <- 1 + effect_g_on_g
effect_g_on_y <- 1 + effect_g_on_y

m <- c(1,1)
for(j in 1:12){
  m[1] <- m[1]*effect_g_on_y[j]
  m[2] <- m[2]*effect_g_on_g[j]
}

dat[2,1] <- dat[1,1]*m[1]
dat[2,3] <- dat[1,3]*m[2]

mult <- (dat[2,] - dat[1,])[1]/(dat[2,] - dat[1,])[3]
mult





