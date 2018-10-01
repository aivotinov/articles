setwd("~/Dropbox/Статьи/2018/Нормативы и дефолты")
library(openxlsx)
library(reshape2)
library(ggplot2)
library(moments)

dat <- read.csv("Data/135.csv")
otz <- read.xlsx("Data/Otzivi.xlsx")

dat <- dat[,-1]


# оставляем только 2016 и 2017 года
dat <- dat[dat$Date >= 20160101,]
otz <- otz[otz$year >= 2016, ]
dat <- dat[dat$Date < 20180101,]
otz <- otz[otz$year < 2018, ]

# немного преобразовываем данные
dat <- dcast(dat, Date + REGN ~ C1_2, value = C2_2)

# начинаем формировать переменную default
dat$def <- 0

# создаем дополнительный объект, куда будем закидывать данные
# сразу закидываем в него те банки, которые не объявили дефолт
df <- dat[  !(dat$REGN %in%  unique(otz$regn) ),  ]


# по очереди для каждого дефолтного банка выставляем 1 в последние 6 периодов и добавляем
for(i in 1:nrow(otz)){
  cur <- dat[dat$REGN == otz[i,1],]
  if(nrow(cur) > 5){
    cur <- cur[(nrow(cur) - 5):nrow(cur),]  
  }
  cur$def <- 1
  df <- rbind(df,cur)
  rm(cur)
}
rm(dat)

# смотрим 
table(df$def)






# делаем код, который считает количество нарушений норматива для кажой группы

dff <- df
dff$H1.0 <- dff$H1.0 < 8
dff$H1.1 <- dff$H1.1 < 4.5
dff$H1.2 <- dff$H1.2 < 6
dff$H2 <- dff$H2 < 15
dff$H3 <- dff$H3 < 50
dff$H4 <- dff$H4 > 120
dff$H7 <- dff$H7 > 800
dff$H9.1 <- dff$H9.1 > 50
dff$H10.1 <- dff$H10.1 > 3
dff$H12 <- dff$H12 > 25
dff <- dff[,c("H1.0", "H1.1", "H1.2",
              "H2", "H3", "H4", "H7", "H9.1", "H10.1",
              "H12", "def")]

s0 <- (colSums(dff[dff$def == 0, ], na.rm = T))
s1 <- (colSums(dff[dff$def == 1, ], na.rm = T))
p0 <- (colMeans(dff[dff$def == 0, ], na.rm = T))
p1 <- (colMeans(dff[dff$def == 1, ], na.rm = T))
n0 <- s0/p0
n1 <- s1/p1

ptot <- (n1*p1 + n0 * p0)/(n0 + n1)
test <- (p0-p1)/sqrt(  ptot*(1-ptot)/(n0+n1)  )

write.xlsx(rbind(round(100*p1,2),round(100*p0,2), round(100*colMeans(dff, na.rm = T),2)),
           "Analysis/props.xlsx")





#######
# pca #
#######



dfpca <- df[,c("H1.0", "H1.1", "H1.2",
                  "H2", "H3", "H4", "H7", "H9.1", "H10.1",
                  "H12", "def")]

dfpca <- dfpca[complete.cases(dfpca),]

for(i in 1:10){
  dfpca[,i] <- log(dfpca[,i]+1)
}
dfpca <- dfpca[complete.cases(dfpca),]

res <- prcomp(dfpca[,1:10],
       center = TRUE,
       scale. = TRUE)

plot(res, type = "l")
summary(res)

res2 <- data.frame(pc1 = res$x[,1], 
                   pc2 = res$x[,2],
                   pc3 = res$x[,3],
                   pc4 = res$x[,4],
                   def = dfpca$def)

l <- list()
k <- 1
for(i in 1:4){
  cur <- res2[,c(i,5)]
  cur0 <- cur[cur$def == 0, ]
  cur1 <- cur[cur$def == 1, ]
  rm(cur)
  dens0 <- density(cur0[,1], na.rm = T, n = 128)
  dens1 <- density(cur1[,1], na.rm = T, n = 128) 
  df1 <- data.frame(x0 = dens0$x, "Def0" = dens0$y,
                      x1 = dens1$x, "Def1" = dens1$y)
  l[[k]] <- df1
  names(l)[k] <- names(res2)[i]
  k <- k + 1
  rm(df1, dens0, dens1)
}
rm(k,i,cur1,cur0)

write.xlsx(l, "Analysis/resultPCA.xlsx")




#########
# logit #
#########

library(pROC)
library(MASS)

dflog <- df[,c("H1.0", "H1.1", "H1.2",
               "H2", "H3", "H4", "H7", "H9.1", "H10.1",
               "H12", "def")]

dflog <- dfpca[complete.cases(dflog),]

for(i in 1:10){
  dflog[,i] <- log(dflog[,i]+1)
}
dflog <- dflog[complete.cases(dflog),]

fit <- lda(def ~ ., data = dflog)
roc(dflog$def, predict(fit)$posterior[,2])

fit <- glm(def ~ ., data = dflog, family = binomial(link = "logit"))
roc(dflog$def, predict(fit, type = 'response'))

fit <- glm(def ~ ., data = dflog, family = binomial(link = "probit"))
roc(dflog$def, predict(fit, type = 'response'))
