setwd("//v18-fps/users/avotinov/pr/Desktop/Data ECI")

dat <- read.csv(gzfile('country_hsproduct_year.csv.gz'))
dat <- dat[dat$year >= 2000, ]

cdat <- dat[dat$year >= 2014, ]

agg16 <- aggregate( cbind(export_value, import_value) ~ location_code, FUN = sum, data = cdat)
agg16 <- agg16[agg16$export_value >= 3*10**9, ]

library(openxlsx)

pop <- read.xlsx("population.xlsx")
pop <- pop[pop$`2016` >= 10**6,]

agg16 <- agg16[agg16$location_code %in% pop$Country.Code,]

agg16 <- agg16[ !(agg16$location_code %in% c("TCD", "IRQ", "MAC")), ]

tot <- aggregate( cbind(export_value) ~ location_code + year, FUN = sum, data = dat)

library(reshape2)

tot <- dcast( formula = location_code ~ year, value.var = "export_value", data = tot)

tot2 <- tot[ tot$location_code %in% agg16$location_code, ]
tot2 <- tot2[complete.cases(tot2), ]


dat <- dat[dat$location_code %in% tot2$location_code, ]

mean(100*colSums(tot2[,2:ncol(tot2)], na.rm = T)/
colSums(tot[,2:ncol(tot)], na.rm = T))

100*sum(gdp[gdp$Country.Code %in% agg16$location_code, 3], na.rm = T)/
gdp[gdp$Country.Code == "WLD",3]

100*sum(pop[pop$Country.Code %in% agg16$location_code, 3], na.rm = T)/
  pop[pop$Country.Code == "WLD",3]

dat <- dat[ !(dat$product_id %in% as.numeric(names(which(table(dat$product_id) != 2329))) ), ]

table(table(dat$product_id))







dat2 <- dat[,c(1, 2, 3, 13)]

dat2$rca <- 0

finalWithRca <- data.frame()

for(yyear in unique(dat2$year)){
  print(yyear)
  cdat <- dat2[dat2$year == yyear,]
  totalExport <- sum(cdat$export_value)
  totalExportByProduct <- aggregate(data = cdat, export_value ~ product_id, FUN = sum)
  names(totalExportByProduct) <- c("product_id", "totalExportByProduct")
  totalExportByCountry <- aggregate(data = cdat, export_value ~ location_code, FUN = sum)
  names(totalExportByCountry) <- c("location_code", "totalExportByCountry")
  cdat$totalExport <- totalExport
  cdat <- merge(cdat, totalExportByProduct, by = "product_id")
  cdat <- merge(cdat, totalExportByCountry, by = "location_code")
  cdat$rca <- (cdat$export_value/cdat$totalExportByCountry)/(cdat$totalExportByProduct/cdat$totalExport)
  finalWithRca <- rbind(finalWithRca, cdat)
}



finalWithRca <- finalWithRca[,c(1,2,3,5)]
write.csv(finalWithRca, "finalWithRca.csv")
rm(cdat,dat2,gdp,l,pop,tot,tot2,totalExportByCountry, totalExportByProduct,d,labs,totalExport,x,yyear,
   dfMaker)

finalWithRca <- merge(finalWithRca, dat)
finalWithRca$ccc <- (finalWithRca$rca > 1) - (finalWithRca$export_rca > 1)
finalWithRca[finalWithRca$ccc != 0,]$rca <- finalWithRca[finalWithRca$ccc != 0,]$export_rca

finalWithRca$ccc <- (finalWithRca$rca > 1) - (finalWithRca$export_rca > 1)
table(finalWithRca$ccc)

finalWithRca <- finalWithRca[,c(1,2,3,4)]
write.csv(finalWithRca, "finalWithRca2.csv")
