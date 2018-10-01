# set working directory to download data
setwd("~/Desktop/Data")

# create folders for data
path <- getwd()
dir.create(paste(path,"/135",sep = ""))
library(foreign)


# download data

# 135
for (i in 72:95){
    a <- paste(2010+i%/%12, formatC((i %% 12+1),width = 2, format = "d", flag = "0"), "01", sep = "")
    con <- paste("http://www.cbr.ru/credit/forms/135-",a,".rar", sep = "")
    if (file.exists(paste("135/",a,".rar", sep = "")) == FALSE){
        download.file(con, paste("135/",a,".rar", sep = ""))
        cat(i, "\n")
    }
}


stop("Unzip the files before further processing")
