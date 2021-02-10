############----------------Data Processing-------------------------------------------------######
## This program takes the selected precipitation time series with missing values and
## uses linear interpolation to replace the missing values. 
## This program also plots graphs to visualize the original precipitation after using 
## interpolation

## Setting up the project
setwd("C:/Users/Aastha/Desktop/GWProject")
list.of.packages <- c("ggplot2", "forecast","reshape2","dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


## Read the data containing groundwater levels with missing values
prec <- read.csv(file ="Data_Processing/Precipitation_with_missinggaps.csv",header = TRUE)
head(prec)

## Count the missing values
na_count <-sapply(prec, function(y) sum(length(which(is.na(y)))))
(na_count/nrow(prec))*100


library(forecast)
prec_imputed<-c()
for(i in 1:ncol(prec)){
        imp <- na.interp(prec[,i])
        prec_imputed<-cbind(prec_imputed,imp)
}

colnames(prec_imputed)<-c("RV","CDY","KY","HY","GI","GS","GO","NP","DC")

head(prec_imputed)
write.csv(prec_imputed,"Precipitation_imputed.csv")

# Visualize the imputed values in one of the precipitation stations (units changed from inches to mm)
dat <- prec[,1]*25.4
dates = seq(from = as.Date("2000-01-01"), to = as.Date("2018-12-1"), by = 'month')
imp <- na.interp(dat)
x <- 2000 + (which(is.na(dat))-1)/12 
y <- imp[which(is.na(dat))]

dat.ts<-ts(dat,start=c(2000,1),end=c(2018,12),frequency = 12)
ts.plot(dat.ts,gpars = list(xlab="Year",ylab="Precipitation (mm)"),main = "Imputed values",
        type="o",lwd=1.5,col="blue")
points(x,y,col="red",pch=1,cex=1) 
mtext(colnames(prec)[7])
grid()


## Plot precipitation
## For plotting, change feet to metres
prec_m <- sapply(as.data.frame(prec_imputed), function(y) y*25.4)

dates = seq(from = as.Date("2000-01-01"), to = as.Date("2018-12-1"), by = 'month')
prec_m2 <- data.frame(as.data.frame(prec_m),dates)
colnames(prec_m2)[10]<-"Date"
library(reshape2)
library(ggplot2)
library(dplyr)
prec_m3 <- melt(prec_m2, id.vars = "Date", 
                     variable.name = "Prec", value.name="Amount")
prec_m4 <- prec_m3%>%
        group_by(Date,Prec)

ggplot(data = prec_m4,aes(x = Date, y = Amount)) + 
        geom_line() + facet_wrap(~ Prec, nrow = 6, scales = "free") +
        xlab("Year") + ylab("Precipitation (mm)") + theme_bw()
dev.copy2pdf(file = 'Data_Processing/Precipitation_afterImputation.pdf')
dev.off()

