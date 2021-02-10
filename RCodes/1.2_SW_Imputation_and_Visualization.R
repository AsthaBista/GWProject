############----------------Data Processing-------------------------------------------------######
## This program takes the select stream gauge height time series with missing values and
## uses linear interpolation to replace the missing values. 
## This program also plots graphs to visualize the original stream gauge height after using 
## interpolation

## Setting up the project
setwd("C:/Users/Aastha/Desktop/GWProject")
list.of.packages <- c("ggplot2", "forecast","reshape2","dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


## Read the data containing groundwater levels with missing values
swgaugeht <- read.csv(file ="Data_Processing/SW_with_missinggaps.csv",header = TRUE)
head(swgaugeht)

## Count the missing values
na_count <-sapply(swgaugeht[,-1], function(y) sum(length(which(is.na(y)))))
(na_count/nrow(swgaugeht))*100

# Out of the 12 gauge stations, seven stations were selected based on the percentage of missing values. These time series were further used in linear interpolation.

library(forecast)
sw_imputed<-c()
for(i in c(2,3,6,10,11,12,13)){
        imp <- na.interp(swgaugeht[,i])
        sw_imputed<-cbind(sw_imputed,imp)
}

colnames(sw_imputed)<-c("G40","G42","G45","G49","G51","G53","G65")

head(sw_imputed)
write.csv(sw_imputed,"SW_imputed.csv")

# Visualize the imputed values in one of the groundwater wells
dat <- swgaugeht[,2]*0.3048
dates = seq(from = as.Date("2000-01-01"), to = as.Date("2018-12-1"), by = 'month')
imp <- na.interp(dat)
x <- 2000 + (which(is.na(dat))-1)/12 
y <- imp[which(is.na(dat))]

dat.ts<-ts(dat,start=c(2000,1),end=c(2018,12),frequency = 12)
ts.plot(dat.ts,gpars = list(xlab="Year",ylab="Water level (m)"),main = "Imputed values",
        type="o",lwd=1.5,col="blue")
points(x,y,col="red",pch=1,cex=1) 
mtext(colnames(swgaugeht)[2])
grid()

## Plot stream gauge height
## For plotting, change feet to metres
swgaugeht_m <- sapply(as.data.frame(sw_imputed), function(y) y*0.3048)

dates = seq(from = as.Date("2000-01-01"), to = as.Date("2018-12-1"), by = 'month')
swgaugeht_m2 <- data.frame(as.data.frame(swgaugeht_m),dates)
colnames(swgaugeht_m2)[8]<-"Date"
library(reshape2)
library(ggplot2)
library(dplyr)
swgaugeht_m3 <- melt(swgaugeht_m2, id.vars = "Date", 
                   variable.name = "GaugeSt", value.name="GgHt")
swgaugeht_m4 <- swgaugeht_m3%>%
        group_by(Date,GaugeSt)
swgaugeht_m4
ggplot(data = swgaugeht_m4,aes(x = Date, y = GgHt)) + 
        geom_line() + facet_wrap(~ GaugeSt, nrow = 6, scales = "free") +
        xlab("Year") + ylab("Gauge height (m)") + theme_bw()
dev.copy2pdf(file = 'Data_Processing/StreamGaugeHt_afterImputation.pdf')
dev.off()

