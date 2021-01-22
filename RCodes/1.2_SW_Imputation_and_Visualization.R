############----------------Data Processing-------------------------------------------------######
## This program takes the select stream gauge height time series with missing values and
## uses linear interpolation to replace the missing values. 
## This program also plots graphs to visualize the original stream gauge height after using 
## interpolation

## Setting up the project
setwd("C:/Users/Aastha/Desktop/GWProject")
list.of.packages <- c("ggplot2", "imputeTS","reshape2","dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


## Read the data containing groundwater levels with missing values
swgaugeht <- read.csv(file ="Data_Processing/SW_with_missinggaps.csv",header = TRUE)
head(swgaugeht)

## Count the missing values
na_count <-sapply(swgaugeht[,-1], function(y) sum(length(which(is.na(y)))))
(na_count/nrow(swgaugeht))*100

# Out of the 12 gauge stations, seven stations were selected based on the percentage of missing values. These time series were further used in linear interpolation.

library(imputeTS)
sw_imputed<-c()
for(i in c(2,3,6,10,11,12,13)){
        imp <- na.interpolation(swgaugeht[,i],option = 'linear')
        sw_imputed<-cbind(sw_imputed,imp)
}

colnames(sw_imputed)<-c("G40","G42","G45","G49","G51","G53","G65")

head(sw_imputed)
write.csv(sw_imputed,"SW_imputed.csv")

# Visualize the imputed values in one of the groundwater wells
imp <- na.interpolation(sapply(as.data.frame(swgaugeht[,11]), function(y) y*0.3048),option = 'linear')
ggplot_na_imputations(sapply(as.data.frame(swgaugeht[,11]), function(y) y*0.3048),imp,
                      xlab = "Months",ylab = "Gauge height (m)",
                      title = paste("Interpolated values in",colnames(swgaugeht)[11]))

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

