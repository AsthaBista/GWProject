############----------------Data Processing-------------------------------------------------######
## This program takes the selected precipitation time series with missing values and
## uses linear interpolation to replace the missing values. 
## This program also plots graphs to visualize the original precipitation after using 
## interpolation

## Setting up the project
setwd("C:/Users/Aastha/Desktop/GWProject")
list.of.packages <- c("ggplot2", "imputeTS","reshape2","dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


## Read the data containing groundwater levels with missing values
prec <- read.csv(file ="Data_Processing/Precipitation_with_missinggaps.csv",header = TRUE)
head(prec)

## Count the missing values
na_count <-sapply(prec, function(y) sum(length(which(is.na(y)))))
(na_count/nrow(prec))*100


library(imputeTS)
prec_imputed<-c()
for(i in 1:ncol(prec)){
        imp <- na.interpolation(prec[,i],option = 'linear')
        prec_imputed<-cbind(prec_imputed,imp)
}

colnames(prec_imputed)<-c("RV","CDY","KY","HY","GI","GS","GO","NP","DC")

head(prec_imputed)
write.csv(prec_imputed,"Precipitation_imputed.csv")

# Visualize the imputed values in one of the precipitation stations (units changed from inches to mm)
imp <- na.interpolation(sapply(as.data.frame(prec[,1]), function(y) y*25.4),option = 'linear')
ggplot_na_imputations(sapply(as.data.frame(prec[,1]), function(y) y*25.4),imp,
                      xlab = "Months",ylab = "Precipitation (mm)",
                      title = paste("Interpolated values in",colnames(prec_imputed)[1]))

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

