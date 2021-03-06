
setwd("C:/Users/Aastha/Desktop/GWProject")
list.of.packages <- c("ggplot2", "imputeTS","reshape2","dplyr","forecast")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


## Read the data containing groundwater levels with missing values
gwlevel <- read.csv(file ="Data_Processing/GWLevel_with_missinggaps.csv",header = TRUE)
head(gwlevel)

## Count the missing values
na_count <-sapply(gwlevel[,2:13], function(y) sum(length(which(is.na(y)))))
na_count

## Since the number of missing values is less than 10%, a linear interpolation was used
## Linear interpolation of each time series 
library(imputeTS)
library(forecast)

gw_imputed<-c()
for(i in 2:ncol(gwlevel)){
        imp <- na.interp(gwlevel[,i])
        gw_imputed<-cbind(gw_imputed,imp)
}
      
colnames(gw_imputed)<-c("W60","W63","W67","W70","W73","W74","W78","W80","W81","W115","W116","W118") #Add column headings

head(gw_imputed)
write.csv(gw_imputed,"Data_Processing/GWLevel_imputed.csv")

# Visualize the imputed values in one of the groundwater wells

dat <- gwlevel[,11]*0.3048
dates = seq(from = as.Date("2000-01-01"), to = as.Date("2018-12-1"), by = 'month')
imp <- na.interp(dat)
x <- 2000 + (which(is.na(dat))-1)/12 
y <- imp[which(is.na(dat))]

dat.ts<-ts(dat,start=c(2000,1),end=c(2018,12),frequency = 12)
ts.plot(dat.ts,gpars = list(xlab="Year",ylab="Water level (m)"),main = "Imputed values",
        type="o",lwd=1.5,col="blue")
points(x,y,col="red",pch=1,cex=1) 
mtext(colnames(gwlevel)[11])
grid()




##Plot groundwater wells
## For plotting, change feet to metres
gwlevel_m <- sapply(as.data.frame(gw_imputed), function(y) y*0.3048)

dates = seq(from = as.Date("2000-01-01"), to = as.Date("2018-12-1"), by = 'month')
gwlevel_m2 <- data.frame(as.data.frame(gwlevel_m),dates)
colnames(gwlevel_m2)[13]<-"Date"
library(reshape2)
library(ggplot2)
library(dplyr)
gwlevel_m3 <- melt(gwlevel_m2, id.vars = "Date", 
                   variable.name = "Wells", value.name="WL")
gwlevel_m4 <- gwlevel_m3%>%
        group_by(Date,Wells)
gwlevel_m4
ggplot(data = gwlevel_m4,aes(x = Date, y = WL)) + 
        geom_line() + facet_wrap(~ Wells, nrow = 6, scales = "free") +
        xlab("Year") + ylab("Water level (m)") + theme_bw()
dev.copy2pdf(file = 'Data_Processing/GroundwaterLevels_afterImputation.pdf')
dev.off()


