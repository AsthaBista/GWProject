############----------------Data Processing-------------------------------------------------######
## This program takes the selected temperature time series with missing values and
## uses linear interpolation to replace the missing values. 
## This program also plots graphs to visualize the original tmpipitation after using 


## Setting up the project
setwd("C:/Users/Aastha/Desktop/GWProject")

if (!require('Rssa')) install.packages("ggplot2", "forecast","reshape2","dplyr");
x <- c("ggplot2", "forecast","reshape2","dplyr");
lapply(x, require, character.only = TRUE)




## Read the data containing groundwater levels with missing values
tmp<- read.csv(file ="Data_Processing/Temperature_with_missinggaps.csv",header = TRUE)
head(tmp)

## Count the missing values
na_count <-sapply(tmp, function(y) sum(length(which(is.na(y)))))
(na_count/nrow(tmp))*100


tmp_imputed<-c()
for(i in 1:ncol(tmp)){
        imp <- na.interp(tmp[,i])
        tmp_imputed<-cbind(tmp_imputed,imp)
}

colnames(tmp_imputed)<-c("RV_T","CDY_T","KY_T","HY_T","GI_T","GO_T","NP_T","DC_T")

head(tmp_imputed)
write.csv(tmp_imputed,"Data_Processing/Temperature_imputed.csv")

# Visualize the imputed values in one of the temperature stations (units changed from F to C)
dat <- sapply(as.data.frame(tmp[,6]), function(y) ((y - 32) / 1.8))
dates = seq(from = as.Date("2000-01-01"), to = as.Date("2018-12-1"), by = 'month')
imp <- na.interp(dat)
x <- 2000 + (which(is.na(dat))-1)/12 
y <- imp[which(is.na(dat))]

dat.ts<-ts(dat,start=c(2000,1),end=c(2018,12),frequency = 12)
ts.plot(dat.ts,gpars = list(xlab="Year",ylab="Temperature (°C)"),main = "Imputed values",
        type="o",lwd=1.5,col="blue")
points(x,y,col="red",pch=1,cex=1) 
mtext(colnames(tmp)[6])
grid()


## Plot temperature
## For plotting, change feet to metres
tmp_m <- sapply(as.data.frame(tmp_imputed), function(y) ((y - 32) / 1.8))

dates = seq(from = as.Date("2000-01-01"), to = as.Date("2018-12-1"), by = 'month')
tmp_m2 <- data.frame(as.data.frame(tmp_m),dates)
colnames(tmp_m2)[9]<-"Date"
tmp_m3 <- melt(tmp_m2, id.vars = "Date", 
                variable.name = "Tmp", value.name="Amount")
tmp_m4 <- tmp_m3%>%
        group_by(Date,Tmp)

ggplot(data = tmp_m4,aes(x = Date, y = Amount)) + 
        geom_line() + facet_wrap(~ Tmp, nrow = 6, scales = "free") +
        xlab("Year") + ylab("Temperature (°C)") + theme_bw()
dev.copy2pdf(file = 'Data_Processing/Temperature_afterImputation.pdf')
dev.off()

