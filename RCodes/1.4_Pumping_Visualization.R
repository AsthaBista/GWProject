setwd("C:/Users/Aastha/Desktop/GWProject")
list.of.packages <- c("ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


## Read the data containing groundwater levels with missing values
pmp <- read.csv(file ="Data_Processing/Pumping.csv",header = TRUE)
head(pmp)

## Count the missing values
na_count <-sum(length(which(is.na(pmp[,2]))))
na_count

##Plot pumping
## For plotting, change cubic feet to cubic metres
pmp_m <- pmp[,2]* 0.0283168
head(pmp_m)
length(pmp_m)

dates = seq(from = as.Date("2000-01-01"), to = as.Date("2012-12-01"), by = 'month')
pmp_m2 <- data.frame(as.data.frame(pmp_m),dates)
colnames(pmp_m2)<-c("Pumping","Date")
library(ggplot2)


ggplot(data = pmp_m2,aes(x = Date, y = Pumping)) + 
        geom_line() + xlab("Year") + ylab("Pumping (cubic metres)") + 
        scale_x_date(date_breaks = "12 months",date_labels = "%Y") + theme_bw()
dev.copy2pdf(file = 'Data_Processing/Pumping.pdf')
dev.off()

#Now plotting with reverse y axis, to compare it with water levels
ggplot(data = pmp_m2,aes(x = Date, y = Pumping)) + 
        geom_line() + xlab("Year") + ylab("Pumping (cubic metres)") + scale_y_reverse()+
        scale_x_date(date_breaks = "12 months",date_labels = "%Y") + theme_bw()
dev.copy2pdf(file = 'Data_Processing/Pumping_reverse.pdf')
dev.off()
