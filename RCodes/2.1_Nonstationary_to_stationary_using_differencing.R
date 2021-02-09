###---------------------------------------------------------------------------------------------------------------------------------
###This program converts a non-stationary data to stationary using first degree differencing method
###---------------------------------------------------------------------------------------------------------------------------------

setwd("C:/Users/Aastha/Desktop/GWProject")
list.of.packages <- c("tseries")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## First, here is a function to plot many time series using ggplot
## We will be using this function in the following lines
## Plot the stationary time series

create_timeseries_plots <- function(df){
  # change into metres
  df <- sapply(as.data.frame(df), function(y) y*0.3048)
  # column with dates
  dates = seq(from = as.Date("2000-02-01"), to = as.Date("2018-12-1"), by = 'month') 
  # add date column to dataframe
  df_m <- data.frame(as.data.frame(df[,-1]),dates)
  colnames(df_m)[ncol(df_m)]<-"Date"
  library(reshape2)
  library(ggplot2)
  library(dplyr)
  # Rearrange dataframe to long form
  df_m2 <- melt(df_m, id.vars = "Date", 
                variable.name = "Var", value.name="Val")
  # Group the dataframe
  df_m3 <- df_m2%>%
    group_by(Date,Var)
  #Plot using ggplot2
  ggplot(data = df_m3,aes(x = Date, y = Val)) + 
    geom_line() + facet_wrap(~ Var, nrow = 6, scales = "free") +
    xlab("Year") + ylab("Units(ft)") + theme_bw()
}

## Now, for converting time series to stationary

#GWData
GW<-read.csv(file ="Data_Processing/GWLevel_imputed.csv",header = TRUE)  
head(GW)

library(tseries)

#Loop around each column in the dataframe to obtain a dataframe containing first degree differenced values
diff_gw<-c()
for(i in 2:13){               #Loop around 13 wells groundwater levels
  diff_dat<-diff(GW[,i])    #First degree differencing
  diff_gw<-cbind(diff_gw,diff_dat)
}

colnames(diff_gw)<-c("W60","W63","W67","W70","W73","W74","W78","W80","W81","W115","W116","W118") #Add column headings
head(diff_gw)                 #Check the new dataframe

# Check stationarity of each column in new dataframe using Augmented Dickey-Fuller (ADF) 
# t-statistic test for unit root. If H0 is rejected, the series is stationary
for(i in 1:12){ 
  print(adf.test(diff_gw[,i])$p.value)  #Alternate hypotheisis: stationary
}
write.csv(diff_gw,"Approach_I/GWLevels_stationary.csv")
# Plot stationary time series
create_timeseries_plots(diff_gw)
dev.copy2pdf(file = 'Approach_I/GroundwaterLevels_stationary.pdf')
dev.off()
####-----------------------------------------------------------------------------------------------------------------------------------
#The above process was carried out for all variables: streamflow, precipitation and pumping

#Streamflow
SW<-read.csv(file ="Data_Processing/SW_imputed.csv",header = TRUE)  
head(SW)

diff_sw<-c()
for(i in 2:8){
  diff_dat<-diff(SW[,i])
  diff_sw<-cbind(diff_sw,diff_dat)
}

colnames(diff_sw)<-c("G40","G42","G45","G49","G51","G53","G65")

#Check stationarity using Augmented Dickey-Fuller (ADF) t-statistic test for unit root
for(i in 1:7){ 
  print(adf.test(diff_sw[,i])$p.value)  #Alternate hypotheisis: stationary
}
write.csv(diff_sw,"Approach_I/SW_stationary.csv")

# Plot stationary time series
create_timeseries_plots(diff_sw)
dev.copy2pdf(file = 'Approach_I/Streamstage_stationary.pdf')
dev.off()
# ----------------------Precipitation------------------------------------------------
Prec<-read.csv(file ="Data_Processing/Precipitation_imputed.csv",header = TRUE)  
head(Prec)


diff_pr<-c()
for(i in 2:10){
  diff_dat<-diff(Prec[,i])
  diff_pr<-cbind(diff_pr,diff_dat)
}

colnames(diff_pr)<-c("RV","CDY","KY","HY","GI","GS","GO","NP","DC")

#Check stationarity using Augmented Dickey-Fuller (ADF) t-statistic test for unit root
for(i in 1:9){ 
  print(adf.test(diff_pr[,i])$p.value)  #Alternate hypotheisis: stationary
}
write.csv(diff_pr,"Approach_I/Precipitation_stationary.csv")
create_timeseries_plots(diff_pr)
dev.copy2pdf(file = 'Approach_I/Streamstage_stationary.pdf')
dev.off()

#Pumping
Pmp<-read.csv(file ="Data_Processing/Pumping.csv",header = TRUE)  
head(Pmp)


diff_pm<-diff(Pmp[,2])

#Check stationarity using Augmented Dickey-Fuller (ADF) t-statistic test for unit root
adf.test(diff_pm)   #Alternate hypotheisi: stationary

write.csv(diff_pm,"Approach_I/Pumping_stationary.csv")

pmp_m <- diff_pm * 0.0283168 #into cubic metres
dates = seq(from = as.Date("2000-02-01"), to = as.Date("2012-12-01"), by = 'month')
pmp_m2 <- data.frame(as.data.frame(pmp_m),dates)
colnames(pmp_m2)<-c("Pumping","Date")
library(ggplot2)


ggplot(data = pmp_m2,aes(x = Date, y = Pumping)) + 
  geom_line() + xlab("Year") + ylab("Pumping (cubic metres)") + scale_y_reverse()+
  scale_x_date(date_breaks = "12 months",date_labels = "%Y") + theme_bw()

dev.copy2pdf(file = 'Approach_I/Pumping_stationary.pdf')
dev.off()
