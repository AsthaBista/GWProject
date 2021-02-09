###---------------------------------------------------------------------------------------------------------------------------------
###This program converts a non-stationary data to stationary using first degree differencing method
###---------------------------------------------------------------------------------------------------------------------------------

setwd("C:/Users/Aastha/Desktop/GWProject")
list.of.packages <- c("tseries")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#GWData
GW<-read.csv(file ="Data_Processing/GWLevel_imputed.csv",header = TRUE)  
head(GW)

library(tseries)

#Loop around each column in the dataframe to obtain a dataframe containing first degree differenced values
diff_gw<-c()
for(i in 2:13){               #Loop around 13 wells groundwater levels
  diff_dat<-diff(GW[,i])    #First degree differencing
  diff_df<-cbind(diff_df,diff_dat)
}

colnames(diff_gw)<-c("W60","W63","W67","W70","W73","W74","W78","W80","W81","W115","W116","W118") #Add column headings
head(diff_gw)                 #Check the new dataframe

# Check stationarity of each column in new dataframe using Augmented Dickey-Fuller (ADF) 
# t-statistic test for unit root. If H0 is rejected, the series is stationary
for(i in 1:12){ 
  print(adf.test(diff_gw[,i])$p.value)  #Alternate hypotheisis: stationary
}
write.csv(diff_gw,"ApproachI/GWLevels_stationary.csv")

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
write.csv(diff_sw,"ApproachI/SW_stationary.csv")

#Precipitation
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
write.csv(diff_pr,"ApproachI/Precipitation_stationary.csv")


#Pumping
Data<-read.csv(file ="D:/Work/Thesis/Work/Work_Jan10/Preprocessing/Temp2_imp1.csv",header = TRUE)  
head(Data)

library(tseries)

diff_df<-c()
for(i in 1:8){
  diff_dat<-diff(Data[,i])
  diff_df<-cbind(diff_df,diff_dat)
}

colnames(diff_df)<-c("RV","CDY","KY","HY","GI","DC","GO","NP")

#Check stationarity using Augmented Dickey-Fuller (ADF) t-statistic test for unit root
adf.test(diff_df[,4])   #Alternate hypotheisi: stationary

write.csv(diff_df,"D:/Work/Thesis/Work/Work_Jan10/Preprocessing/Temp_stationary.csv")

