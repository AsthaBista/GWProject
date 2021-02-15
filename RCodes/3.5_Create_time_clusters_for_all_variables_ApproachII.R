# Set path
setwd("C:/Users/Aastha/Desktop/GWProject")

if (!require("TSclust")) install.packages("TSclust","dplyr");
x <- c("TSclust","dplyr");
lapply(x, require, character.only = TRUE)


# Prepare data
W <- read.csv("Data_Processing/GWLevel_imputed.csv", sep=",")[-1]
S <- read.csv("Data_Processing/SW_imputed.csv", sep=",")[-1]
P <- read.csv("Data_Processing/Precipitation_imputed.csv", sep=",")[-1]
Tm <- read.csv("Data_Processing/Temperature_imputed.csv", sep=",")[-1]
Pu <- -read.csv("Data_Processing/Pumping.csv", sep=",")[-1]

#Combining all variables
All <- data.frame(W,S,P,Tm)
All$Pu <- c(unlist(Pu),rep(NA, nrow(All)-nrow(Pu)))
tbl_df(All)

#Selecting variables for cluster analysis
onlyWells <- scale(All[,1:12])
allVar <- scale(All[1:156,])

#Using Pearson's correlation coeffiecint for oly groundwater levels
cordat<-diss(t(onlyWells), "COR")
hc<-hclust(cordat, method="ward.D")
plot(hc,main=" ",sub =" ",xlab = " ")
rect(0.7,-0.15,6.3,1.3,border =  "red")
rect(6.6,-0.15,9.1,1.3,border =  "red")
rect(9.5,-0.15,12.2,1.35,border =  "red")
mtext('Group I',side = 1,at = 3)
mtext('Group III',side = 1,at = 8)
mtext('Group II',side = 1,at = 11)
dev.copy(png,file= "Approach_II/Heirarchical_clusters_onlyWells_ApproachII.png")
dev.off() 



#Using Pearson's correlation coeffiecint foe all variables
cordat<-diss(t(allVar), "COR")
hc<-hclust(cordat, method="ward.D")
plot(hc,main=" ",sub =" ",xlab = " ")
rect.hclust(hc, k=3,border = 'red', )
mtext('C1',side = 1,at = 8)
mtext('C2',side = 1,at = 25)
mtext('C3',side = 1,at = 32)
dev.copy(png,file= "Approach_II/Heirarchical_clusters_allVariables_ApproachII.png")
dev.off() 
