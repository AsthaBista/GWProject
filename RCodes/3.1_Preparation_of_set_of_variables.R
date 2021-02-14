### This program prepares data subsets of variables for MSSA analysis


#Gather all required variable files
GW<-read.csv(file ="Data_Processing/GWLevel_imputed.csv",header = TRUE) [-1] 
SW<-read.csv(file ="Data_Processing/SW_imputed.csv",header = TRUE) [-1] 
Pr<-read.csv(file ="Data_Processing/Precipitation_imputed.csv",header = TRUE)[-1] 
Tmp<-read.csv(file ="Data_Processing/Temperature_imputed.csv",header = TRUE)[-1] 
Pmp<-read.csv(file ="Data_Processing/Pumping.csv",header = TRUE)[-1]

head(GW); head(SW);head(Pr);head(Tmp)
#we need to assemble sets of variables that are close to each other
#the sets are named on the basis of groundwater well in the area

w60Gp <- data.frame(GW[,1],SW[,2],Pr[,3],Tmp[,3],c(unlist(Pmp),rep(NA, nrow(GW)-nrow(Pmp)))) ; #W60,G42,KY
colnames(w60Gp) <- c("W60","G42","KY_P","KY_T","Pumping") 
write.csv(w60Gp,"Approach_II/MSSA_Work/W60_variables_set.csv")

w63Gp <- data.frame(GW[,2],SW[,2],Pr[,3],Tmp[,3],c(unlist(Pmp),rep(NA, nrow(GW)-nrow(Pmp))))   #W63,G42,KY
colnames(w63Gp) <- c("W63","G42","KY_P","KY_T","Pumping") 
write.csv(w63Gp,"Approach_II/MSSA_Work/W63_variables_set.csv")

w67Gp <- data.frame(GW[,3],SW[,3],Pr[,3],Tmp[,3],c(unlist(Pmp),rep(NA, nrow(GW)-nrow(Pmp))))   #W67,G45,KY
colnames(w67Gp) <- c("W67","G45","KY_P","KY_T","Pumping") 
write.csv(w67Gp,"Approach_II/MSSA_Work/W67_variables_set.csv")

w70Gp <- data.frame(GW[,4],SW[,1],Pr[,2],Tmp[,2],c(unlist(Pmp),rep(NA, nrow(GW)-nrow(Pmp))))   #W70,G40,CDY
colnames(w70Gp) <- c("W70","G40","CDY_P","CDY_T","Pumping") 
write.csv(w70Gp,"Approach_II/MSSA_Work/W70_variables_set.csv")

w73Gp <- data.frame(GW[,5],SW[,1],Pr[,2],Tmp[,2],c(unlist(Pmp),rep(NA, nrow(GW)-nrow(Pmp))))   #W73,G40,CDY
colnames(w73Gp) <- c("W73","G40","CDY_P","CDY_T","Pumping") 
write.csv(w73Gp,"Approach_II/MSSA_Work/W73_variables_set.csv")

w74Gp <- data.frame(GW[,6],SW[,2],Pr[,7],Tmp[,6],c(unlist(Pmp),rep(NA, nrow(GW)-nrow(Pmp))))   #W74,G42,GO
colnames(w74Gp) <- c("W74","G42","GO_P","GO_T","Pumping") 
write.csv(w74Gp,"Approach_II/MSSA_Work/W74_variables_set.csv")

w78Gp <- data.frame(GW[,7],SW[,4],Pr[,5],Tmp[,5],c(unlist(Pmp),rep(NA, nrow(GW)-nrow(Pmp))))   #W78,G49,GI
colnames(w78Gp) <- c("W78","G49","GI_P","GI_T","Pumping") 
write.csv(w78Gp,"Approach_II/MSSA_Work/W78_variables_set.csv")

w80Gp <- data.frame(GW[,8],SW[,2],Pr[,7],Tmp[,6],c(unlist(Pmp),rep(NA, nrow(GW)-nrow(Pmp))))   #W80,G42,GO
colnames(w80Gp) <- c("W80","G42","GO_P","GO_T","Pumping") 
write.csv(w80Gp,"Approach_II/MSSA_Work/W80_variables_set.csv")

w81Gp <- data.frame(GW[,9],SW[,4],Pr[,5],Tmp[,5],c(unlist(Pmp),rep(NA, nrow(GW)-nrow(Pmp))))   #W81,G49,GI
colnames(w81Gp) <- c("W81","G49","GI_P","GI_T","Pumping") 
write.csv(w81Gp,"Approach_II/MSSA_Work/W81_variables_set.csv")

w115Gp <- data.frame(GW[,10],SW[,6],Pr[,5],Tmp[,5],c(unlist(Pmp),rep(NA, nrow(GW)-nrow(Pmp))))  #W115,G53,GI
colnames(w115Gp) <- c("W115","G53","GI_P","GI_T","Pumping") 
write.csv(w115Gp,"Approach_II/MSSA_Work/W115_variables_set.csv")

w116Gp <- data.frame(GW[,11],SW[,6],Pr[,5],Tmp[,5],c(unlist(Pmp),rep(NA, nrow(GW)-nrow(Pmp))))  #W116,G53,GI
colnames(w116Gp) <- c("W116","G53","GI_P","GI_T","Pumping") 
write.csv(w80Gp,"Approach_II/MSSA_Work/W116_variables_set.csv")

w118Gp <- data.frame(GW[,12],SW[,7],Pr[,9],Tmp[,8],c(unlist(Pmp),rep(NA, nrow(GW)-nrow(Pmp))))  #W116,G65,DC
colnames(w118Gp) <- c("W118","G65","DC_P","DC_T","Pumping") 
write.csv(w118Gp,"Approach_II/MSSA_Work/W118_variables_set.csv")

