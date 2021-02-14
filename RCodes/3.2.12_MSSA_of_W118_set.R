if (!require('Rssa')) install.packages('ggplot2'); library('ggplot2')

# Set path
setwd("C:/Users/Aastha/Desktop/GWProject")
Data<-scale(read.csv(file ="Approach_II/MSSA_Work/W118_variables_set.csv",header = TRUE)[-1])
head(Data)

L=120 #Assume window length close to N/2 (N=222 here)
#Create a ssa object
s <- ssa(Data, L = L, kind = "mssa")


#Plot eigenvectors with L window length
windows(width=100, height=50)
par(mar = c(4, 4, 2, 2.5))
par(mfrow=c(5,4))
for(i in 1:20){
  plot(s$U[,i], type="l",ylab = " ")
}
dev.copy(png,file = "Approach_II/MSSA_Work/Eigenvectors_MSSA_W118.png")
dev.off()
  


#Check screeplot
plot(s,pch=16,col="black",ylab="log of singular values",main=" ")
dev.copy(png,file = "Approach_II/MSSA_Work/Screeplot_MSSA_W118.png")
dev.off()


# Calculate the w-correlation matrix between first 30 series
# for a guess for grouping
w <- wcor(s, groups = 1:20)
plot(w, grid = c(2,4, 5,7))
dev.copy(png,file = "Approach_II/MSSA_Work/Wcor_MSSA_W118.png")
dev.off()


#Reconstruct the time series according to the grouping
#This creates a frequency component for each group in each time series
#This means, if there are 15 time series, each series will have 8 frequency components
r <- reconstruct(s,groups = list(c(1,2),c(3,4),c(5,6),c(7,8),c(9,10,11,12),c(13),c(14,15),c(16,17),c(18,19),c(20)))

wellNo <- c("W60","W63","W67","W70","W73","W74","W78","W80","W81","W115","W116","W118")

for (i in 1:length(r)) {
  write.csv(r[i],file=paste0("Approach_II/MSSA_Work/",wellNo[12],"_RC",
                             sprintf("%02d", as.numeric(i)),".csv"))    
}