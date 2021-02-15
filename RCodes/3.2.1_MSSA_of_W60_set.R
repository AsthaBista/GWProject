if (!require('Rssa')) install.packages('Rssa'); library('Rssa')

# Set path
setwd("C:/Users/Aastha/Desktop/GWProject")
Data<-scale(read.csv(file ="Approach_II/MSSA_Work/W60_variables_set.csv",header = TRUE)[-1])
head(Data)

L=108 #Assume window length close to N/2 (N=222 here)
#Create a ssa object
s <- ssa(Data, L = L, kind = "mssa")

plot(s)
#Plot eigenvectors with L window length
windows(width=10, height=5)
par(mar = c(4, 4, 2, 2.5))
par(mfrow=c(2,2))
perc <- c("18.72%","18.53%","10.08%","7.75%")
for(i in 1:4){
        plot(s$U[,i], type="l",ylab ="Standardized units",
             xlab = "Months",main=paste0("F",i,": ",perc[i]),cex.main=1)
}

dev.copy(png,file = "Approach_II/MSSA_Work/Eigenvectors_MSSA_W60.png")
dev.off()
  


#Check screeplot
plot(s,pch=16,col="black",ylab="log of singular values",main=" ")
dev.copy(png,file = "Approach_II/MSSA_Work/Screeplot_MSSA_W60.png")
dev.off()


# Calculate the w-correlation matrix between first 30 series
# for a guess for grouping
w <- wcor(s, groups = 1:20)
plot(w, grid = c(2,4, 5,7))
dev.copy(png,file = "Approach_II/MSSA_Work/Wcor_MSSA_W60.png")
dev.off()


#Reconstruct the time series according to the grouping
#This creates a frequency component for each group in each time series
#This means, if there are 15 time series, each series will have 8 frequency components
r <- reconstruct(s,groups = list(c(1,2),c(3,4),c(5,6),c(7,10),c(8,9),c(11),c(12,14),
                                 c(13,15,18),c(16,17),c(19,20)))


wellNo <- c("W60","W63","W67","W70","W73","W74","W78","W80","W81","W115","W116","W118")

for (i in 1:10) {
  write.csv(r[i],file=paste0("Approach_II/MSSA_Work/",wellNo[1],"_RC",
                                                     sprintf("%02d", as.numeric(i)),".csv"))    
}





