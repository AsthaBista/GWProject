###---------------------------------------------------------------------------------------------------------------
###This program uses k-means and heirarchical clustering
###For k -means clustering, optimal number of clusters is examined using elbow, silhoutte and gap statistics
###---------------------------------------------------------------------------------------------------------------

# Set path
setwd("C:/Users/Aastha/Desktop/GWProject")

#Install packages
list.of.packages <- c("factoextra","NbClust","rgl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library("factoextra")
library(NbClust)
library(rgl)

#Preparation of data
ldfile<-read.csv(file = "Approach_I/PC_loadings.csv",header = T)
head(ldfile)
selectedData<-ldfile[,2:4]
#Set the row names
row.names(selectedData)<-c("W60","W63","W67","W70","W73","W74","W78","W80","W81","W115",
                           "W116","W118")


# Elbow method to find optimal number of clusters
fviz_nbclust(selectedData, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")

#Heirarchical clustering
distanceData = dist(selectedData, method="euclidean")
hclustData<-hclust(distanceData,method="ward.D")
plot(hclustData,main=" ",sub="Heirarchical clustering",xlab=" ")
rect.hclust(hclustData, k=3,border = "red")
mtext('Group A',side = 1,at = 3)
mtext('Group B',side = 1,at = 7)
mtext('Group C',side = 1,at = 10)
dev.copy2pdf(file = 'Approach_I/heirarchicalclustering_Ap1.pdf')
dev.off()

#k-means clustering
kmeansData<-kmeans(selectedData,3)   #k-means clustering using 3 clusters
ClusterData<-kmeansData$cluster

#Plot loadings of PC1 and PC2
png(file = 'Approach_I/PC1-PC2.png')
palette(c("maroon","dark green","blue"))
plot(selectedData[,1],selectedData[,2],ylim=c(-0.3,0.6),xlim=c(-0.15,0.45),
     xlab="PC1",ylab="PC2",pch=19,cex=1,lty='solid',lwd=2,col= ClusterData)
text(selectedData[,c(1,2)],labels=rownames(selectedData),cex=1,pos=3,col= ClusterData)
legend('topright',legend=c("Group A","Group B","Group C"),col=c("maroon","blue","dark green"),pch=19,
       cex=1,bty="y")
grid()
dev.off()

##Plot loadings of PC2 and PC3
png(file = 'Approach_I/PC2-PC3.png')
plot(selectedData[,2],selectedData[,3],ylim=c(-0.5,0.7),xlim=c(-0.3,0.6),
     xlab="PC2",ylab="PC3",pch=19,cex=1,lty='solid',lwd=2,col= ClusterData)
text(selectedData[,c(2,3)],labels=rownames(selectedData),cex=1,pos=3,col= ClusterData)
legend('topright',legend=c("Group A","Group B","Group C"),col=c("maroon","blue","dark green"),pch=19,
       cex=1,bty="y")
dev.off()

##Plot loadings of PC1 and PC3
png(file = 'Approach_I/PC1-PC3.png')
plot(selectedData[,1],selectedData[,3],ylim=c(-0.5,0.6),xlim=c(-0.1,0.5),
     xlab="PC1",ylab="PC3",pch=19,cex=1,lty='solid',lwd=2,col= ClusterData)
text(selectedData[,c(1,3)],labels=rownames(selectedData),cex=1,pos=3,col= ClusterData)
legend('topright',legend=c("Group A","Group B","Group C"),col=c("red","blue","dark green"),pch=19,
       cex=1,bty="y")
dev.off()

#For 3d viewing of clusters
palette(c("red","dark green","blue"))
plot3d(selectedData, col= ClusterData, pch=16,xlab = "PC1",ylab = "PC2",zlab = "PC3")
with(selectedData, text3d(selectedData, texts = row.names(selectedData),col = ClusterData))
dev.off()
