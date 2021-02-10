###---------------------------------------------------------------------------------------------------------------
###This program uses k-means and heirarchical clustering
###For k -means clustering, optimal number of clusters is examined using elbow, silhoutte and gap statistics
###---------------------------------------------------------------------------------------------------------------

path <- "D:/Work/Thesis/Work/Work_Jan10/PCA/PCAFiles"
setwd(path)


myClusters<-function(data_with_path,imagepath){
  data<-read.csv(file = data_with_path,header = T)
  sel.dat<-data[,2:4]
  #Set the row names
  row.names(sel.dat)<-c("W60","W63","W67","W70","W73","W74","W78","W80","W81","W115","W116","W118")
  library(factoextra)
  library(NbClust)
    # Elbow method
  fviz_nbclust(sel.dat, kmeans, method = "wss") +
    geom_vline(xintercept = 3, linetype = 2)+
    labs(subtitle = "Elbow method")
  #dev.copy(png,file.path(imagepath,'Elbow method.png'))
  #dev.off()
  #Heirarchical clustering
  dist_dat = dist(sel.dat, method="euclidean")
  hclust_dat<-hclust(dist_dat,method="ward.D")
  plot(hclust_dat,main=" ",sub="Heirarchical clustering",xlab=" ")
  rect.hclust(hclust_dat, k=3,border = "red")
  mtext('Group A',side = 1,at = 3)
  mtext('Group B',side = 1,at = 7)
  mtext('Group C',side = 1,at = 10)
  dev.copy2pdf(file = 'D:/Work/Thesis/Work/Work_Jan10/PCA/Images/heirarchical_Ap2.pdf')
  dev.off()
  
  #k-means cluster
  mydata<-sel.dat
  data_k<-kmeans(mydata,3)   #k-means clustering using 3 clusters
  Cluster<-data_k$cluster
  
  png(file = 'D:/Work/Thesis/Work/Work_Jan10/PCA/Images/PC1-PC2.png')
  palette(c("maroon","blue","dark green"))
  ##Plot loadings of PC1 and PC2
  plot(sel.dat[,1],sel.dat[,2],ylim=c(-0.3,0.6),xlim=c(-0.15,0.45),
       xlab="PC1",ylab="PC2",pch=19,cex=1,lty='solid',lwd=2,col=Cluster)
  text(sel.dat[,c(1,2)],labels=rownames(sel.dat),cex=1,pos=3,col=Cluster)
  legend('topright',legend=c("Group A","Group B","Group C"),col=c("maroon","blue","dark green"),pch=19,
         cex=1,bty="y")
  dev.off()
  
  ##Plot loadings of PC2 and PC3
  png(file = 'D:/Work/Thesis/Work/Work_Jan10/PCA/Images/PC2-PC3.png')
  plot(sel.dat[,2],sel.dat[,3],ylim=c(-0.5,0.7),xlim=c(-0.3,0.6),
       xlab="PC2",ylab="PC3",pch=19,cex=1,lty='solid',lwd=2,col=Cluster)
  text(sel.dat[,c(2,3)],labels=rownames(sel.dat),cex=1,pos=3,col=Cluster)
  legend('topright',legend=c("Group A","Group B","Group C"),col=c("maroon","blue","dark green"),pch=19,
         cex=1,bty="y")
  dev.off()
 
  ##Plot loadings of PC1 and PC3
  png(file = 'D:/Work/Thesis/Work/Work_Jan10/PCA/Images/PC1-PC3.png')
  plot(sel.dat[,1],sel.dat[,3],ylim=c(-0.5,0.6),xlim=c(-0.1,0.5),
       xlab="PC1",ylab="PC3",pch=19,cex=1,lty='solid',lwd=2,col=Cluster)
  text(sel.dat[,c(1,3)],labels=rownames(sel.dat),cex=1,pos=3,col=Cluster)
  legend('topright',legend=c("Group A","Group B","Group C"),col=c("red","blue","dark green"),pch=19,
         cex=1,bty="y")
  dev.off()
}



## Now, using the function:
#Arguments
data_with_path<-"D:/Work/Thesis/Work/Work_Jan10/PCA/PCAFiles/Loadings_normal_stat.csv"
imagepath<-"D:/Work/Thesis/Work/Work_Jan10/PCA/Images"
myClusters(data_with_path,imagepath)








#Prepare data
data<-read.csv(file ="Loadings_normal_stat.csv",header = T)
sel.dat<-data[1:12,2:4]

#Set the row names
row.names(sel.dat)<-c("W60","W63","W67","W70","W73","W74","W78","W80","W81","W115","W116","W118")

#Elbow method
k.max <- 10 # Maximal number of clusters
mydata<-z
wss <- sapply(1:k.max,
              function(k){kmeans(sel.dat, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 3, lty =2)

#Install packages
install.packages("factoextra")
install.packages("NbClust")
library(factoextra)
library(NbClust)


# Elbow method
fviz_nbclust(sel.dat, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(sel.dat, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(sel.dat, kmeans, nstart = 10,  method = "gap_stat", nboot = 100)+
  labs(subtitle = "Gap statistic method")

##Heirarchial cluster
dist_dat = dist(sel.dat, method="euclidean")
hclust_dat<-hclust(dist_dat,method="ward.D")
plot(hclust_dat,cex=1.2,main=" ")
rect.hclust(hclust_dat, k=3,border = "red")

sessionInfo()
#K-means cluster
#k-means cluster
mydata<-sel.dat
data_k<-kmeans(mydata,3)   #k-means clustering using 3 clusters
Cluster<-data_k$cluster
mydata$Group <- as.factor(Cluster)
nrow(mydata)
data_k$size
write.csv(Cluster,file="Platte_kmeans.csv")

#3d clusters
library(rgl)
palette(c("red","blue","dark green","brown","dark green"))
plot3d(sel.dat, col=Cluster, pch=16,xlab = "PC1",ylab = "PC2",zlab = "PC3")
with(sel.dat, text3d(sel.dat, texts = row.names(sel.dat),col = Cluster))
dev.off()

##Plot loadings of PCs
palette(c("maroon","blue","dark green","blue"))
plot(sel.dat[,1],sel.dat[,2],ylim=c(-0.3,0.7),xlim=c(-0.1,0.5),
     xlab="PC1",ylab="PC2",pch=19,cex=1,lty='solid',lwd=2,col=Cluster)
text(sel.dat[,c(1,2)],labels=rownames(sel.dat),cex=1,pos=3,col=Cluster)
legend('topright',legend=c("Group A","Group B","Group C"),col=c("red","blue","dark green"),pch=19,
       cex=1,bty="y")
dev.off()

# Apply k-means with k=3
z<-sel.dat
k <- kmeans(z, 3, nstart=10, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(z, col=Cluster, pch=16)
with(z, text(z, labels = row.names(z), pos = 2))
dev.off()
