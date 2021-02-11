###---------------------------------------------------------------------------------------------------------------
###This program plots principal component scores of groundwater
######---------------------------------------------------------------------------------------------------------------

# Set path
setwd("C:/Users/Aastha/Desktop/GWProject")

# Prepare data
Scores <- read.csv(file = "Approach_I/PC_scores.csv",header = T)[-1]
head(Scores)

#Time series data for PC scores
pc1.ts<-ts(Scores[,1],start=c(2000,1),end=c(2018,12),frequency = 12) 
pc2.ts<-ts(Scores[,2],start=c(2000,1),end=c(2018,12),frequency = 12) 
pc3.ts<-ts(Scores[,3],start=c(2000,1),end=c(2018,12),frequency = 12) 

#Plot PC scores
windows(width=18, height=15)
par(mfrow=c(3,1), mar=c(0,3.5,0,1), oma=c(4,0,2,0), mgp=c(2,.6,0), cex.lab=1.1, tcl=-.3, las=1)
#Group1
ts.plot(pc1.ts,
        gpars = list(xlab="Year",ylab="Score",cex=1,xaxt="n"),ylim=c(-7,7),
        col=c("black"),lwd=c(1,2,2,2),lty=c(1,1,1,1,1,1))
abline(v=seq(2000,2019,1),lty = 6, col = "cornsilk2")
#abline(v=seq(2000,2019,1/12),lty = 3,lwd=0.01, col = "cornsilk2")
grid (NA,NULL, lty = 6, col = "cornsilk2")
text(2002.5,6.5,"PC1: 35.75% variance explained",cex=1.5,bty="n")

#Group2
ts.plot(pc2.ts,
        gpars = list(xlab="Year",ylab="Score",cex=1,xaxt="n"),ylim=c(-7,7),
        col=c("black"),lwd=c(1,2,2,2),lty=c(1,1,1,1,1,1))
abline(v=seq(2000,2019,1),lty = 6, col = "cornsilk2")
#abline(v=seq(2000,2019,1/12),lty = 3,lwd=0.01, col = "cornsilk2")
grid (NA,NULL, lty = 6, col = "cornsilk2")
text(2002.5,6.6,"PC2: 17.30% variance explained",cex=1.5,bty="n")

#Group3
ts.plot(pc3.ts,
        gpars = list(xlab="Year",ylab="Score",cex=1,xaxt="n"),ylim=c(-7,7),
        col=c("black"),lwd=c(1,2,2,2),lty=c(1,1,1,1,1,1))
abline(v=seq(2000,2019,1),lty = 6, col = "cornsilk2")
#abline(v=seq(2000,2019,1/12),lty = 3,lwd=0.01, col = "cornsilk2")
grid (NA,NULL, lty = 6, col = "cornsilk2")
text(2002.5,6.6,"PC3: 16.32% variance explained",cex=1.5,bty="n")

year.text=c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013",
            "2014","2015","2016","2017","2018","2019")
axis(1,at=seq(2000,2019,1),labels=year.text)
title(xlab="Year", outer=TRUE)

dev.copy2pdf(file = 'Approach_I/Plot_of_PC_scores.pdf')
dev.off()

###---------------------------------------------------------------------------------------------------------------
###This program plots PCs with wells, and PCs with other variables
###---------------------------------------------------------------------------------------------------------------

W <- scale(read.csv("Approach_I/GWLevels_stationary.csv", sep=","))


#PC time series
pc1.ts<-ts(Scores[,1],start=c(2000,1),end=c(2018,12),frequency = 12)
pc2.ts<-ts(Scores[,2],start=c(2000,1),end=c(2018,12),frequency = 12)
pc3.ts<-ts(Scores[,3],start=c(2000,1),end=c(2018,12),frequency = 12)

head(W)
#Wells time series
w60.ts<-ts(W[,2],start=c(2000,1),end=c(2018,12),frequency = 12)
w63.ts<-ts(W[,3],start=c(2000,1),end=c(2018,12),frequency = 12) 
w67.ts<-ts(W[,4],start=c(2000,1),end=c(2018,12),frequency = 12) 
w70.ts<-ts(W[,5],start=c(2000,1),end=c(2018,12),frequency = 12) 
w73.ts<-ts(W[,6],start=c(2000,1),end=c(2018,12),frequency = 12) 
w74.ts<-ts(W[,7],start=c(2000,1),end=c(2018,12),frequency = 12) 
w78.ts<-ts(W[,8],start=c(2000,1),end=c(2018,12),frequency = 12) 
w80.ts<-ts(W[,9],start=c(2000,1),end=c(2018,12),frequency = 12) 
w81.ts<-ts(W[,10],start=c(2000,1),end=c(2018,12),frequency = 12) 
w115.ts<-ts(W[,11],start=c(2000,1),end=c(2018,12),frequency = 12) 
w116.ts<-ts(W[,12],start=c(2000,1),end=c(2018,12),frequency = 12) 
w118.ts<-ts(W[,13],start=c(2000,1),end=c(2018,12),frequency = 12) 


#####-------------------------------------PCs and groups of wells plot------------------------------------------------------------------------------
windows(width=30, height=20)
par(mfrow=c(3,1), mar=c(0,3.5,0,3), oma=c(4,2,2,0), mgp=c(2,.6,0), cex.lab=1.5, tcl=-.3, las=1)

#PC1
ts.plot(pc1.ts,w63.ts,w81.ts,w78.ts,
        gpars = list(xlab=" ",ylab=" ",cex=1,xaxt="n",yaxt ="n"),ylim=c(-6,6),
        col=c("red","gray","gold4","darkseagreen"),lwd=c(2,2,2,2),lty=c(1,1,1,1))
axis(2,cex.axis=1.2)

abline(v=seq(2000,2019,1),lty = 6, col = "cornsilk2")
grid (NA,NULL, lty = 6, col = "cornsilk2")
legend('topleft',horiz = TRUE,legend=c("PC1","W63","W81","W78"),
       col=c("red","gray","gold4","darkseagreen"),lwd=c(2,2,2,2),lty=c(1,1,1,1),cex=1.5,bty="y")
text(2011,5.5,"PC1 with Group A wells",cex=1.5,bty="n",font=1)



#PC2
ts.plot(pc2.ts,w60.ts,w74.ts,w80.ts,
        gpars = list(xlab=" ",ylab=" ",xaxt="n",yaxt ="n"),ylim=c(-6,6),
        col=c("red","gray","gold4","darkseagreen"),lwd=c(2,2,2,2),lty=c(1,1,1,1))

abline(v=seq(2000,2019,1),lty = 6, col = "cornsilk2")
grid (NA,NULL, lty = 6, col = "cornsilk2")
legend('topleft',horiz = TRUE,legend=c("PC2","W60","W74","W80"),
       col=c("red","gray","gold4","darkseagreen"),lwd=c(2,2,2,2),lty=c(1,1,1,1),cex=1.5,bty="y")
text(2011,5.5,"PC2 with Group B wells",cex=1.5,bty="n",font=1)
axis(2,cex.axis=1.2)

#PC3
ts.plot(pc3.ts,w70.ts,w73.ts,w115.ts,
        gpars = list(xlab=" ",ylab=" ",cex=1,xaxt="n",yaxt ="n"),ylim=c(-6,6),
        col=c("red","gray","gold4","darkseagreen"),lwd=c(2,2,2,2),lty=c(1,1,1,1))

abline(v=seq(2000,2019,1),lty = 6, col = "cornsilk2")
grid (NA,NULL, lty = 6, col = "cornsilk2")
legend('topleft',horiz = TRUE,legend=c("PC3","W70","W73","W115"),
       col=c("red","gray","gold4","darkseagreen"),lwd=c(2,2,2,2),lty=c(1,1,1,1),cex=1.5,bty="y")
text(2011,5.5,"PC3 with Group C wells",cex=1.5,bty="n",font=1)
axis(2,cex.axis=1.2)

year.text=c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013",
            "2014","2015","2016","2017","2018","2019")
axis(1,at=seq(2000,2019,1),labels=year.text,cex.axis = 1.2)

mtext("Standardized units",side = 2,outer = TRUE,las = 0,cex = 1)
mtext("Year",side = 1,outer = TRUE,line = 2,cex = 1)


dev.copy2pdf(file = 'Approach_I/Plot_of_PC_scores_with_wells.pdf')
dev.off()

