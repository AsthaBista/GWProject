## Putting together seasonal series for all wells
seasonalmerge <-as.data.frame(matrix(NA))
for (i in 1:length(wellNo)) {
        filepath <-Sys.glob(path=paste0("Approach_II/MSSA_Work/",wellNo[i],"_RC01.csv"))
        file <- read.csv(file=filepath,header = TRUE)[,2]
        seasonalmerge <- cbind(seasonalmerge,file)
}
seasonalmerge <- seasonalmerge[-1]
colnames(seasonalmerge) <- wellNo
write.csv(seasonalmerge,"Approach_II/Seasonal_RC_of_groundwater.csv")

## Putting together decadal series for all wells
decadalmerge <-as.data.frame(matrix(NA))
for (i in 1:length(wellNo)) {
        filepath <-Sys.glob(path=paste0("Approach_II/MSSA_Work/",wellNo[i],"_RC02.csv"))
        file <- read.csv(file=filepath,header = TRUE)[,2]
        decadalmerge <- cbind(decadalmerge,file)
}
decadalmerge <- decadalmerge[-1]
colnames(decadalmerge) <- wellNo
write.csv(decadalmerge,"Approach_II/Decadal_RC_of_groundwater.csv")



## Read original, decadal, and seasonal dataframes
OT <- scale(read.csv("Data_Processing/GWLevel_imputed.csv"),scale=F)[,-1]
DT <- scale(read.csv("Approach_II/Decadal_RC_of_groundwater.csv"),scale=F)[,-1]
ST <- scale(read.csv("Approach_II/Seasonal_RC_of_groundwater.csv"),scale=F)[,-1]


# Original time series
for(i in 1:length(wellNo)){
        assign(paste0("o","_",wellNo[i],".ts"),
               ts(OT[,i],start=c(2000,1),end=c(2018,12),frequency = 12))
        
}
# Decadal time series
for(i in 1:length(wellNo)){
    assign(paste0("d","_",wellNo[i],".ts"),
           ts(DT[,i],start=c(2000,1),end=c(2018,12),frequency = 12))
    
}
# Seasonal time series
for(i in 1:length(wellNo)){
    assign(paste0("s","_",wellNo[i],".ts"),
           ts(ST[,i],start=c(2000,1),end=c(2018,12),frequency = 12))
    
}

## Plot all decadal and seasonal based on groups of groundwater wells
windows(width=40, height=20)
par(mfcol=c(3,2), mar=c(0,3.5,0,1), oma=c(4,2,2,0), mgp=c(2,.6,0), cex.lab=1.1, tcl=-.3, las=1)

#cor long term wells

#Group1
ts.plot(d_W63.ts,d_W67.ts,d_W78.ts,d_W81.ts,d_W115.ts,d_W118.ts,
        gpars = list(xlab=" ",ylab=" ",cex=1,xaxt="n",yaxt="n"),
        col=c("blue","purple","orange","green","dark green","maroon"),lwd=c(2,2,2,2),lty=c(1,1,1,1,1,1),ylim=c(-2,2.5))
abline(v=seq(2000,2019,1),lty = 6, col = "cornsilk2")
axis(2,cex.axis=1.2)
grid (NA,NULL, lty = 6, col = "cornsilk2")
legend('topleft',horiz = TRUE,legend=c("W63","W67","W78","W81","W115","W118"),
       col=c("blue","purple","orange","green","dark green","maroon"),lwd=c(2,2,2,2),lty=c(1,1,1,1,1,1,1),cex=1.2,bty="y")
text(2018,-1.8,"Group I",cex=1.5,bty="n")

#Group2
ts.plot(d_W60.ts,d_W74.ts,d_W80.ts,
        gpars = list(xlab=" ",ylab=" ",cex=1,xaxt="n",yaxt="n"),
        col=c("maroon","green","blue","purple","blue","green"),lwd=c(2,2,2,2),lty=c(1,1,1),ylim=c(-2,2.5))
abline(v=seq(2000,2019,1),lty = 6, col = "cornsilk2")
axis(2,cex.axis=1.2)
grid (NA,NULL, lty = 6, col = "cornsilk2")
legend('topleft',horiz = TRUE,legend=c("W60","W74","W80"),
       col=c("maroon","green","blue","purple","blue","green"),lwd=c(2,2,2,2),lty=c(1,1,1),cex=1.2,bty="y")
text(2018,-1.8,"Group II",cex=1.5,bty="n")

#Group3
ts.plot(d_W70.ts,d_W73.ts,d_W116.ts,
        gpars = list(xlab=" ",ylab=" ",cex=1,xaxt="n",yaxt="n"),
        col=c("maroon","dark green","purple","orange","blue","green"),lwd=c(2,2,2,2),lty=c(1,1,1),ylim=c(-2,2.5))
abline(v=seq(2000,2019,1),lty = 6, col = "cornsilk2")
axis(2,cex.axis=1.2)
grid (NA,NULL, lty = 6, col = "cornsilk2")
legend('topleft',horiz = TRUE,legend=c("W70","W73","W116"),
       col=c("maroon","dark green","purple","orange","blue","green"),lwd=c(2,2,2,2),lty=c(1,1,1),cex=1.2,bty="y")
text(2018,-1.8,"Group III",cex=1.5,bty="n")


year.text=c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013",
            "2014","2015","2016","2017","2018","2019")
axis(1,at=seq(2000,2019,1),labels=year.text,cex.axis = 1.2)
mtext("Standardized units",side = 2,outer = TRUE,las = 0,cex = 1.2)
mtext("Year",side = 1,outer = TRUE,line = 2,cex = 1.2)


## Seasonal time series
#Group1
ts.plot(s_W63.ts,s_W67.ts,s_W78.ts,s_W81.ts,s_W115.ts,s_W118.ts,
        gpars = list(xlab=" ",ylab=" ",cex=1,xaxt="n",yaxt="n"),
        col=c("blue","purple","orange","green","dark green","maroon"),lwd=c(2,2,2,2),lty=c(1,1,1,1,1,1),ylim=c(-1.5,1.5))
abline(v=seq(2000,2019,1),lty = 6, col = "cornsilk2")
axis(2,cex.axis=1.2)
grid (NA,NULL, lty = 6, col = "cornsilk2")
legend('topleft',horiz = TRUE,legend=c("W63","W67","W78","W81","W115","W118"),
       col=c("blue","purple","orange","green","dark green","maroon"),lwd=c(2,2,2,2),lty=c(1,1,1,1,1,1,1),cex=1.2,bty="y")
text(2018,-1.3,"Group I",cex=1.5,bty="n")

#Group2
ts.plot(s_W60.ts,s_W74.ts,s_W80.ts,
        gpars = list(xlab=" ",ylab=" ",cex.axis=1.2,xaxt="n",yaxt="n"),
        col=c("maroon","green","blue","purple","blue","green"),lwd=c(2,2,2,2),lty=c(1,1,1),ylim=c(-1.5,1.5))
abline(v=seq(2000,2019,1),lty = 6, col = "cornsilk2")
axis(2,cex.axis=1.2)
grid (NA,NULL, lty = 6, col = "cornsilk2")
legend('topleft',horiz = TRUE,legend=c("W60","W74","W80"),
       col=c("maroon","green","blue","purple","blue","green"),lwd=c(2,2,2,2),lty=c(1,1,1),cex=1.2,bty="y")
text(2018,-1.3,"Group II",cex=1.5,bty="n")

#Group3
ts.plot(s_W70.ts,s_W73.ts,s_W116.ts,
        gpars = list(xlab=" ",ylab=" ",cex=1,xaxt="n",yaxt="n"),
        col=c("maroon","dark green","purple","orange","blue","green"),lwd=c(2,2,2,2),lty=c(1,1,1),ylim=c(-1.5,1.5))
abline(v=seq(2000,2019,1),lty = 6, col = "cornsilk2")
axis(2,cex.axis=1.2)
grid (NA,NULL, lty = 6, col = "cornsilk2")
legend('topleft',horiz = TRUE,legend=c("W70","W73","W116"),
       col=c("maroon","dark green","purple","orange","blue","green"),lwd=c(2,2,2,2),lty=c(1,1,1),cex=1.2,bty="y")
text(2018,-1.3,"Group III",cex=1.5,bty="n")



year.text=c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013",
            "2014","2015","2016","2017","2018","2019")
axis(1,at=seq(2000,2019,1),labels=year.text,cex.axis = 1.2)

dev.copy2pdf(file = 'Approach_II/Decadal_seasonal_oscillations_in_three_groups.pdf', width = 15, height = 8)
dev.off()


