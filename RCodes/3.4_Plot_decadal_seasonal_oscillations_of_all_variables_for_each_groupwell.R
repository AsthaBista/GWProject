# Set path
setwd("C:/Users/Aastha/Desktop/GWProject")

#Groundwater wells list
wellNo <- c("W60","W63","W67","W70","W73","W74","W78","W80","W81","W115","W116","W118")
# List of the subgroups for MSSA operation
# Each list contains a groundwater level, gauge station, weather station and pumping
variableList <- list(c("W60","G42","Kearney (KY) precipitation","Kearney (KY) temperature","Pumping"),
                     c("W63","G42","Kearney (KY) precipitation","Kearney (KY) temperature","Pumping"),
                     c("W67","G45","Kearney (KY) precipitation","Kearney (KY) temperature","Pumping"),
                     c("W70","G40","Canaday steam plant (CDY) precipitation","Canaday steam plant (CDY) temperature","Pumping"),
                     c("W73","G40","Canaday steam plant (CDY) precipitation","Canaday steam plant (CDY) temperature","Pumping"),
                     c("W74","G42","Gothenberg (GO) precipitation","Gothenberg (GO) temperature","Pumping"),
                     c("W78","G49","Grand Island (GI) precipitation","Grand Island (GI) temperature","Pumping"),
                     c("W80","G42","Gothenberg (GO) precipitation","Gothenberg (GO) temperature","Pumping"),
                     c("W81","G49","Grand Island (GI) precipitation","Grand Island (GI) temperature","Pumping"),
                     c("W115","G53","Grand Island (GI) precipitation","Grand Island (GI) temperature","Pumping"),
                     c("W116","G53","Grand Island (GI) precipitation","Grand Island (GI) temperature","Pumping"),
                     c("W118","G65","David City (DC) precipitation","David City (DC) temperature","Pumping"))

# List of seasonal and decadal variations in MSSA operations for each respective subgroup
var_explained <- list(c(36.08,33.92,35.65,37.84,38.53,36.01,38.47,37.67,38.66,37.72,37.93,38.06),
                      c(18.19,27.76,23.62,24.8,24.34,19.57,23.8,17.64,23.69,23.35,21.71,15.8))
osc <- c("Seasonal","Decadal")

list <- c("W","G","P","T","Pu")

## Loop around each well or each subgroup
for (i in 1:length(wellNo)) {
    windows(width=40, height=20)
    par(mfrow = c(1, 2))
    par(mar = c(4, 4, 2, 3))
    
    ## Loop around seasonal and decadal oscillations
    for (j in 1:2){
        # Assign filepath for each subgroup
        assign(paste0("filepath",j),Sys.glob(path=paste0("Approach_II/MSSA_Work/",wellNo[i],
                                                         "_RC",sprintf("%02d", as.numeric(j)),".csv")))
        # Read RC files for decadal and seasonal in each subgroup
        assign(paste0("F",j),read.csv(file=get(paste0("filepath",j)),header = TRUE))
        
        ## Loop around each variable 
        for (k in 1:length(list)){
            # Create time series object for each variable
            assign(paste0(list[k],".ts"),ts(get(paste0("F",j))[,k+1],start=c(2000,1),end=c(2018,12),
                                            frequency = 12))
        }
        
        ## Plot time series of all variables for decadal and seasonal separately
        ts.plot(W.ts,G.ts,P.ts,T.ts,Pu.ts,
                gpars = list(xlab="Year",ylab="Standardized units",xaxt="n",ylim=c(-2,4),
                             col=c("green","blue","purple","orange","dark green"),lty=c(1,1,2,2,2,3),lwd=c(2,2,2,2,2,3)))
        
        year.text=c("2000","2001","2002","2003","2004","2005","2006","2007",
                    "2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")
        axis(1,at=seq(2000,2019,1),labels=year.text)
        abline(v=seq(2000,2019,1),lty = 6, col = "cornsilk2")
        grid (NA,NULL, lty = 6, col = "cornsilk2")
        
        
        legend('topleft',legend=variableList[[i]],
               col=c("green","blue","purple","orange","dark green"),lty=c(1,1,2,2,2,3),lwd=c(2,2,2,2,2,3),cex=1,bty ="n")
        text(2012.5,4,paste0(osc[j]," oscillation: Explained variance = ",var_explained[[j]][[i]],"%"),cex=1,font = 1)
    }  
    # save the plot for each subgroup
    savePlot(filename= paste0("Approach_II/",wellNo[i],"_decadal_and_seasonal_oscillations.png"),type="png")
    
}
        
        
        