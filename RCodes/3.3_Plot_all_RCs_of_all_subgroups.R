

VariableList <- list(c("W60","G42","KY_P","KY_T","Pumping"),c("W63","G42","KY_P","KY_T","Pumping"),
                     c("W67","G45","KY_P","KY_T","Pumping"),c("W70","G40","CDY_P","CDY_T","Pumping"),
                     c("W73","G40","CDY_P","CDY_T","Pumping"),c("W74","G42","GO_P","GO_T","Pumping"),
                     c("W78","G49","GI_P","GI_T","Pumping"),c("W80","G42","GO_P","GO_T","Pumping"),
                     c("W81","G49","GI_P","GI_T","Pumping"),c("W115","G53","GI_P","GI_T","Pumping"),
                     c("W116","G53","GI_P","GI_T","Pumping"),c("W118","G65","DC_P","DC_T","Pumping"))




for(l in 1:length(VariableList)){  
        filepath <-Sys.glob(path=paste0("Approach_II/MSSA_Work/",wellNo[l],"_RC*.csv")) 
        for (i in 1:length(filepath)) {
                assign(paste0("F",i),read.csv(file=paste0("Approach_II/MSSA_Work/",wellNo[l],"_RC",
                                                          sprintf("%02d", as.numeric(i)),".csv"),header = TRUE))    
        }
        
        for (i in 1:5){  
                windows(width=100, height=50)
                par(mar = c(4, 4, 2, 2.5))
                par(mfrow=c(4,3))
                W.ts <- ts(Data[,i],start=c(2000,1),end=c(2018,12),frequency = 12)
                plot(W.ts,type ="l",ylab = "Scores",xlab ="Units",col="black",main=paste0("Original ",VariableList[[l]][[i]]))
                grid()
                for(k in 1:10){
                        assign(paste0("F",k,".ts"),ts(get(paste0("F",k))[i+1],start=c(2000,1),end=c(2018,12),frequency = 12))
                        plot(get(paste0("F",k,".ts")),type ="l",ylab = "Units",xlab ="Year",col="black",
                             main=paste0(VariableList[[l]][[i]],paste0("(F",k,")")))
                        grid()
                        
                        
                }
                dev.copy(png,file= paste0("Approach_II/MSSA_Work/RC_Images/",wellNo[l],"_",VariableList[[l]][[i]],".png"))
                dev.off() 
        }
           
        
}



