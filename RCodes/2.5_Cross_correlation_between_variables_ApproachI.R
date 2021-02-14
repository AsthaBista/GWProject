# Set path
setwd("C:/Users/Aastha/Desktop/GWProject")

if (!require('Rssa')) install.packages("kableExtra","dplyr","tidyr");
x <- c("kableExtra","dplyr","tidyr");
lapply(x, require, character.only = TRUE)


# Prepare data
W <- read.csv("Approach_I/GWLevels_stationary.csv", sep=",")[-1]
S <- read.csv("Approach_I/SW_stationary.csv", sep=",")[-1]
P <- read.csv("Approach_I/Precipitation_stationary.csv", sep=",")[-1]
Tm <- read.csv("Approach_I/Temperature_stationary.csv", sep=",")[-1]
Pu <- -read.csv("Data_Processing/Pumping.csv", sep=",")[-1]

#Combining all variables
All <- data.frame(W,S,P,Tm)
All$Pu <- c(unlist(Pu),rep(NA, nrow(All)-nrow(Pu)))
tbl_df(All)

#Function to find maximum correlation
Find_Max_CCF<- function(a,b)
{
  d <- ccf(a, b, plot = FALSE,na.action = na.contiguous)
  cor = d$acf[,,1]
  lag = d$lag[,,1]
  res = data.frame(cor,lag)
  res_max = res[which.max(res$cor),]
  return(res_max)
} 


#Function to find crosscorrelations for each pair
Create_crosscorrelation_table<- function(df)
{
  col<-colnames(df)
  addData<-as.data.frame(matrix(NA, ncol = 2, nrow = (ncol(All))^2 - ncol(All)))
  varnames<-data.frame()
  rownumber=1
  #Loop for selecting the first variable
  for(i in 1:ncol(df)){
    #Build time series of first variable
    a.ts<-ts(df[,i],start=c(2000,1),end=c(2018,12),frequency = 12) 
    #Loop around for selecting the second variable
    for(j in 1:ncol(df)){
      if(i!=j){             #removing selecting same variable  
        #build time series of second variable
        b.ts<-ts(df[,j],start=c(2000,1),end=c(2018,12),frequency = 12)
        #use given function to find the maximum correlation and its lag
        ccfmax<-Find_Max_CCF(a.ts,b.ts)
        addData[rownumber,1]<-ccfmax[1]  #add correlation value to first column
        addData[rownumber,2]<-ccfmax[2]  #add lag to second column
        varnames[rownumber,1]<-col[i]    #add first variable name to first column
        varnames[rownumber,2]<-col[j]    #add second variable name to second column
        rownumber<-rownumber+1   
      }
    }
  }
  new_df<-data.frame(varnames,addData)   #Combine names dataframe and values dataframe
  new_df$Days<-new_df[,4]*12             #Convert lag to months
  colnames(new_df)<-c("Var1","Var2","Cor","Lag","Months")
  return(new_df)
} 

new_df <- Create_crosscorrelation_table(All)
write.csv(new_df,"Approach_I/Cross_correlation_firstresult.csv")

## Arrange table in a horizontal form
tbl_df(new_df)
new_df<- new_df[,c(1,2,3,5)]
colnames(new_df)<- c("X","Y","Cor","Months")

df2 <- new_df %>%
  filter(grepl("W",X)) %>%        #select only wells in first column
  filter(!grepl("W",Y)) %>%       #select other variables in second column
  gather(Var, val, c(Cor, Months)) %>%   #arrange dataframe in a long form
  unite(Var1,Var, Y) %>%                 #unite headings: e.g.Cor_G40,Months_G40
  pivot_wider(names_from = Var1, values_from = val)   #arrange dataframe in a wide form

#Seoarate columns with correlation and months
cor_df<-cbind(df2[,1],round(df2[,2:26],3))
mth_df<-df2[,27:51]
#arrange the order
neworder <- order(c(2*(seq_along(cor_df[,-1]) - 1) + 1,
                    2*seq_along(mth_df)))
df3<-cbind(Wells = cor_df[,1],cbind(cor_df[,-1], mth_df)[,neworder])
colnames(df3)[1] <- "Wells"

#Group the wells into three groups
df4<-df3 %>%
  mutate(Group = ifelse(grepl("W63|W67|W78|W81|W116",Wells),"Group A",
                               ifelse(grepl("W60|W74|W80",Wells),"Group B",
                                      ifelse(grepl("W70|W73|W115|W118",Wells),"Group C","None")))) %>%
  select(Wells, Group, Cor_G40:Months_Pu) %>%
  group_by(Group) %>%
  arrange(Group)
write.csv(df4,"Approach_I/Cross_correlation_all_variables.csv")


## Arrange the table merging headers (simply for visualization in html)
dt <-df4
kbl(dt) %>%
  kable_paper() %>%
  add_header_above(c(" " = 1," " = 1, "G40" = 2,"G42" = 2,"G45"=2,"G49"=2,"G51"=2,"G53"=2,"G65"=2,
                     "RV"=2,"CDY"=2,"KY"=2,"HY"=2,"GI"=2,"GS"=2,"GO"=2,"NP"=2,"DC"=2,
                     "RV_T"=2,"CDY_T"=2,"KY_T"=2,"HY_T"=2,"GI_T"=2,"GS_T"=2,"GO_T"=2,"NP_T"=2,
                     "DC_T"=2,"Pu" = 2)) %>%
  add_header_above(c(" " = 1," " = 1, "Gauge_stations" = 14,"Precipitation" = 18,
                     "Temperature" = 18,"Pumping" = 2))


                   