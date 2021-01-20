---
title: "Data Processing"
author: "Astha"
date: "1/18/2021"
output: html_document
---

```{r path}
setwd("D:/Work/Thesis/Work/GWProject")
```

# Data Processing
## Groundwater levels
Groundwater levels were obtained from [National Groundwater Monitoring Network](https://cida.usgs.gov/ngwmn/) for Platte River Basin in Nebraska from 2000 to 2018. 12 groundwater wells were selected based on data available, and screen depth. 
Read the data containing groundwater levels with missing values
```{r rawdata}
gwlevel <- read.csv(file ="/Data_Processing/GWLevel_with_missinggaps.csv",header = TRUE)
head(gwlevel)
```
Count the missing values
```{r na}
na_count <-sapply(gwlevel, function(y) sum(length(which(is.na(y)))))
na_count
```


## Including Plots

Since the number of missing values is less than 10%, a linear interpolation was used
Linear interpolation of each time series 
```{r}
install.packages("imputeTS")
library(imputeTS)
```

```{r}
gw_imputed<-c()
for(i in 1:ncol(gwlevel)){
        imp <- na.interpolation(gwlevel[,i],option = 'linear')
        gw_imputed<-cbind(gw_imputed,imp)
}
       
colnames(gw_imputed)<-c("W60","W63","W67","W70","W73","W74","W78","W80","W81","W115","W116","W118")
head(gw_imputed)
write.csv(gw_imputed,"GWLevel_imputed.csv")
```

```{r plot_impute,echo = FALSE}
imp <- na.interpolation(gwlevel[,10],option = 'linear')
ggplot_na_imputations(gwlevel[,10],imp)
```



