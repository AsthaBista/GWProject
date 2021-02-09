###----------------------------------------------------------------------------------------------
#This code has three parts:
#Exploratory analysis of the data: Summary statistics, correlation
#Principal Component Analysis using FactoMineR package and preliminary visualization
#Scores and Loadings from PCA
#Source: http://www.sthda.com/english/wiki/print.php?id=202  
###----------------------------------------------------------------------------------------------
# Set path
setwd("C:/Users/Aastha/Desktop/GWProject")

#Install packages
list.of.packages <- c("FactoMineR","factoextra","rela","psych","corrplot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library("FactoMineR")
library("factoextra")
library(rela)
library(psych)
library(corrplot)

#Import data
GWData<-read.csv(file ="Approach_I/GWLevels_stationary.csv",header = TRUE)[,-1]  
head(GWData)

#Some extra tools for PCA
#First compute correlation matrix
GWDataCor <- cor(GWData)
GWDataCov <- cov(GWData)

# We now examine the data to assess whether the assumptions for PCA have been met 
# before proceeding. We use the paf() function from the rela package.
assumptions <- paf(as.matrix(GWData), eigcrit = 1, convcrit = .001)

# To test for the first assumption, we perform Bartlett’s Test for Sphericity.
# The null hypothesis for this test is that the intercorrelation matrix comes from a 
# noncollinear populaton or simply that there is nocollinearity between the variables, 
# which would render PCA impossible as it depends on the construction of a linear combination
# of the variables. We use a significance level α=.05.
bartlettTest <- cortest.bartlett(GWDataCor)
bartlettTest

# Bartlett test is rejected becaus p values is less than 0.01.
# Now we will extract Kaiser-Meyer-Olkin (KMO) from the assumptions object
print(assumptions$KMO)

# This value is more than 0.7, so can be acceptable. 
det(GWDataCor)
# The determinant is positive, hence it satisfies all three assumptions of PCA
# Therefore, we can proceed with PCA

# Summary of the data
summary(GWData)

##----------Correlation Matrix-------------
cor.mat <- round(cor(GWData),2)
head(cor.mat[, 1:6])

##Visualize correlation
install.packages("corrplot")
library("corrplot")
corrplot(cor.mat, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)
dev.copy(png,file = "Approach_I/GW_Wells_CorrelationPlot.png")
dev.off()

##Scatterplot matrix showing correlation coefficients and significance levels
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(GWData, histogram=TRUE, pch=19)
dev.copy(png,file = "Approach_I/GW_Wells_Scatterplot_Correlation.png")
dev.off()


#--------------Principal Component Analysis------------------------------------
GWData<-scale(GWData)     #Centre and scale dataframe
res.pca <- PCA(GWData, graph = T)   #Perform PCA

#Variances of the principal components
eigenvalues <- res.pca$eig
head(eigenvalues[, 1:3])

#Make a scree plot using base graphics
barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
lines(x = 1:nrow(eigenvalues), eigenvalues[, 2], 
      type="b", pch=19, col = "red")
dev.copy(png,file = "Approach_I/GWLevel_screeplot.png")
dev.off()

#Variables factor map : The correlation circle
head(res.pca$var$coord)    #coordinates for the variables


#Contributions of the variables to the principal components
#Variable contributions in the determination
#of a given principal component are (in percentage) :
#(var.cos2 * 100) / (total cos2 of the component)
head(res.pca$var$contrib)

#Graph of variables using FactoMineR base graph
plot(res.pca, choix = "var")
#or plot showing contribution levels
fviz_pca_var(res.pca, col.var="contrib")+ 
  scale_color_gradient2(low="white",mid="blue",high="red", midpoint=2.5)+theme_bw()

# Scores and loadings
loadings<-sweep(res.pca$var$coord,2,sqrt(res.pca$eig[1:ncol(res.pca$var$coord),1]),FUN="/")   #Scores/sqrt(eigenvalues) ---http://factominer.free.fr/question/FAQ.html
scores <- res.pca$ind$coord

write.csv(loadings,file = "Approach_I/PC_loadings.csv")
write.csv(scores,file = "Approach_I/PC_scores.csv")


