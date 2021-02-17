# Understanding the spatiotemporal variation of groundwater level dynamics using data mining approaches
## Abstract
Rapid population growth and climate variability exert pressure on groundwater supplies, especially in regions dominated by agriculture augmented with groundwater irrigation. Effective management practices require a better understanding of groundwater dynamics and their contributing factors. Although groundwater models can provide valuable insights into such dynamics, the models are often unavailable, cost prohibitive to develop, or require extensive resources to maintain. The purpose of this study is to use a data-driven approach to examine the spatiotemporal groundwater dynamics based on groundwater level monitoring records. Using the Platte River Basin in Nebraska as the study area, we employed two different data mining approaches to statistically analyze the relationships between groundwater level fluctuation and potential explanatory variables (e.g. precipitation, pumping, and streamflow). In the first approach, principal component analysis and cluster analysis were used to partition the wells into three different groups. A cross-correlation analysis was then used to associate the clusters with explanatory variables. In the second approach, multivariate singular spectrum analysis was used to decompose the time series of wells into two components, long-term and seasonal oscillatory signals. Then, groundwater wells were grouped into three clusters based on the MSSA signals and a cluster analysis. We found that the first approach is useful for understanding the temporal patterns of the groundwater levels, and the second approach is suitable for identifying the dominant factors affecting groundwater level dynamics. This study shows that data mining is a cost-effective tool for understanding the groundwater systems in areas with a groundwater monitoring network. 
Keywords:  Groundwater management; Data mining; PCA; MSSA; Cluster analysis; Platte River

## Contents
This project package consists of all the methods that were used. The methods is comprised mainly of three parts:
* Data processing
  + contains imputation of data
  + visualization of data
* Approach I
  + Principal component analysis
  + Cluster analysis
  + Visualization and comparison
  + Cross correlation between variables
* Approach II
  + Preparation of subgroup of variables
  + MSSA operation of subgroups
  + Cluster analysis
  + Visualization

Each of these methods are explained in their respective .md files. All the R files are in the folder "RCodes".
