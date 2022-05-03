#   This R script runs a hierachical cluster analysis of an antisaccade task 

#  Copyright (C) April 2020, last modified April 2022
#   J.Waldthaler
#   University Hospital of Gießen and Marburg
#
#   This software may be used, copied, or redistributed as long as it is
#   not sold and this copyright notice is reproduced on each copy made.
#   This routine is provided as is without any express or implied
#   warranties whatsoever.

library(cluster)

# load data
setwd("~/Desktop")
filename <- paste(".csv")
df <- read.csv(filename)

#scale each variable to have a mean of 0 and sd of 1
df <- scale(df)

# use Gower dissimilarity because of missing values in the dataset 
daisy_output <- daisy(df, metric = "gower")

#perform hierarchical clustering using Ward's minimum variance
clust <- agnes(daisy_output, method = "ward")

#produce dendrogram for overview 
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 

