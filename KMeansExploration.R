#Identifying clusters in edge data

#Load libraries and set wd

library(tidyverse)
library(tidylog)  #more verbose tidyverse
library(magrittr)
library(factoextra) #K clustering
library(ggplot2)
library(ggfortify) #Prettier PCA
library(psych)  #Decent, generic pairs plot 
library(GGally) #For customizable pairs plot

setwd("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/EdgeClusters")

#Load data

df <- read_csv('Metrics_Filtered_Organized_20210602.csv') 

summary(df)

#Many of the variables are out of scale with each other. We'll fix this by multiplying by a scaler

#This code worked for the original document. Updating it for the new version
# df %<>% mutate(den_m2 = den_m2/100,
#                vert.sd_m =  vert.sd_m/10,
#                VAI = VAI/10,
#                maxZ_m = maxZ_m/100,
#                max.canopy.ht_m = max.canopy.ht_m/100,
#                mean.max.canopy.ht_m = mean.max.canopy.ht_m/100,
#                q0_m = q0_m/10,
#                q25_m = q25_m/100,
#                q50_m = q50_m/100,
#                q75_m = q75_m/100,
#                q100_m = q100_m/100)

#New file

df %<>% mutate(vert.sd =  vert.sd/10,
              sd.sd = sd.sd/10,
              maxZ_ft = maxZ_ft/100,
              rumple = rumple/10,
              top.rugosity = top.rugosity/10,
              max.canopy.ht_ft = max.canopy.ht_ft/100,
              mean.max.canopy.ht_ft = mean.max.canopy.ht_ft/100,
              q0 = q0/10,
              q25 = q25/10,
              q50 = q50/100,
              q75 = q75/100,
              q100 = q100/100)

summary(df) #Better!

#Cluster analysis

#First let's visualize the data (from https://uc-r.github.io/kmeans_clustering)

distance <- get_dist(df[,7:20]) #Calculate Euclidean distance from factoextra package
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#That's a hot mess (too many rows), but there does seem to be some clustering in the dataset.


#Figure out optimal number of clusters (from https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/)

#We'll compare several methods to look for consensus

#This uses the elbow method to determine number of clusters
fviz_nbclust(df[,c()], FUNcluster = kmeans, method = 'wss') +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method") # 4 clusters

# Silhouette method
fviz_nbclust(df[,7:20], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method") #2 clusters

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(df[,7:20], kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method") #10 (probably doesn't resolve)

#These don't seem to be clustering well. 

#Let's just visualize with a PCA

#Drop some unnecessary variables

df2 <- df[,-c(1:2,5:7,16:18,20)]

colnames(df2) #better

#visualize relationships
pairs.panels(df2[,3:18])

#Many of the variables are strongly and linearly correlated. It may be appropriate
#to drop some of these variables.

PCA <-prcomp(df2[,3:18])

biplot(PCA, choices = c(1,2)) #First two axes
biplot(PCA, choices = c(2,3)) #second and third axes

#PCA looks legit, no weird shapes and lots of variance explained in the first axes (~86%!)

#Let's visualize it with the colors from our land use types.

autoplot(PCA, data = df2, colour = 'Land_Class',
         loadings = TRUE,
         loadings.label = TRUE, loadings.label.size = 3)


#Let's try to make a prettier version of the pairs panel.

ggpairs(df2, columns = 3:19) #Ugh. What a mess. Let's just look at the most influential ones from the PCA

ggpairs(df2, columns = c(3,7,10,12), ggplot2::aes(colour=Land_Class, alpha = 0.1))

