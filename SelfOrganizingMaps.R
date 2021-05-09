#########################################################
#                                                       #
#        Self Organizing Maps - April 2020              #
#           Data mining and exploration                 #
#                                                       #
#  Written by: Marco Pizzolato                          #
#              pizzolatomarco@yahoo.it                  #
#                                                       #
#########################################################


###   CLEAN THE ENVIRONMENT ----

#rm(list = ls()) 

###   SET WD ----

#Set the w directory
setwd("D:/../../..")
#get the wd
getwd()


#load the libraries
###  LOAD PACKAGES  ### ----

#load the libraries

library(kohonen)
library(ggplot2)
library(ggmap)
library(rgdal)
library(gridExtra)
library(grid)
library(rgeos)
library(maptools)
library(broom)
library(dplyr)
library(data.table)
library(tidyr)
library(MASS)

###  VARIBLES ### ----

var_train <- c("DataZone","IncRate","EmpRate",
               "HlthCIF","HlthAlcSR","HlthDrugSR","HlthSMR","HlthDprsPc","HlthLBWTPc","HlthEmrgSR",
               "EduAttend","EduAttain","EduNoQuals","EduNEET","EduHESA",
               "GAccPetrol","GAccDTGP","GAccDTPost","GAccDTPsch","GAccDTRet","GAccDTSsch","GAccPTGP","GAccPTPost","GAccPTRet",
               "CrimeRate",
               "HouseOCrat","HouseNCrat")

var_fin <- c("DataZone","IncRate","HlthAlcSR","HlthSMR","HlthDprsPc","HlthLBWTPc","HlthEmrgSR",
               "EduAttend","EduNoQuals","EduNEET","EduHESA",
               "GAccPTGP","GAccPTPost","GAccPTRet",
               "CrimeRate",
               "HouseOCrat","HouseNCrat")

var_train_som <- c("IncRate","EmpRate",
               "HlthCIF","HlthAlcSR","HlthDrugSR","HlthSMR","HlthDprsPc","HlthLBWTPc","HlthEmrgSR",
               "EduAttend","EduAttain","EduNoQuals","EduNEET","EduHESA",
               "GAccPetrol","GAccDTGP","GAccDTPost","GAccDTPsch","GAccDTRet","GAccDTSsch","GAccPTGP","GAccPTPost","GAccPTRet",
               "CrimeRate",
               "HouseOCrat","HouseNCrat")

var_fin_som <- c("IncRate","HlthAlcSR","HlthSMR","HlthDprsPc","HlthLBWTPc","HlthEmrgSR",
             "EduAttend","EduNoQuals","EduNEET","EduHESA",
             "GAccPTGP","GAccPTPost","GAccPTRet",
             "CrimeRate",
             "HouseOCrat","HouseNCrat")

###  LOAD COLOR PALLETTE  ### ----

source('coolBlueHotRed.R')

clus_pal <- c("#F7FA8A", '#F3BA55', '#B6D5D8', '#F1A1B1', '#6B8EC0','#fb484f', '#809B5B', '#C3C4C0', '#dad5d1')

###  LOAD DATA  ### ----
# load SIMD data
data <- read.csv(file="data/SIMD_2016_edimburgh.csv", sep=";", header = TRUE)

# load normalized ranking data for Edinburgh
rank_edi <- read.csv(file="data/SIMD_2016_edi_ranks.csv", sep=",", header = TRUE)

## Dataset for the Training ##
data_int <- data[, match(var_train,names(data) )]
View(data_int)
head(data_int)
str(data_int)

## Dataset for the the analysis ## 16 Var
data_fin <- data[, match(var_fin,names(data) )]
View(data_fin)
head(data_fin)
str(data_fin)


###  LOAD SHAPEFILE  ### ----

# Load the Edinburgh Shape file
edimburgh_map <- readOGR(dsn = "shapefile", layer = "Edimburgh2", verbose = FALSE)

# Check the data #
class(data)
class(edimburgh_map)

names(data)
names(edimburgh_map)

nrow(data)
nrow(edimburgh_map)

# Check the coordinate system of the spatial polygons data frame
proj4string(edimburgh_map)

# Plot the map quickly with the standard function
plot(edimburgh_map)

# Change the projection of the shapefile
edimburgh_map <- spTransform(edimburgh_map, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

names(edimburgh_map)
nrow(edimburgh_map)
head(edimburgh_map)

# Fortify the spatial polygon to make it printable
edimburgh_fort <- fortify(edimburgh_map, region= "DataZone")
# Order to avoid weird printing
edimburgh_fort <- edimburgh_fort[order(edimburgh_fort$order),]

###  CREATE A GEOGRAPHICAL PLOT  ### ----

# test plotting functions # 
(p <- ggplot() +
   geom_polygon(data = edimburgh_fort,
                aes(x = long, y = lat, group = group),
                color = rgb(110,110,110, max = 255), 
                size = .1, 
                fill = "white")+
   theme_nothing(legend = TRUE)+
   coord_equal())

###  TRAIN THE ALGORITHM # TRAINING DATASET  ### ----

# Prepare the Dataset for the Training ##
data_train <- data_int[, match(var_train_som,names(data_int) )]

# Turn the dataset into a matrix and standardize the data using z value 
data_train_matrix <- as.matrix(scale(data_train))

# Keep the column names of data_train as names in our new matrix
names(data_train_matrix) <- names(data_train)

# Define the size and topology of the som grid
som_grid_train <- somgrid(xdim = 11, ydim=11, topo="hexagonal", neighbourhood.fct = "gaussian")

# Train the SOM model!
som_model_train <- som(data_train_matrix,
                 grid=som_grid_train,
                 rlen=800,
                 alpha=c(0.1,0.01),
                 keep.data = TRUE )

# Plot of the training progress - how the node distances have stabilized over time
# mean distance to closes codebook vector during training
plot(som_model_train, type = "changes", main="Training progress - Gaussian")

# save the image
ggsave(paste("images/Progresses_train_gauss",".jpeg", sep = ""), 
       plot = plot(som_model_train, type = "changes", main="Training progress - Gaussian"), 
       width = 19.4, height = 12.7, units = c("cm"), dpi = 500)

###  PLOTS ANALYSIS SOM RESULT # TRAINING DATASET  ### ----

## NODE COUNT ##
# "counts" -> shows the number of objects mapped to the individual units.
plot(som_model_train, type = "counts", shape="straight", main="Node Counts", palette.name=coolBlueHotRed)

# save the image
ggsave(paste("images/Node_counts_train_gauss",".jpeg", sep = ""), 
       plot = plot(som_model_train, type = "counts", shape="straight", main="Node Counts", palette.name=coolBlueHotRed), 
       width = 19.4, height = 12.7, units = c("cm"), dpi = 500)

## NODE QUALITY ##
# "quality" -> Shows the mean distance of objects mapped to a unit to the codebook vector of that unit.
# The smaller the distances, the better the objects are represented by the codebook vectors.
plot(som_model_train, type = "quality", shape="straight", main="Node Quality/Distance", palette.name=coolBlueHotRed)

# save the image
ggsave(paste("images/Node_quality_train_gauss",".jpeg", sep = ""), 
       plot(som_model_train, type = "quality", shape="straight", main="Node Quality/Distance", palette.name=coolBlueHotRed), 
       width = 19.4, height = 12.7, units = c("cm"), dpi = 500)

## NEIGHB DIST ##
# "distance" -> Shows the sum of the distances to all immediate neighbors.
# This kind of visualization is also known as a U-matrix plot.
# Units near a class boundary can be expected to have higher average distances to their neighbors. 
plot(som_model_train, type="dist.neighbours", shape="straight", main = "SOM neighbour distances", palette.name=coolBlueHotRed)

# save the image
ggsave(paste("images/Node_dist_train",".jpeg", sep = ""), 
       plot(som_model_train, type="dist.neighbours", shape="straight", main = "SOM neighbour distances", palette.name=coolBlueHotRed), 
       width = 19.4, height = 12.7, units = c("cm"), dpi = 500)

## PIE CHART ## 
# prevalence of one variable over the other
plot(som_model_train, type = "codes")

## PLOT ALL THE VARIABES ###

# SOM gris are placed in a grid
par(mfrow = c(5,6))
for (i in 1:26) {
  plot(som_model_train, type = "property", property = getCodes(som_model_train)[,i],
       main=colnames(getCodes(som_model_train))[i], palette.name=coolBlueHotRed)
} 

# No automatic save (!)
dev.off()

###  TRAIN THE ALGORITHM # 16 VARIBLES  ### ----

# Prepare the Dataset for the Training ##
data_train2 <- data_fin[, match(var_fin_som,names(data_fin) )]

# Turn the dataset into a matrix and standardise the data uing z value 
data_train_matrix2 <- as.matrix(scale(data_train2))

# Keep the column names of data_train as names in our new matrix
names(data_train_matrix2) <- names(data_train2)

# Define the size and topology of the SOM grid
som_grid_fin <- somgrid(xdim = 11, ydim=11, topo="hexagonal", neighbourhood.fct = "gaussian")

# Train the SOM model!
som_model_fin <- som(data_train_matrix2,
                       grid=som_grid_fin,
                       rlen=800,
                       alpha=c(0.1,0.01),
                       keep.data = TRUE )

# Plot of the training progress - how the node distances have stabilised over time
# mean distance to closes codebook vector during training
plot(som_model_fin, type = "changes")

ggsave(paste("images/Training_progress_fin",".jpeg", sep = ""),
       plot(som_model_fin, type = "changes"),
       width = 19.4, height = 12.7, units = c("cm"), dpi = 500)

###  PLOTS ANALYSIS SOM RESULT # 16 VARIBLES  ### ----

## NODE COUNT ##
# "counts" -> shows the number of objects mapped to the individual units.
# Empty units are depicted in gray.
plot(som_model_fin, type = "counts", shape="straight", main="Node Counts", palette.name=coolBlueHotRed)

# save the image
ggsave(paste("images/Node_counts_fin",".jpeg", sep = ""), 
       plot(som_model_fin, type = "counts", shape="straight", main="Node Counts", palette.name=coolBlueHotRed), 
       width = 19.4, height = 12.7, units = c("cm"), dpi = 500)

## NODE QUALITY ##
# "quality" -> Shows the mean distance of objects mapped to a unit to the codebook vector of that unit.
# The smaller the distances, the better the objects are represented by the codebook vectors.
plot(som_model_fin, type = "quality", shape="straight", main="Node Quality/Distance", palette.name=coolBlueHotRed)

# save the image
ggsave(paste("images/Node_quality_fin",".jpeg", sep = ""), 
       plot(som_model_fin, type = "quality", shape="straight", main="Node Quality/Distance", palette.name=coolBlueHotRed), 
       width = 19.4, height = 12.7, units = c("cm"), dpi = 500)

## NEIGHB DIST ##
# "distance" -> Shows the sum of the distances to all immediate neighbors.
# This kind of visualization is also known as a U-matrix plot.
# Units near a class boundary can be expected to have higher average distances to their neighbors. 
plot(som_model_fin, type="dist.neighbours", shape="straight", main = "SOM neighbour distances", palette.name=coolBlueHotRed)

# save the image
ggsave(paste("images/Node_dist_fin",".jpeg", sep = ""), 
       plot(som_model_fin, type="dist.neighbours", shape="straight", main = "SOM neighbour distances", palette.name=coolBlueHotRed), 
       width = 19.4, height = 12.7, units = c("cm"), dpi = 500)

## PIE CHART ## 
# prevalence of one variable over the other
plot(som_model_fin, type = "codes", shape="straight")

## PLOT ALL THE VARIABES ###

# SOM gris are placed in a grid
par(mfrow = c(4,4))
for (i in 1:26) {
  plot(som_model_fin, type = "property", property = getCodes(som_model_fin)[,i],
       main=colnames(getCodes(som_model_fin))[i], palette.name=coolBlueHotRed)
} 

# no automatic export (!)
dev.off()


###  CREATE METRICS AND CLUSTERS # 16 VARIBLES  ### ----

# Show the WCSS metric for k-means for different clustering sizes.
# Can be used as a "rough" indicator of the ideal number of clusters
mydata <- getCodes(som_model_fin)

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)

# Within groups sum of squares
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)")

# save the image
ggsave(paste("images/group_sumsquare",".jpeg", sep = ""), 
       plot(1:15, wss, type="b", xlab="Number of Clusters",
            ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)"), 
       width = 19.4, height = 12.7, units = c("cm"), dpi = 500)

###  CREATE CLUSTER ON SOM  ### ----

# Use of hierarchical clustering to cluster the codebook vectors
som_model_fin_clust <- cutree(hclust(dist(getCodes(som_model_fin))), 5)

head(som_model_fin_clust)
View(som_model_fin_clust)
str(som_model_fin_clust)

# Plot divided in clusters only colors
plot(som_model_fin, type="mapping", bgcol = clus_pal[som_model_fin_clust],
     main = "Clusters", labels="", heatkey="FALSE", shape="straight")

# save the image
ggsave(paste("images/cluster_fin",".jpeg", sep = ""), 
       plot(som_model_fin, type="mapping", bgcol = clus_pal[som_model_fin_clust],
            main = "Clusters", labels="", heatkey="FALSE", shape="straight"), 
       width = 19.4, height = 12.7, units = c("cm"), dpi = 500)


# Plot divided in clusters colors plus separation line
plot(som_model_fin, type="mapping", bgcol = clus_pal[som_model_fin_clust],
     main = "Clusters", labels="", heatkey="FALSE", shape="straight")
add.cluster.boundaries(som_model_fin, som_model_fin_clust)

# save the image
ggsave(paste("images/cluster_clus_fin",".jpeg", sep = ""), 
       plot(som_model_fin, type="mapping", bgcol = clus_pal[som_model_fin_clust],
            main = "Clusters", labels="", heatkey="FALSE", shape="straight") +
            add.cluster.boundaries(som_model_fin, som_model_fin_clust),
       width = 19.4, height = 12.7, units = c("cm"), dpi = 500)


# Plot with the pie-charts
plot(som_model_fin, type="codes",codeRendering = "segments", shape="straight",
     main = "SOM custer details", palette.name=coolBlueHotRed)
	 
# save the image
ggsave(paste("images/piechart2_fin",".jpeg", sep = ""), 
       plot(som_model_fin, type="codes",codeRendering = "segments", shape="straight",
            main = "SOM custer details", palette.name=coolBlueHotRed),
       width = 19.4, height = 21, units = c("cm"), dpi = 500)

# Plot with the pie-charts and clusters
plot(som_model_fin, type="codes",codeRendering = "segments", shape="straight",
     main = "SOM neighbour distances", palette.name=coolBlueHotRed)
     add.cluster.boundaries(som_model_fin, som_model_fin_clust)

# save the image
ggsave(paste("images/piechart_clus2_fin",".jpeg", sep = ""), 
       plot(som_model_fin, type="codes",codeRendering = "segments", shape="straight",
            main = "SOM neighbour distances", palette.name=coolBlueHotRed) +
         add.cluster.boundaries(som_model_fin, som_model_fin_clust),
       width = 19.4, height = 21, units = c("cm"), dpi = 500)

###  CLUSTERS ON GEOGRAPHICAL SPACE ### ----

##create data frame of the small area id and of the cluster unit
cluster_details <- data.frame(id=data_fin$DataZone , cluster=som_model_fin_clust[som_model_fin$unit.classif])
##we can just merge our cluster details onto the fortified spatial polygon dataframe we created earlier
mappoints <- merge(edimburgh_fort, cluster_details, by ="id")


##Finally map the areas and colour by cluster
ggplot(data=mappoints, aes(x=long, y=lat, group=group, fill=factor(cluster))) +
  geom_polygon(colour="transparent") +
  #coord_equal() +
  scale_fill_manual(values = clus_pal)

# not printing it but saving the map in p for the ggsave
(p <- ggplot() +
    geom_polygon(data=mappoints,
                 aes(x = long, y = lat, group=group, fill=factor(cluster))) +
    scale_fill_manual(values = clus_pal)+
    geom_polygon(data = edimburgh_fort,
                 aes(x = long, y = lat, group = group),
                 color = rgb(110,110,110, max = 255), 
                 size = .1, 
                 fill = "transparent")+
    theme_nothing(legend = TRUE) +
    guides(fill=guide_legend(title="Cluster"))) #+
    #coord_equal())

# save the image
ggsave(paste("images/geography_of_clusters",".png", sep = ""), 
       p, 
       width = 50, height = 32.73, units = c("cm"), dpi = 800)


###  GET STATS FILE OUT ### ----

# combine the census and cluster data
temp <- merge(data_fin, cluster_details, by.x="DataZone", by.y="id")

# SCALE the variables before calculate the the stats
# (!) Mutate without the grouping is chopping the original columns (!)
temp2 <- temp %>%
         mutate_at(var_fin_som, funs(scale))

# rename noooowww !!
colnames(temp2) <- c("DataZone",
                      "Inc_S","H_Alc_S","H_SMR_S","H_Dprs_S","H_LBWT_S","H_Emrg_S","EdAtt_S","EdNoEq_S","EdNEET_S","EdHESA_S","G_GP_S","G_Post_S","G_Ret_S","Crime_S","Ho_O_S","Ho_N_S",
                     "Cluster") 
                             
# get back the original columns
temp2 <- merge(data_fin, temp2, by="DataZone")

# calculate some stats 
som_stats <-temp2%>%
            group_by(Cluster) %>% 
            mutate_at(var_fin_som, funs(mean,sd))

# merge some_stats with the datazone revised ranking
som_stats2 <- merge(som_stats, rank_edi, by="DataZone", by.y="Data_Zone")

# export the stats as a csv
write.csv(som_stats2, file = "data/SomStats.csv",row.names=FALSE)


###  GET ALSO A SHAPE FILE OUT ### ----

# delete the columns already included in the shapefile to avoid redundancy
# Shapefile does not like one column matrices, therefore let's get rid of them 
som_stats_red <- som_stats2[,-(2:33)] 

# rename the columns to make them all right for arcgis
colnames(som_stats_red) <- c("DataZone",
                             "Cluster",
                             "Inc_mn","H_Alc_mn","H_SMR_mn","H_Dprs_mn","H_LBWT_mn","H_Emrg_mn","EdAtt_mn","EdNoEq_mn","EdNEET_mn","EdHESA_mn","G_GP_mn","G_Post_mn","G_Ret_mn","Crime_mn","Ho_O_mn","Ho_N_mn",
                             "Inc_sd","H_Alc_sd","H_SMR_sd","H_Dprs_sd","H_LBWT_sd","H_Emrg_sd","EdAtt_sd","EdNoEq_sd","EdNEET_sd","EdHESA_sd","G_GP_sd","G_Post_sd","G_Ret_sd","Crime_sd","Ho_O_sd","Ho_N_sd",
                             "SIMD_rk","Inc_rk","Emp_rk","H_rk","Ed_rk","Hou_rk","Acc_rk","Crime_rk")

                         
write.csv(som_stats_red, file = "data/som_stats_red.csv",row.names=FALSE)

# Combine  original spatial polygons(which has SIMD) + CLUSTER (and cluster stats) + REVISED RANK 
som_spatial_output <- merge(edimburgh_map, som_stats_red, by.x="DataZone", by.y="DataZone")

# Export the shapefile
writeOGR(obj=som_spatial_output,
         dsn="shapefile",
         layer="som_results",
         driver="ESRI Shapefile")

		 
###  CLUSTER PROFILES ### ----

# Here is when the trickery happens

# Get in R a Pivot table done on the file SomStats.csv I exported before, file:SomStats.csv
# Pivot table of cluster number VS average value per each indicator
clust_prof <- read.csv(file="data/PivotGraph.csv", sep=",", header = TRUE)

# rename noooowww !!
colnames(clust_prof) <- c("Cluster",
                     "Inc","H_Alc","H_SMR","H_Dprs","H_LBWT","H_Emrg","Ed_Att","Ed_NoEq","Ed_NEET","Ed_HESA","G_GP","G_Post","G_Ret","Crime","Ho_O","Ho_N")

# To get the Plot I want I need to transfor the table from  WIDE table to a LONG table
clust_prof_long <- gather(clust_prof, Vari, Val, c(Inc,H_Alc,H_SMR,H_Dprs,H_LBWT,H_Emrg,Ed_Att,Ed_NoEq,Ed_NEET,Ed_HESA,G_GP,G_Post,G_Ret,Crime,Ho_O,Ho_N))

# Plot the graph
(scatter <- ggplot(clust_prof_long, aes (x=Cluster, y=Val, colour=Vari)) +
    geom_point() +
    scale_x_continuous(breaks = c(1:6)) +
    theme(legend.title=element_blank()) +
    #theme(plot.title=element_text("MadonnaMia")) +
    #theme_bw() +                    # Changing the theme to get rid of the grey background
    ylab("Normalized Values\n") +    # Changing the text of the y axis label
    xlab("\nCluster") +
    ggtitle("CLUSTER PROFILES\n") + # \n adds a blank line
    theme(plot.title = element_text(size=12, face="bold", hjust = 0.5)) +
    theme(axis.text.x=element_text(size=8),# Changing font size of axis labels
          axis.text.y=element_text(size=8),
          axis.title.x=element_text(size=10, face="plain"), # Changing font size of axis titles
          axis.title.y=element_text(size=10, face="plain"),
          axis.ticks = element_blank(), # face="plain" changes font type, could also be italic, etc
          #panel.grid.major.x=element_blank(),# Removing the grey grid lines
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          #panel.grid.major.y=element_blank(),
          plot.margin = unit(c(0.3,0.3,0.3,0.3), units = , "cm"))) # Putting a margin around the plot

ggsave(paste("images/cluster_profile_fin",".jpeg", sep = ""), plot = scatter, width = 19.4, height = 12.7, units = c("cm"), dpi = 500)

## END ##
## by Marco Pizzolato - April 2020 ##