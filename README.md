# Self Organizing Maps

This project uses a machine learning methodology to explore spatial data and test the efficacy of the Scottish Index of Multi Deprivation (SIMD) (Scottish Government, 2016).

## Background

The Self Organizing Maps (SOM) is an algorithm belonging to the broad family of the Artificial Neural Networks. Given a dataset
with multiple variables and observations it performs both (1) clustering and (2) dimensional reduction (Skupin and Agarwal, 2008).  
* Clustering means that the SOM algorithm evaluates the similarities and the differences between the observations, grouping together the ones that are more similar.  
* The dimensional reduction takes place when the SOM represents high dimensional data (many variables) in a 2-dimensional space, consisting of a grid of cells. These characteristics make the SOM a very good tool for initial data-mining exploration (Vesanto and Alhoniemi, 2000).

## Folders content

__data:__ source data and tabular outputs;  
__images:__ graphics output;  
__shapefile:__ source spatial file (.shp) and .shp outputs.  
  
The dataset is not the original one downloadable from the Scottish Government website (http://www.gov.scot/Topics/Statistics/SIMD). It has been cleaned and tailored on the Edinburgh Local Authority area.   


## Requirements

Built With: **[R](https://www.rstudio.com/)**   

Packages to install:

```
install.packages(“kohonen”)
install.packages(“ggplot2”)
install.packages(“ggmap”)
install.packages(“rgdal”)
install.packages(“gridExtra”)
install.packages(“grid”)
install.packages(“rgeos”)
install.packages(“maptools”)
install.packages(“broom”)
install.packages(“dplyr”)
install.packages(“data.table”)
install.packages(“tidyr”)
install.packages(“MASS”)
```

## References

\- Scottish Government (2016) ‘The Scottish Index of Multiple Deprivation (SIMD)’, pp. 1–20.  
\- Skupin, A. and Agarwal, P. (2008) ‘Introduction: What is a Self-Organizing Map?’, Self-Organising Maps:
  Applications in Geographic Information Science, pp. 1–20.  
\- Vesanto, J. and Alhoniemi, E. (2000) ‘Clustering of the self-organizing map’, IEEE Transactions on Neural
  Networks, 11(3), pp. 586–600.  
\- Creator Marco Pizzolato - This project is licensed under the MIT License   