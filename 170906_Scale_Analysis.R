## Scale analysis
## After discussions with Steve Lade he recommended me to create a data frame or matrix dissentangling which regime shifts match or not in scale as an imput for the generalized modelling. I believe it will also be useful for the discussion of the cascading effects paper.
## by Juan Rocha
## juan.rocha@su.se

## load the same libraries as in the paper
rm(list = ls())
set.seed(12345)
library (dplyr)
library (tidyr)
library (network)
library (sna)
library(RColorBrewer)
library(ggplot2)
library(GGally)

# for plotting
library(ggmap)
library(maptools)
library(maps)
library(mapproj)

library(gridBase)
library(grid)
library(gridExtra)


## for cleaning models
library(car)
library(broom)
library(sandwich); library(lmtest)

### ERGMS for Regime Shifts
library(ergm.count)


## load the model fitted data
# this loads the data, it was cleaned and prepared with script 170329_read_data.R
# and load the ergm models fitted with script 170329_ergms.R

load('~/Documents/Projects/Cascading Effects/170525_ergm_data.RData')


# bipmat is the bipartite matrix
# out is the dataframe for domino effects.
# df_inc is the dataframe for hidden feedbacks.
