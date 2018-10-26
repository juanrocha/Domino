### Script for reading the data
### Cascading effects paper
### by Juan Rocha
### juan.rocha@su.se


# ## clean and load packages
rm(list = ls())
set.seed(12345)
library (dplyr)
library (tidyr)
library (network)
library (sna) # load the package
library(RColorBrewer)
library(ggplot2)
library(GGally)

# for plotting
library(ggmap)
library(maptools)
library(maps)
library(mapproj)

library(grid)
library(gridExtra)

## for cleaning models
library(car)
library(broom)
library(sandwich); library(lmtest)
### ERGMS for Regime Shifts
library(ergm.count)

### Read CLD data# load CLD
dat <- read.csv2('~/Documents/JUAN/PhD-SRC/RS20+Analysis/RS_CLD_2018.csv')
dat <- dat[,-1]
dat$col <- ifelse(dat$Polarity == -1, "#FC8D62","#1F78B4")

## Updated ploting function
plotnet <- function(net, ...){
	plot.network(net, #mode='circle',
			vertex.col = alpha(net %v% 'col', 0.7),
			label = network.vertex.names(net),
			label.cex= 1,
			main = net %n% 'name', col.main = "grey11", cex.main = 1,
			vertex.border = 0,
			usecurve=T,
			vertex.cex= 1, #2 + scale(net %v% 'fb'),
			label.pos= 5,
			edge.col= alpha(net %e% 'col' , 1),
			edge.lwd =  0.1, #0.05 + net %e% 'fb',
			edge.curve = 0.01,
			displayisolates=T, pad = 1
			)
		}

# Now extract only one regime shift
# levels(rs$Regime.Shift)	# this are the RS currently on dataset
# com <- combn(levels(rs$Regime.Shift), m=2) # combinatories

## A function to create the network, add polarity and cycles / feedbacks as edge attributes
## J161105: Edge attributes are tricky to set because once the network is created the ordering of edges is lost and I cannot find the default order to set the right values. Reading the help of 'loading.attributes' I found that when declared as edgelist one can add attributes directly on the dataframe.

rs.net <- function (dat, i){
	# filter dataset
	dat <- filter(dat, Regime.Shift == levels(dat$Regime.Shift)[i],
	 Polarity == 1 | Polarity == -1) %>% unique()
	#dat <- droplevels(dat)
	# This ordering step is necessary to make sure polarity and other attributes are assigned correctly
	# dat <- dat[order(dat$Tail, partial = dat$Head, decreasing = F),]
	# tab <- table(dat$Tail, dat$Head, dat$Polarity) # tab is an array of tables
		# dimnames(tab) [[3]] "-1"   "-0.5" "0.5"  "1"
		# v <- as.matrix(tab[,,1]*-1) + as.matrix(tab[,,2])
	# build network
		rs.x <- network(select(dat, Tail, Head, Polarity, col), directed = T, ignore.eval=FALSE, matrix.type = 'edgelist')
	# add polarity to edges: Outated, the way I create the network includes already the polarities by using ignore.eval = F. Edge color is also set on the network creation line.
		# rs.x %e% 'polarity' <- dat$Polarity
		# set.edge.value(rs.x, 'polarity', value = as.matrix(v))
		# rs.x %e% 'col' <- ifelse(rs.x %e% "Polarity" == -1, 'red', 'blue')
	# add cycles to nodes and edges.
		fb.sum <- kcycle.census(rs.x, maxlen=network.size(rs.x), mode='digraph',	tabulate.by.vertex=T, cycle.comembership='sum')
		# number of cycles per nodes
		rs.x %v% 'fb' <-  diag(fb.sum$cycle.comemb)
		# number of cycles per edge
		rs.x %e% 'fb' <- as.sociomatrix(rs.x) * fb.sum$cycle.comemb
		#J161106: Note that cycle.comemb is a matrix with comemberships, so two nodes that are not connected in the network can belong to the same cycles. That's why I multiply by the adjacency matrix, to isolate only the edges. cycle.comemb can be used later for discovery of inconvenient feedbacks.
	## add vertex attributes
		rs.x %v% 'col' <- ifelse(colSums(fb.sum$cycle.count)[-1] == 0, "#E41A1C", "#8DA0CB")
		rs.x %n% 'name' <- levels(dat$Regime.Shift)[i]
	return(rs.x)
}

source('~/Dropbox/Code/JuanFunctions.R')


### Read RSDB data

rsdb <- read.csv(file = "~/Documents/Projects/Cascading Effects/161025_RegimeShiftsDatabase.CSV",
                  header = TRUE, sep = ",", stringsAsFactors = F)

cases <- read.csv(file = '~/Downloads/Regime Shifts Database Case Studies_170120.CSV', header = T, sep = ',' )

rsdb <- rsdb [-2,] # delete for know Invasive floating plants... seems to be duplicated.
rs <- as.character(rsdb[,3])

# Note that the number of RS depends of the version of the RSDB file. Current version 161025
type <- rep(0,30)
type[c(9,14)] <- 'Social'
type[c(27,22,19,18,5,1)] <- 'Terrestrial'
type[c(30, 17, 10, 23, 7, 6, 21, 28, 20, 12, 2, 3 )] <- 'Aquatic'
type[c(29, 25, 26, 24, 11)] <- 'Earth'
type[c(4, 15, 13, 16, 8)] <- 'Land-Water interface'
df.biome <- as.data.frame(cbind(rs,type))

# this function takes the original csv from the website and create edgelist of regime shfits and the variables from the database. z is the column from the original file that is to be cracked, so for example column 7 are drivers. data is to make sure you declare the file you're using, here data=data. If you want to create a matrix then use table as shown below
cracking <- function(z, data){
	y <- data.frame()
	for (i in 1:dim(data)[1]){
		x <- as.character(data[i,z])
        x <- gsub(pattern = "[\\[(].*?[\\])]",replacement = '',x=x, perl = T)
        x <- unlist(lapply(x, function(x){strsplit(x,split = ', ')}))
        x <- lapply(x, function(x){gsub('^\\s+|\\s+$', '', x)})
		if (length(unlist(x)) == 0) x <- 'none'
		y <- rbind(y,cbind(as.character(data[i,1]), unlist(x)),  deparse.level=1) #J170323: here updated to data[i,1] since now rsdb[,1]is the name, I'm working with a simplified dataset.
	}
    # y$value <- 1 # add this if you need to manipulate with tidyr
return (y) #return is a long form dataframe that can be used with ggplot2
}

# If you want to see the table
# drivers <- as.data.frame(table(cracking(z=7, data=data)))
# df <- as.data.frame(table(cracking (z=22, data = rsdb)))
# df <- df %>% filter (Freq > 0) %>%
#     select (V1, V2)
#
# ## Correct incosistent naming on the RSDB. Note it's not the regime shift name (column 3), but how they are referred to in column 22
# # The problematic names:
# # setdiff(levels(df$V2), levels(df$V1))
# levels(df$V2) [14] <- levels(df$V1) [10]
# levels(df$V2) [15] <- levels(df$V1) [6]
# levels(df$V2) [18] <- levels(df$V1) [6]
# levels(df$V2) [17] <- levels(df$V1) [10]
# levels(df$V2) [17] <- levels(df$V1) [7]
# levels(df$V2) [19] <- levels(df$V1) [24]
#
# levels(df$V2) [17] <- levels(df$V2) [20] # I don't have Desertification nor Forest to Cropland published on website!!!
#
#
# cas.rs <- network( df, directed = TRUE)
# network::delete.vertices(cas.rs, vid = 20)

## Clean the RSDB file to keep only important things:
rsdb <- select(rsdb, name = Regime.Shift.Name,
    drivers = Drivers...Key.direct.drivers,
    landuse = Drivers...Land.use,
    ecotype = Impacts...Ecosystem.type,
    ecoprocess = Impacts...Key.Ecosystem.Processes,
    biodiversity = Impacts...Biodiversity,
    prov_service = Impacts...Provisioning.services,
    reg_service = Impacts...Regulating.services,
    cult_service = Impacts...Cultural.services,
    hwb = Impacts...Human.Well.being,
    space_scale = Key.Attributes...Typical.spatial.scale,
    time_scale = Key.Attributes...Typical.time.scale,
    reversibility = Key.Attributes...Reversibility,
    evidence = Key.Attributes...Evidence,
    conf_existence = Key.Attributes...Confidence..Existence.of.RS,
    conf_mechanism = Key.Attributes...Confidence..Mechanism.underlying.RS,
    links = Links.to.other.regime.shifts)

# I need to manually add desertification:
rsdb[31, 1] <- "Desertification"
rsdb[31,2] <- "Vegetation conversion and habitat fragmentation, Harvest and resource consumption, Infrastructure development (e.g. roads, pipelines, Soil erosion & land degradation, Environmental shocks (e.g. fire, floods, droughts), Global climate change"
rsdb[31,3] <- "Small-scale subsistence crop cultivation, Large-scale commercial crop cultivation, Extensive livestock production (natural rangelands)"
rsdb[31,4] <- "Drylands & deserts (below ~500mm rainfall/year), Grasslands"
rsdb[31,5] <- "Soil formation, Primary production, Water cycling"
rsdb[31,6] <- "Biodiversity"
rsdb[31,7] <- "Freshwater, Food Crops, Livestock, Wild animal and plant products, Woodfuel"
rsdb[31,8] <- "Air quality regulation, Climate regulation, Regulation of soil erosion"
rsdb[31,9] <- "Knowledge and educational values"
rsdb[31,10] <- "Food and nutrition, Livelihoods and economic activity, Cultural, aesthetic and recreational values, Social conflict"
rsdb[31,11] <- "Local/landscape (e.g. lake, catchment, community)"
rsdb[31,12] <- "Years, Decades"
rsdb[31,13] <- "Hysteretic"
rsdb[31,14] <- "Models, Paleo-observation, Contemporary observations"
rsdb[31,15] <- "Well established – Wide agreement in the literature that the RS exists"
rsdb[31,16] <- "Contested – Multiple proposed mechanisms, reasonable evidence both for and against different mechanisms"
rsdb[31,17] <- "Impoundments, Shrub encroachment, Monsoon circulation, Forest to Savannas"

# correct names in RSDB to match the ones on networks (shorten)
rsdb$name[6] <- 'Marine foodwebs'
rsdb$name[10] <- 'Marine eutrophication'
rsdb$name[11] <- 'WAIS'
rsdb$name[21] <- 'Bivalves collapse'
rsdb$name[22] <- 'Bush encroachment'
rsdb$name[5] <- "Coniferous to deciduous forest"
rsdb$name[17] <- "Floating plants"
rsdb$name[28] <- "Coral transitions"
rsdb$name[19] <- "Forest to savanna"
rsdb$name[30] <- "Freshwater eutrophication"
rsdb$name[25] <- 'Greenland Ice Sheet collapse'
rsdb$name[20] <- "Kelps transitions"
rsdb$name[29] <- "Moonson"
rsdb$name[2] <- "Primary production Arctic Ocean"
rsdb$name[16] <- "Salt marshes to tidal flats"
rsdb$name[4] <- "Thermokarst lakes"
rsdb$name[18] <- "Tundra to forest"
rsdb$name[15] <- "River channel change"
rsdb$name[14] <- "Sprawling vs compact city"

# rsdb$name
# levels(dat$Regime.Shift)[!levels(dat$Regime.Shift) %in% rsdb$name]
rsdb <- rsdb[order(rsdb$name), ]
rsdb <- rsdb[-5, ]
rsdb <- rsdb[, -17 ]
