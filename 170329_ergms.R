### Script for ergm analysis
### Cascading effects paper
### by Juan Rocha
### juan.rocha@su.se
# rm(list = ls())
source('~/Documents/Projects/Cascading Effects/Domino/170329_read_data.R')


##### ERGMS for Bipartite network: Fork connections

## Setting the bipartite network
nets <- list()
bip.edgelist <- list()

for (i in 1:length(levels(dat$Regime.Shift)) ){ #length(levels(dat$Regime.Shift))
	net <- rs.net(dat = dat,  i = i)
	fb <- kcycle.census(net, maxlen = network.size(net), mode = 'digraph', tabulate.by.vertex=T, cycle.comembership = 'sum' )
	driv <- names(colSums(fb$cycle.count)[colSums(fb$cycle.count) == 0])
	rs.nam <- rep(net %n% "name", length(driv))
	df <- data.frame(drivers = driv, rs = rs.nam)
	bip.edgelist[[i]] <- df
    nets[[i]] <- net
}

bip.edgelist <- bind_rows(bip.edgelist)
bipmat <- as.matrix(table(bip.edgelist))

# library(bipartite)
# N <- nestedness(bipmat, null.models=TRUE, n.nulls=100)
# nest <- nestedrank(bipmat, method="binmatnest", normalise=T, return.matrix=F)
#
# network.layout.nest <- function (d, layout.par){
# 	y <- c(nest[[1]],nest[[2]])* -20
# 	x <- c(rep(0,dim(bipmat)[1]), rep(3,dim(bipmat)[2]))
# 	cbind(x,y) # q <- t(rbind(x,y))
# }
#
# y <- c(nest[[1]],nest[[2]])* -20
# x <- c(rep(0,dim(bipmat)[1]), rep(3,dim(bipmat)[2]))

bip1 <- network(bipmat, bipartite = T)
degbip <- sna::degree(bip1, gmode="graph")

# df <- data.frame(names = network.vertex.names(bip1))
# setwd("~/Documents/Projects/Cascading Effects")
# write.csv2(df, file = 'bip_attributes.csv')

bip_attr <- read.csv2('~/Documents/Projects/Cascading Effects/bip_attributes.csv')

colors <- c( "#0000EE" ,"#FFB90F", "#EE3B3B", "#8A2BE2", "#FF7F00", "#9ACD32")

bip1 %v% 'attr' <- as.vector(bip_attr$attribute)
bip1 %v% 'col' <- colors[factor(bip_attr$attribute)]

# op <- par()

# par(mar=c(1,1,1,1)) #, xaxs = 'i', yaxs = 'i'
# # Plotting the bipartite network
# # plot.new(asp = 1,  xlim = c(5,-2), ylim = c(2,-22), type = 'n')
# plot.network(bip1, edge.lwd=0.01, usecurve=F, edge.curve= -0.01,
#     vertex.cex= degbip*0.05 + 0.1, displayisolates=F, interactive=F,
#     mode="nest", jitter=F,  vertex.lty=1, vertex.border = 'white',
#     edge.col = "gray84", suppress.axes = T, ylim = c(-22, 2),
#     vertex.col = bip1 %v% 'col')
# #labels for drivers
# text(x,y,c(network.vertex.names(bip1)[1:dim(bipmat)[1]],rep(' ',dim(bipmat)[2])), pos=2, col='grey12', cex = 0.5)
# #labels for RS
# text(x,y,c(rep(" ", dim(bipmat)[1]),network.vertex.names(bip1)[(1 + dim(bipmat)[1]): network.size(bip1)]), pos=4, col='grey12', cex = 0.5)
# # title("b", adj = 0, cex = 0.7)


### Alternative visualiztion with circlesize
## J170323: There is errors with this alternative in the way colours are plotted. It's nicer to the eye but it's using the edge list instead of the network object and the colouring of drivers get messed up. To correct on final version.
# library(circlize)
# quartz(width=7, height =7, family = 'Helvetica', pointsize = 7)
#
# circos.par(gap.degree = c(rep(2, length(unique(bip.edgelist[[1]]))-1), 30,
#                           rep(2, length(unique(bip.edgelist[[2]]))-1), 30),
#             start.degree = 15)
#
# chordDiagram(bip.edgelist, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.3), grid.col = bip1 %v% 'col')
# #
# ## from tutorial
# circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
#     xlim = get.cell.meta.data("xlim")
#     xplot = get.cell.meta.data("xplot")
#     ylim = get.cell.meta.data("ylim")
#     sector.name = get.cell.meta.data("sector.index")
#     #if(abs(xplot[2] - xplot[1]) < 20) {
#         circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
#         niceFacing = TRUE, adj = c(0, 0.5), cex = .8)#}
#     # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
#     #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
#     # }
# }
# , bg.border = NA)
# circos.clear()
# detach(package:circlize)

### Add legends.


## Prepare the one-mode networks for ergm modeling
### The code below comes from my PlosOne analysis
## bipartite projections according with Newman 2010

#This function takes a bipartite network and return the one-mode projection as an object of the class network. It can be modified to get the adjacency matrix with weigthed paths, or the number of nodes of class 2 a dyad of class one is connected, co-occurrence.
mode.1<- function (net){
	m <- as.matrix.network (net, expland.bipartite=F)

    # First mode
    mode1 <- m %*% t(m)
	## set the diagonal to zero - the diagonal is the degree on bip mode
    d1 <- diag(mode1)
	diag(mode1)<-0
    mat1 <- mode1
    ## reduce to adjacency matrix
	mode1[mode1>1]<-1

    ## create network
	net1<- network (mode1, loops=F, dir=F, hyper=F,
					multiple=F, bipartite=F )
        net1 %v% 'deg' <- d1
		set.edge.value(net1, "paths", mat1)

    ## Second mode
	mode2 <- t(m) %*% m
    d2 <- diag(mode2)
	## set diag to zero
	diag(mode2)<-0
    mat2 <- mode2
    ## reduce to adjacency matrix
	mode2[mode2>1]<-1


	net2<- network (mode2, loops=F, dir=F, hyper=F,
					multiple=F, bipartite=F )
        net1 %v% 'deg' <- d1
		set.edge.value(net2, "paths", mat2)

	return(list(net1, net2)) #	Working!!
} #### There is an error in the function, the path attributes for driv.net is not working properly. To fix. J170308: Error corrected in new version in this script. Proof is below on commmented text, to do it you need to run the function separately - the test is the heatmap of the attribute 'path', it should be the same as the matrix. The error was that the diag on the matrix was not zero.

x <- mode.1(bip1)[[2]] # x is the regime shifts one-mode network

# gplots::heatmap.2(mat2, trace = 'none', margins = c(10,10))
# gplots::heatmap.2(as.sociomatrix(x, 'paths'), trace = 'none', margins = c(10,10))

### Declare attributes for the regime shifts one-mode network:
# reconstruct database:
# drivers
# cracking(7,rsdb) %>% spread(key = V2, value = value, fill = 0) # J170308: this option is more elegant programming wise but I need a table. If so, one needs to modify the cracking function to add a column 'value' <- 1

# cracking(8,rsdb) %>% select(-value) %>% table() %>% as.matrix() %>% dist( method="binary", upper=T, diag=T) %>% as.matrix() %>% gplots::heatmap.2(trace = 'none', margin= c(15,15))
#
# Description of the problem: When I calculate the distance between attributes in the RSDB, the result is a distance matrix. But when it's assigned to the network as an attribute in the network, it gets multiplied by the adjacency matrix, then information is lost (i believe). See the example below with land use

## J170323: partilly solved by adding desertification, and modifying the cracking function.
# x %e% 'landuse' <- cracking(3,rsdb) %>% #select(-value) %>%
# table() %>% as.matrix() %>% dist( method="binary", upper=T, diag=T) %>% as.matrix() %>% clean.and.order()
#
# cracking(3,rsdb) %>% table() %>% as.matrix() %>% dist( method="binary", upper=T, diag=T) %>% as.matrix() %>% clean.and.order() %>% image
#
# as.sociomatrix(x, 'landuse') %>% image

for (i in 2:14){
    x %e% names(rsdb)[i] <- cracking(i,rsdb) %>%
    table() %>% as.matrix() %>% dist( method="binary", upper=T, diag=T) %>% as.matrix() %>% clean.and.order()
}

## Note that the matrix is symmetrical (undirected graph)
# isSymmetric(as.sociomatrix(x))
# so you only need a triangle in your dataframe
df_test <- data.frame(paths = as.sociomatrix(x, 'paths')[upper.tri(as.sociomatrix(x))])

for(i in 2:14){
    m <- cracking(i,rsdb) %>%
    table() %>% as.matrix() %>% dist( method="binary", upper=T, diag=T) %>% as.matrix() %>% clean.and.order()

    df_test[i] <- 1 - m[upper.tri(m)]
    names(df_test)[i] <- names(rsdb)[i]
}

fit1 <- lm(paths ~ landuse + ecotype + ecoprocess + prov_service + reg_service + cult_service + hwb + space_scale + time_scale + reversibility + evidence, data = df_test)

### ERGMS for Regime Shifts
# library(ergm.count)

fit.null1 <- suppressMessages(ergm(x ~ nonzero + sum , response ='paths', reference=~Poisson, control = control.ergm(MCMLE.trustregion=1000))) ## AIC: -1258


fit.w1 <- suppressMessages(ergm(x ~ nonzero + sum + edgecov(x,'landuse') + edgecov(x,'ecotype')+
 	edgecov(x,'ecoprocess') + edgecov(x,'prov_service') +  edgecov(x,'reg_service') + edgecov(x,'cult_service') + edgecov(x,'hwb') + edgecov(x,'space_scale')+ edgecov(x,'time_scale') + edgecov(x,'reversibility') + edgecov(x,'evidence'), response ='paths', reference = ~Poisson, control = control.ergm(MCMLE.trustregion=1000)))

summary(fit1); summary(fit.null1); summary(fit.w1)
#
# library(corrgram)
# corrgram(df_test[-6], type = 'data', order= T,  lower.panel = 'panel.pts', upper.panel = 'panel.cor', diag.panel = 'panel.density')

####### Ergms for domino effects

## Domino effects will be studied with Arctic regime shifts. This is: if a driver in one regime shift is part of a feedback in another.
net.merge <- function(dat,i,j){
    # create network. Attributes are already declared when using ignore.evale = F
    df <- rbind(
        filter(dat, Regime.Shift ==
                     levels(dat$Regime.Shift)[i], Polarity == 1 | Polarity == -1),
        filter(dat, Regime.Shift==
                     levels(dat$Regime.Shift)[j], Polarity == 1 | Polarity == -1))

		rs.mix <- network(select(df, Tail, Head, Polarity, col),
                directed = T, ignore.eval = F, matrix.type = 'edgelist')
    # Add cycles to nodes and edges
		fb.sum <- kcycle.census(rs.mix, maxlen=network.size(rs.mix), mode='digraph', tabulate.by.vertex=T, cycle.comembership='sum')
        rs.mix %v% 'fb' <-  diag(fb.sum$cycle.comemb)
		rs.mix %e% 'fb' <-  as.sociomatrix(rs.mix) * fb.sum$cycle.comemb
    # name the network
        rs.mix %n% 'name' <- paste(levels(dat$Regime.Shift)[i],
			levels(dat$Regime.Shift)[j], sep=' - ')
    # add vertex attributes
        rs.mix %v% 'col' <- ifelse(colSums(fb.sum$cycle.count)[-1] == 0, "grey84", 'goldenrod2')
	return(rs.mix)

}

# subset the dominoes dataset
# dom <- filter(dat, Regime.Shift == 'Arctic Sea Ice collapse'|
#                 Regime.Shift == 'Fisheries collapse' |
#                 Regime.Shift == 'Greenland Ice Sheet collapse'|
#                 Regime.Shift == 'Marine foodwebs'|
#                 Regime.Shift == 'Thermohaline circulation' |
#                 Regime.Shift == 'Tundra to forest'|
#                 Regime.Shift == 'Steppe to Tundra' |
#                 Regime.Shift == 'Peatland transitions'
#                 )
# dom <- droplevels(dom)

# list for network outcomes
out_dom <- list()

for (i in 1:length(levels(dat$Regime.Shift))){
    net <- rs.net(dat, i)
    out_dom[[i]] <- net
}

key <- combn(seq(1, length(levels(dat$Regime.Shift))),2)


out <- list()
for (i in 1:dim(key)[2]){
    # drivers in regime shift i
    x1 <- (out_dom[[key[1,i]]] %v% 'vertex.names') [out_dom[[key[1,i]]] %v% 'col' == "#FFD92F"]
    # feedback variables in regime shift j
    y1 <- (out_dom[[key[2,i]]] %v% 'vertex.names') [out_dom[[key[2,i]]] %v% 'col' != "#FFD92F"]

    # drivers in regime shift j
    x2 <- (out_dom[[key[2,i]]] %v% 'vertex.names') [out_dom[[key[2,i]]] %v% 'col' == "#FFD92F"]
    # feedback variables in regime shift i
    y2 <- (out_dom[[key[1,i]]] %v% 'vertex.names') [out_dom[[key[1,i]]] %v% 'col' != "#FFD92F"]

    # resulting interactions
    df1 <- data.frame(Tail = out_dom[[key[2,i]]] %n% 'name',
                    Head = out_dom[[key[1,i]]] %n% 'name',
                    weight = sum(x1 %in% y1),
                    driv2feed = paste(x1[x1 %in% y1], collapse = ', ') )
    df2 <- data.frame(Tail = out_dom[[key[1,i]]] %n% 'name',
                    Head = out_dom[[key[2,i]]] %n% 'name',
                    weight = sum(x2 %in% y2),
                    driv2feed = paste(x2[x2 %in% y2], collapse = ', '))
    df <- bind_rows(df1,df2)

    out[[i]] <- df
}

out <- bind_rows(out)
# domino <- select(out, Tail, Head, weight = weight1dir)
# domino <- rbind(domino, select(out, Tail = Head, Head = Tail, weight = weigth2dir))

dom_net <- network(filter(out, weight > 0),
    directed = T, ignore.eval = FALSE, matrix.type = 'edgelist')

dom_net %v% 'indegree' <- sna::degree(dom_net, cmode = 'indegree')
dom_net %v% 'outdegree' <- sna::degree(dom_net, cmode = 'outdegree')




# ## Heatmap with ggplot
# g <- ggplot(data = out, aes( Head, Tail)) +
#     geom_tile(aes(fill = weight)) + ylab("Independent regime shift") + xlab("Dependent regime shift") + theme_minimal(base_size=6)
#
# g <- g + scale_fill_gradient2(low = "#FFA50080", mid = "gray84" ,high = "#0000FF80", midpoint = 0, na.value = "grey50")+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ))

## heatmap with heatmaply

# x <- out %>% select(Head, Tail, weight) %>%
#     spread(Tail, value = weight) %>%
#     select(-1) %>%
#     as.matrix()
# rownames(x) <- colnames(x)
#
# library(heatmaply)
# heatmaply(x, alpha = .6,
#     na.value = "grey84",
#     column_text_angle = 90,
#     margins = c(250,250,30,30),
#     xlab = "Receiver",
#     ylab = "Source",
#     main = "Domino effects",
#     )

## heatmap with d3
# library(d3heatmap)
# d3heatmap(x, scale = "none", colors = "Spectral", theme = "dark")

# ## change this figure by a heatmap.
# quartz(pointsize=5)
# plot.network(dom_net,
#     label = network.vertex.names(dom_net),
#     label.cex= 1, label.pos= 5,
#     edge.lwd = .2, #* dom_net %e% 'weight' ,
#     #edge.curve = 0.01, usecurve=F,
#     displayisolates=T, pad=1,
#     vertex.col = alpha('orange', 0.5),
#     edge.col = alpha('grey', 0.5)
#     )
# title('a', adj= 0, cex = 3)


# mutuality(dom_net)
# ## plotting parameters for disciplines
# p <- ggnet2(net = dom_net, size = 3,
#         #size.legend = "Number of papers",
#         #label.size = 2,
#         #mode = 'mds',
#         #node.color = 'orange',
#         node.color = dom_net %v% 'indegree' + 1,
#         node.alpha = 0.3,
#         edge.color = "grey10",
#         edge.alpha = scales::rescale(dom_net %e% "weight", to = c(0.25, 1)),
#         edge.size = scales::rescale(dom_net %e% "weight", to = c(0.25, 1)),
#         arrow.size = 5, arrow.gap = 0.025
#         )
#
# p + scale_fill_gradient("Indegree", high = "red", low = "yellow")
#
# p <- ggnetworkmap(net = dom_net, arrow.size = 0.2,
#     node.group = indegree,
#              ring.group = outdegree, size = 4) +
#   scale_fill_continuous("Indegree", high = "red", low = "yellow") +
#   labs(color = "Outdegree")
#
# p
# p + scale_fill_gradient("Indegree", high = "red", low = "yellow") +
#   labs(color = "Outdegree")

#### Run ergms to explain the matrix

for (i in 2:14){
    dom_net %e% names(rsdb)[i] <- cracking(i,rsdb) %>%
    table() %>% as.matrix() %>% dist( method="binary", upper=T, diag=T) %>% as.matrix() %>% clean.and.order()
}

## Note that the matrix is symmetrical (undirected graph)
# isSymmetric(as.sociomatrix(x))
# so you only need a triangle in your dataframe
df_test2 <- data.frame(paths = as.vector(as.sociomatrix(dom_net, 'weight')))

for(i in 2:14){
    m <- cracking(i,rsdb) %>%
    table() %>% as.matrix() %>% dist( method="binary", upper=T, diag=T) %>% as.matrix() %>% clean.and.order()

    df_test2[i] <- 1 - as.vector(m)
    names(df_test2)[i] <- names(rsdb)[i]
}

fit2 <- lm(paths ~ landuse + ecotype + ecoprocess + prov_service + reg_service + cult_service + hwb + space_scale + time_scale + reversibility + evidence, data = df_test2)

### ERGMS for Regime Shifts
# library(ergm.count)

fit.null2 <- suppressMessages(
    ergm(dom_net ~ nonzero + sum , response ='weight', reference=~Poisson,
         control = control.ergm(MCMLE.trustregion=1000)) ## AIC: -1258
        )

fit.w2 <- suppressMessages(
    ergm(dom_net ~ nonzero + sum + edgecov(dom_net,'landuse') +
    edgecov(dom_net,'ecotype') + edgecov(dom_net,'ecoprocess') + edgecov(dom_net,'prov_service') + edgecov(dom_net,'reg_service') + edgecov(dom_net,'cult_service') + edgecov(dom_net,'hwb') + edgecov(dom_net,'space_scale')+ edgecov(dom_net,'time_scale') + edgecov(dom_net,'reversibility') + edgecov(dom_net,'evidence'),
    response ='weight', reference = ~Poisson,
    control = control.ergm(MCMLE.trustregion=1000))
    )

summary(fit2); summary(fit.null2); summary(fit.w2)

##### Ergms for inconvenient feedbacks

# A function to merge networks based on edge lists aggregation
# Note this comes from another script and needs to be updated to the variables declared here.

# key <- combn(seq(1,5),2)
# key <- combn(seq(1, length(levels(dat$Regime.Shift))),2)

# subset dataset
# rs <- filter(dat, Regime.Shift == 'Bush encroachment'|
#                 Regime.Shift == 'Desertification' |
#                 Regime.Shift == 'Forest to savanna'|
#                 Regime.Shift == 'Steppe to Tundra'|
#                 Regime.Shift == 'Soil Salinisation'
#                 )
# rs <- droplevels(rs)


# The function below is useful when cycles numbers are not calculated. Currently I'm using cycle.comemebership='sum' which gives me the link weigth. As alternative, cycle.comembership='byblength' retunrs an array where each matrix shows cycle co-membership by length.
# source('~/Dropbox/Code/themeJuan.R')

net.fb <- function(net1, net2, net3){

	#count cycles for all networks
	x1cycle <- kcycle.census(net1, maxlen=network.size(net3), mode='digraph',
		tabulate.by.vertex=T, cycle.comembership='sum')

	x2cycle <- kcycle.census(net2, maxlen=network.size(net3), mode='digraph',
		tabulate.by.vertex=T, cycle.comembership='sum')

	x3cycle <- kcycle.census(net3, maxlen=network.size(net3), mode='digraph',
		tabulate.by.vertex=T, cycle.comembership='bylength') #cycle.comembership='sum'

	#create a matrix with results
	feed.mat <- cbind(x1cycle$cycle.count[,1], x2cycle$cycle.count[,1],
		 x3cycle$cycle.count[,1]) # feedbacks matrix
	# put some colnames
	colnames(feed.mat) <- c('RS1', 'RS2', 'RS.mix')
	#c(net1 %n% 'name', net2 %n% 'name', net3 %n% 'name')

	feed.mat <- as.data.frame(feed.mat)
	feed.mat$feed.length <- rownames(feed.mat)
	feed.mat$Inconvenient <- feed.mat$RS.mix - (feed.mat$RS1 + feed.mat$RS2)
    feed.mat$Expected <- (feed.mat$RS1 + feed.mat$RS2)

	# #put data on long format
	# library(reshape2)
	# x.long <- melt(feed.mat, id.var="feed.length",
	#  		 measure.var= colnames(feed.mat)[c(1:3,5,6)],
	#  		 value.name='value')
    #
	#  x.long$value <- as.integer(x.long$value)
	#  x.long$feed.length <- as.integer(x.long$feed.length)
    #
# 	# plot it
# 	library(ggplot2)
# 	g <- ggplot(filter(x.long, variable == 'Expected' | variable == 'Inconvenient' ), aes(x=feed.length, y=value), group=variable)
# 	g <- g + geom_bar(aes(fill=variable), position = 'stack', stat = 'identity')  +
# 		ylab('Number of feedbacks') + xlab('Feedback length') +
# 	 	ggtitle(net3 %n% 'name') + theme_bw(base_size = 7) +
# 	 	theme(legend.position=c(0.8,0.7), plot.title=element_text(size=rel(0.85))) + scale_fill_manual("Feedbacks",values=c("#377EB8CC", "#E41A1CCC")) + xlim(0, max(x.long[x.long$value > 0, ]$feed.length))
# # colors not used "#984EA3", "#4DAF4A"
	return(list(feed.mat)) #
}

# ## update plotnet to plot smaller nodes and no labels
# plotnet <- function(net, ...){
# 	plot.network(net, #mode='circle',
# 			vertex.col = alpha(net %v% 'col', 0.8),
# 			# label = network.vertex.names(net),
# 			# boxed.labels=F,
# 			# label.cex=0.5,
# 			main = net %n% 'name', col.main = "grey",
# 			vertex.border = 0,
# 			usecurve=T,
# 			vertex.cex= 1, #2 + scale(net %v% 'fb')/2,
# 			label.pos=5,
# 			edge.col= alpha(net %e% 'col' , 0.75),
# 			edge.lwd = 0.05 + net %e% 'fb',
# 			edge.curve = 0.01,
# 			displayisolates=T, pad=1
# 			)
# 		}


## automatize from here!
# list of results
out_inc <- list() # output for merged networks of inconvenient feedbacks

for (i in 1:dim(key)[2]){
	out_inc[[i]] <- net.merge(dat, i = key[1,i], j = key[2,i])
}

out_dat <- list()
# out_graph <- list() # output for graphics
for (i in 1:dim(key)[2]){
    x3 <- net.fb(rs.net(dat, key[1,i]), rs.net(dat, key[2,i]), out_inc[[i]])
    out_dat[[i]] <- x3[[1]]
    # out_graph[[i]] <- x[[2]]
}

### When doing for all of them:
inc <- sapply(out_dat, function(x) sum(x$Inconvenient, na.rm = T), simplify = T)

df_inc <- data.frame(inc = inc, Tail = levels(dat$Regime.Shift)[key[1,]], Head = levels(dat$Regime.Shift)[key[2,]])


# g <- ggplot(data = df_inc, aes(Tail, Head)) +
#     geom_tile(aes(fill = log(inc))) + theme_dark(base_size = 6)
#
# g <- g + scale_fill_gradient(low = "#FFA50080", high = "#0000FF80", na.value = "grey50")+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 )) + xlab('') + ylab('') +
# inset(
#   grob = ggplotGrob( ggplot(data = df_inc, aes(inc)) + geom_histogram() + xlab('Inconvenient feedbacks') + theme_gray(base_size = 5) ) ,
#   xmin = 14, xmax =28 , ymin = 1, ymax = 12)

# ## heatmap with heatmaply
# x <- df_inc %>% select(Head, Tail, inc) %>%
#     spread(Head, value = inc) %>%
#     select(-1) %>%
#     as.matrix()
# rownames(x) <- colnames(x)
#
# library(heatmaply)
# heatmaply(x, alpha = .6,
#     na.value = "grey84",
#     column_text_angle = 90,
#     margins = c(250,250,30,30),
#     xlab = "Receiver",
#     ylab = "Source",
#     main = "Inconvenient feedbacks",
#     )

## plot outliers:
# quartz(width = 10, height = 3.5, pointsize = 12)
# par(mfrow = c(1,3))
#
# plotnet(out_dom[[7]]); title('a', adj = 0)
# plotnet(out_dom[[19]]); title('b', adj = 0)
# plotnet(net.merge(dat,7,19))

# out_graph[[267]]

# network.vertex.names(out_dom[[7]])[network.vertex.names(out_dom[[7]]) %in% network.vertex.names(out_dom[[19]])]

inc_net <- network(df_inc %>% select(Tail, Head, inc) %>% filter(inc > 0),
    directed = F, ignore.eval = FALSE, matrix.type = 'edgelist')

inc_net %v% 'indegree' <- sna::degree(inc_net, cmode = 'indegree')
inc_net %v% 'outdegree' <- sna::degree(dom_net, cmode = 'outdegree')

rsdb2 <- rsdb[-25,] # without sprawling cities since it's not in the network

for (i in 2:14){
    inc_net %e% names(rsdb2)[i] <- cracking(i,rsdb) %>%
    table() %>% as.matrix() %>% dist( method="binary", upper=T, diag=T) %>% as.matrix() %>% clean.and.order()
}

## Note that the matrix is symmetrical (undirected graph)
# isSymmetric(as.sociomatrix(x))
# so you only need a triangle in your dataframe
df_test3 <- data.frame(paths = as.sociomatrix(inc_net, 'inc')[upper.tri(as.sociomatrix(inc_net))])

for(i in 2:14){
    m <- cracking(i,rsdb2) %>%
    table() %>% as.matrix() %>% dist( method="binary", upper=T, diag=T) %>% as.matrix() %>% clean.and.order()

    df_test3[i] <- 1 - m[upper.tri(m)]
    names(df_test3)[i] <- names(rsdb)[i]
}

fit3 <- lm(paths ~ landuse + ecotype + ecoprocess + prov_service + reg_service + cult_service + hwb + space_scale + time_scale + reversibility + evidence, data = df_test3)

### ERGMS for Regime Shifts
# library(ergm.count)

fit.null3 <- suppressMessages(
    ergm(inc_net ~ nonzero + sum , response ='inc', reference=~Poisson,
         control = control.ergm(MCMLE.trustregion=1000)) ## AIC: -1258
)

fit.w3 <- suppressMessages(
    ergm(inc_net ~ nonzero + sum + edgecov(inc_net,'landuse') +
    edgecov(inc_net,'ecotype') + edgecov(inc_net,'ecoprocess') + edgecov(inc_net,'prov_service') + edgecov(inc_net,'reg_service') + edgecov(inc_net,'cult_service') + edgecov(inc_net,'hwb') + edgecov(inc_net,'space_scale')+ edgecov(inc_net,'time_scale') + edgecov(inc_net,'reversibility') + edgecov(inc_net,'evidence'),
    response ='inc', reference = ~Poisson,
    control = control.ergm(MCMLE.trustregion=1000)))

summary(fit3); summary(fit.null3); summary(fit.w3)

# ### tables are working so save the work space_scale
# setwd("~/Documents/Projects/Cascading Effects")
# save.image("ergm_data.RData", safe = T)
