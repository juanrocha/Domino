### Script for ergm analysis
### Cascading effects paper
### by Juan Rocha
### juan.rocha@su.se
# rm(list = ls())

### The code version from 1704__ includes code chunks for previous visualization and extensive comments. Here I only keep what has been useful and restrict comments to the minimum.

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

# create bipartite network
bip1 <- network(bipmat, bipartite = T)
degbip <- sna::degree(bip1, gmode="graph")

bip_attr <- read.csv2('~/Documents/Projects/Cascading Effects/bip_attributes.csv')

colors <- c( "#0000EE" ,"#FFB90F", "#EE3B3B", "#8A2BE2", "#FF7F00", "#9ACD32")

bip1 %v% 'attr' <- as.vector(bip_attr$attribute)
bip1 %v% 'col' <- colors[factor(bip_attr$attribute)]



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

	return(list(net1, net2))
}

x <- mode.1(bip1)[[2]] # x is the regime shifts one-mode network

## This loop adds the edge attributes using the RSDB + Jaccard distance
for (i in 2:14){
    x %e% names(rsdb)[i] <- cracking(i,rsdb) %>%
    table() %>% as.matrix() %>% dist( method="binary", upper=T, diag=T) %>% as.matrix() %>% clean.and.order()
}

## add categorical version of scale:
x %v% "time_range" <- c(
	"year_decade",
	"decade_century",
	"month_year",
	"year_decade",
	"decade_century",
	"year_decade",
	"year_decade",
	"year_decade",
	"week_month",
	"decade_century",
	"year_decade",
	"decade_century",
	"month_year",
	"month_year",
	"year_decade",
	"year_decade",
	"year_decade",
	"year_decade",
	"decade_century",
	"decade_century",
	"month_year",
	"year_decade",
	"month_year",
	"year_decade",
	"year_decade",
	"year_decade",
	"decade_century",
	"year_decade",
	"decade_century"
)

x %v% "space_range" <- c(
	"local",
	"sub-continental",
	"local",
	"local",
	"national",
	"local",
	"local",
	"sub-continental",
	"local",
	"national",
	"local",
	"sub-continental",
	"local",
	"local",
	"local",
	"national",
	"national",
	"sub-continental",
	"local",
	"sub-continental",
	"local",
	"local",
	"local",
	"local",
	"local",
	"sub-continental",
	"sub-continental",
	"local",
	"sub-continental"
)
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

## The code above is nice because it calculates the distance automatically for all variables on RSDB. However, the breaking might not be the best and just complicate the interpretation of coeffficients. I will combine all ES and HWB into one variable "impacts"

m <- cracking(7,rsdb) %>%
	rbind(cracking(8,rsdb)) %>%
	rbind(cracking(9,rsdb)) %>%
	rbind(cracking(10,rsdb)) %>%
	table() %>% as.matrix() %>% dist( method="binary", upper=T, diag=T) %>% as.matrix() %>% clean.and.order()

df_test$impacts <- 1- m[upper.tri(m)]
x %e% "impacts" <- m

fit1 <- lm(paths ~ landuse + ecotype + ecoprocess + prov_service + reg_service + cult_service + hwb + space_scale + time_scale + reversibility + evidence, data = df_test)

### ERGMS for Regime Shifts
# library(ergm.count)

fit.null1 <- suppressMessages(ergm(x ~ nonzero + sum , response ='paths', reference=~Poisson, control = control.ergm(MCMLE.trustregion=1000))) ## AIC: -1258


fit.w1 <- suppressMessages(ergm(x ~ nonzero + sum + edgecov(x,'landuse') + edgecov(x,'ecotype')+
 	edgecov(x,'ecoprocess') + edgecov(x,'reg_service') + edgecov(x,'prov_service') + edgecov(x,'cult_service') +  edgecov(x,'hwb') + nodefactor('space_range') + nodefactor('time_range') + nodematch('space_range', diff = FALSE)+ nodematch('time_range', diff = FALSE)   + edgecov(x,'reversibility') + edgecov(x,'evidence'), response ='paths', reference = ~Poisson, control = control.ergm(MCMLE.trustregion=1000)))
# + nodemix('space_range', base = 1) + nodemix("time_range", base = 1) gives a degenerated results.

fit.w1a <- suppressMessages(ergm(x ~ nonzero + sum + edgecov(x,'landuse') +
 	edgecov(x,'ecotype')+
 	edgecov(x,'ecoprocess') + edgecov(x,'reg_service') + edgecov(x,'prov_service') + edgecov(x,'cult_service') +  edgecov(x,'hwb') + edgecov(x,'space_scale')+ edgecov(x,'time_scale') + edgecov(x,'reversibility') + edgecov(x,'evidence'), response ='paths', reference = ~Poisson, control = control.ergm(MCMLE.trustregion=1000)))

summary(fit1); summary(fit.null1); summary(fit.w1); summary(fit.w1a)
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
        rs.mix %v% 'col' <- ifelse(colSums(fb.sum$cycle.count)[-1] == 0, "#E41A1C", "#8DA0CB")
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




#### Run ergms to explain the matrix

for (i in 2:14){
    dom_net %e% names(rsdb)[i] <- cracking(i,rsdb) %>%
    table() %>% as.matrix() %>% dist( method="binary", upper=T, diag=T) %>% as.matrix() %>% clean.and.order()
}
## add categorical version of scale:
dom_net %v% "time_range" <- c(
	"year_decade",
	"decade_century",
	"month_year",
	"year_decade",
	"decade_century",
	"year_decade",
	"year_decade",
	"year_decade",
	"week_month",
	"decade_century",
	"year_decade",
	"decade_century",
	"month_year",
	"month_year",
	"year_decade",
	"year_decade",
	"year_decade",
	"year_decade",
	"decade_century",
	"decade_century",
	"month_year",
	"year_decade",
	"month_year",
	"year_decade",
	"year_decade",
	"year_decade",
	"decade_century",
	"year_decade",
	"decade_century"
)

dom_net %v% "space_range" <- c(
	"local",
	"sub-continental",
	"local",
	"local",
	"national",
	"local",
	"local",
	"sub-continental",
	"local",
	"national",
	"local",
	"sub-continental",
	"local",
	"local",
	"local",
	"national",
	"national",
	"sub-continental",
	"local",
	"sub-continental",
	"local",
	"local",
	"local",
	"local",
	"local",
	"sub-continental",
	"sub-continental",
	"local",
	"sub-continental"
)
## Note that the matrix is not symmetrical (directed graph)
# isSymmetric(as.sociomatrix(dom_net))
# so you only need the complete matrix in your dataframe
df_test2 <- data.frame(paths = as.vector(as.sociomatrix(dom_net, 'weight')))

for(i in 2:14){
    m <- cracking(i,rsdb) %>%
    table() %>% as.matrix() %>% dist( method="binary", upper=T, diag=T) %>% as.matrix() %>% clean.and.order()

    df_test2[i] <- 1 - as.vector(m)
    names(df_test2)[i] <- names(rsdb)[i]
}


m <- cracking(7,rsdb) %>%
	rbind(cracking(8,rsdb)) %>%
	rbind(cracking(9,rsdb)) %>%
	rbind(cracking(10,rsdb)) %>%
	table() %>% as.matrix() %>% dist( method="binary", upper=T, diag=T) %>% as.matrix() %>% clean.and.order()
df_test2$impacts <- 1 - as.vector(m)
dom_net %e% "impacts" <- m

fit2 <- lm(paths ~ landuse + ecotype + ecoprocess + prov_service + reg_service + cult_service + hwb + space_scale + time_scale + reversibility + evidence, data = df_test2)

### ERGMS for Regime Shifts
# library(ergm.count)

fit.null2 <- suppressMessages(
    ergm(dom_net ~ nonzero + sum , response ='weight', reference=~Poisson,
         control = control.ergm(MCMLE.trustregion=1000)) ## AIC: -1258
        )

fit.w2 <- suppressMessages(
    ergm(dom_net ~ nonzero + sum + edgecov(dom_net,'landuse', form = "sum") +
    edgecov(dom_net,'ecotype', form = "sum") + edgecov(dom_net,'ecoprocess', form = "sum") + edgecov(dom_net,'prov_service', form = "sum") + edgecov(dom_net,'reg_service', form = "sum") + edgecov(dom_net,'cult_service', form = "sum") + edgecov(dom_net,'hwb', form = "sum") + edgecov(dom_net,'space_scale', form = "sum")+ edgecov(dom_net,'time_scale', form = "sum") + edgecov(dom_net,'reversibility', form = "sum") + edgecov(dom_net,'evidence', form = "sum"),
    response ='weight', reference = ~Poisson,
    control = control.ergm(MCMLE.trustregion=1000))
    )

# fit.w2a <- suppressMessages(
#     ergm(dom_net ~ nonzero + sum + edgecov(dom_net,'landuse') +
#     edgecov(dom_net,'ecotype') + edgecov(dom_net,'ecoprocess') + edgecov(dom_net,'space_scale')+ edgecov(dom_net,'time_scale') + edgecov(dom_net,'reversibility') + edgecov(dom_net,'evidence'),
#     response ='weight', reference = ~Poisson,
#     control = control.ergm(MCMLE.trustregion=1000))
#     ) ## this simpler fit is not better than the w2

fit.w2a <- suppressMessages(
    ergm(dom_net ~ nonzero + sum + edgecov(dom_net,'landuse', form = "sum") +
    edgecov(dom_net,'ecotype', form = "sum") + edgecov(dom_net,'ecoprocess', form = "sum") + edgecov(dom_net,'prov_service', form = "sum") + edgecov(dom_net,'reg_service', form = "sum") + edgecov(dom_net,'cult_service', form = "sum") + edgecov(dom_net,'hwb', form = "sum") +
	nodefactor('space_range', form = "sum") + nodefactor('time_range', form = "sum") +
	nodematch('space_range', diff = FALSE, form = "sum")+ nodematch('time_range', diff = FALSE, form = "sum") + 
    edgecov(dom_net,'reversibility', form = "sum") + edgecov(dom_net,'evidence', form = "sum"),
    response ='weight', reference = ~Poisson,
    control = control.ergm(MCMLE.trustregion=1000))
    )

fit.w2b <- suppressMessages(
    ergm(dom_net ~ nonzero + sum + edgecov(dom_net,'landuse', form = "sum") +
    edgecov(dom_net,'ecotype', form = "sum") + edgecov(dom_net,'ecoprocess', form = "sum") + edgecov(dom_net,'prov_service', form = "sum") + edgecov(dom_net,'reg_service', form = "sum") + edgecov(dom_net,'cult_service', form = "sum") + edgecov(dom_net,'hwb', form = "sum") +
	nodeifactor('space_range', form = "sum") + nodeifactor('time_range', form = "sum") +
	nodeofactor('space_range', form = "sum") + nodeofactor('time_range', form = "sum") +
	nodematch('space_range', diff = TRUE, form = "sum")+ nodematch('time_range', diff = FALSE, form = "sum") + 
    edgecov(dom_net,'reversibility', form = "sum") + edgecov(dom_net,'evidence', form = "sum"),
    response ='weight', reference = ~Poisson,
    control = control.ergm(MCMLE.trustregion=1000))
    )
### J180910: I'm trying to fit a model with nodemix, which is the term that would allow us to see more clearly
## cross-scale interactions: whether a link is more likely between nodes with different types of scale attributes.
## The mixingmatrix(dom_net, "time_range") reveals there is zeroes, and I get errors of singular matrices.
## But I do get the model fit without extra terms (sometimes). In any case I don't trust it - so let's keep the
## original specification on the paper.

fit.w2c <- suppressMessages(
    ergm(
        dom_net ~ nonzero + sum + #edgecov(dom_net,'landuse', form = "sum") +
        #edgecov(dom_net,'ecotype', form = "sum") + edgecov(dom_net,'ecoprocess', form = "sum") + edgecov(dom_net,'prov_service', form = "sum") + edgecov(dom_net,'reg_service', form = "sum") + edgecov(dom_net,'cult_service', form = "sum") + edgecov(dom_net,'hwb', form = "sum") +
        #nodefactor('space_range', form = "sum") + nodefactor('time_range', form = "sum") +
        #nodematch('space_range', diff = TRUE, form = "sum")+ nodematch('time_range', diff = FALSE, form = "sum") + 
        nodemix("space_range", form = "sum") + 
        nodemix("time_range", form = "sum") #+
        #edgecov(dom_net,'reversibility', form = "sum") + edgecov(dom_net,'evidence', form = "sum")
        ,
    response ='weight', reference = ~Poisson,
    control = control.ergm(MCMLE.trustregion=1000))
)

summary(fit2); summary(fit.null2); summary(fit.w2); summary(fit.w2a); summary(fit.w2b); summary(fit.w2c)

##### Ergms for inconvenient feedbacks

# A function to merge networks based on edge lists aggregation
# The function below is useful when cycles numbers are not calculated. Currently I'm using cycle.comemebership='sum' which gives me the link weigth. As alternative, cycle.comembership='byblength' retunrs an array where each matrix shows cycle co-membership by length.


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

inc_net <- network(
	df_inc %>% select(Tail, Head, inc) %>% filter(inc > 0),
    directed = F, ignore.eval = FALSE, matrix.type = 'edgelist')

inc_net %v% 'degree' <- sna::degree(inc_net, gmode = 'graph')

rsdb2 <- rsdb[-25,] # without sprawling cities since it's not in the network

for (i in 2:14){
    inc_net %e% names(rsdb2)[i] <- cracking(i,rsdb) %>%
    table() %>% as.matrix() %>% dist( method="binary", upper=T, diag=T) %>% as.matrix() %>% clean.and.order()
}

## add categorical version of scale:
inc_net %v% "time_range" <- c(
	"year_decade",
	"decade_century",
	"month_year",
	"year_decade",
	"decade_century",
	"year_decade",
	"year_decade",
	"year_decade",
	"week_month",
	"decade_century",
	"year_decade",
	"decade_century",
	"month_year",
	"month_year",
	"year_decade",
	"year_decade",
	"year_decade",
	"year_decade",
	"decade_century",
	"decade_century",
	"month_year",
	"year_decade",
	"month_year",
	"year_decade",
	"year_decade",
	"year_decade",
	"decade_century",
	"year_decade",
	"decade_century"
)

inc_net %v% "space_range" <- c(
	"local",
	"sub-continental",
	"local",
	"local",
	"national",
	"local",
	"local",
	"sub-continental",
	"local",
	"national",
	"local",
	"sub-continental",
	"local",
	"local",
	"local",
	"national",
	"national",
	"sub-continental",
	"local",
	"sub-continental",
	"local",
	"local",
	"local",
	"local",
	"local",
	"sub-continental",
	"sub-continental",
	"local",
	"sub-continental"
)


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


m <- cracking(7,rsdb2) %>%
	rbind(cracking(8,rsdb2)) %>%
	rbind(cracking(9,rsdb2)) %>%
	rbind(cracking(10,rsdb2)) %>%
	table() %>% as.matrix() %>% dist( method="binary", upper=T, diag=T) %>% as.matrix() %>% clean.and.order()

df_test3$impacts <- 1 - m[upper.tri(m)]
inc_net %e% "impacts" <- m

## transform the response var to log space to see if it improves regressions

# df_test3$log_path <- log(1 + df_test3$paths)
# inc_net %e% "log_inc" <- log(1 + inc_net %e% 'inc')

fit3 <- lm(paths ~ landuse + ecotype + ecoprocess + prov_service + reg_service + cult_service + hwb + space_scale + time_scale + reversibility + evidence, data = df_test3)

### ERGMS for Regime Shifts
# library(ergm.count)

fit.null3 <- suppressMessages(
    ergm(inc_net ~ nonzero + sum , response ='inc', reference=~Poisson,
         control = control.ergm(MCMLE.trustregion=1000)) ## AIC: -1258
)

fit.w3 <- suppressMessages(
    ergm(inc_net ~ nonzero + sum + edgecov(inc_net,'landuse', form = 'sum') +
    edgecov(inc_net,'ecotype', form = 'sum') + edgecov(inc_net,'ecoprocess', form = 'sum') + edgecov(inc_net,'prov_service', form = 'sum') + edgecov(inc_net,'reg_service', form = 'sum') + edgecov(inc_net,'cult_service', form = 'sum') + edgecov(inc_net,'hwb', form = 'sum') + edgecov(inc_net, 'space_scale', form = 'sum')+ edgecov(inc_net,'time_scale', form = 'sum') + edgecov(inc_net,'reversibility', form = 'sum') + edgecov(inc_net,'evidence', form = 'sum'),
    response ='inc', reference = ~Poisson,
    control = control.ergm(MCMLE.trustregion=1000)))

fit.w3a <- suppressMessages(
    ergm(inc_net ~ nonzero + sum + edgecov(inc_net,'landuse', form = 'sum') +
    edgecov(inc_net,'ecotype', form = 'sum') + edgecov(inc_net,'ecoprocess', form = 'sum') + edgecov(inc_net,'prov_service', form = 'sum') + edgecov(inc_net,'reg_service', form = 'sum') + edgecov(inc_net,'cult_service', form = 'sum') + edgecov(inc_net,'hwb', form = 'sum') +
	#edgecov(inc_net, 'space_scale')+ edgecov(inc_net,'time_scale') +
	nodefactor('space_range', form = 'sum') + nodefactor('time_range', form = 'sum') +
	nodematch('space_range', diff = FALSE, form = 'sum')+
	nodematch('time_range', diff = FALSE, form = 'sum') +
	edgecov(inc_net,'reversibility', form = 'sum') + edgecov(inc_net,'evidence', form = 'sum'),
    response ='inc', reference = ~Poisson,
    control = control.ergm(MCMLE.trustregion=1000)))

### check the mixingmatrix(inc_net, "time_range")
fit.w3b <- suppressMessages(
    ergm(inc_net ~ nonzero + sum + edgecov(inc_net,'landuse', form = 'sum') +
             edgecov(inc_net,'ecotype', form = 'sum') + edgecov(inc_net,'ecoprocess', form = 'sum') + 
             #edgecov(inc_net,'prov_service', form = 'sum') + edgecov(inc_net,'reg_service', form = 'sum') + edgecov(inc_net,'cult_service', form = 'sum') + edgecov(inc_net,'hwb', form = 'sum') +
             #edgecov(inc_net, 'space_scale')+ edgecov(inc_net,'time_scale') +
             #nodefactor('space_range', form = 'sum') + nodefactor('time_range', form = 'sum') +
             #nodematch('space_range', diff = FALSE, form = 'sum')+
             #nodematch('time_range', diff = FALSE, form = 'sum') +
             nodemix("space_range", form = "sum") + 
             nodemix("time_range", form = "sum", base = 0 ) + 
             # the base argument is to avoid calculating week to week since it's a zero on the mixing matrix, to figure out which combination you need to delete see inc_net %v% "time_range" %>% unique()
              edgecov(inc_net,'reversibility', form = 'sum') + edgecov(inc_net,'evidence', form = 'sum'),
         response ='inc', reference = ~Poisson,
         control = control.ergm(MCMLE.trustregion=1000)))

summary(fit3); summary(fit.null3); summary(fit.w3); summary(fit.w3a); summary(fit.w3b)

### J180911: none of the models with nodemix can be fitted. The fact that there is zero values on the 
## mixingmatrix implies that the term will have NA std errors and p-values. If I set the base argument to the
## pairing to be excluded (e.g. nodemix ('time_range', form = "sum", base = c(6))) where 6 is the pairing coefficient
## to be excluded) then all other coefficients end up with NA std errors and p-values. Nodemix would have been
## the perfect term to test cross-scale interactions, but doesn't work. For now, we can leave the paper as it is
## given that the other terms offer a way around that is calculable.

# ### tables are working so save the work space_scale
# setwd("~/Documents/Projects/Cascading Effects")
# save.image("170525_ergm_data.RData", safe = T)
