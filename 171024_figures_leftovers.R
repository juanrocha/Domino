### Left overs 171024
## Old figures that were not used in the paper:

## Figure 2.

# quartz(width = 6, height = 4, pointsize = 7)
par(mfrow = c(3,3), mar = c(1,1,1,1)) # layout(matrix(1:12,4,3, byrow=T))
plot.new()
# grid.newpage() # I need this line to plot on quartz, but not on markdown pdf.
pushViewport(viewport(layout = grid.layout(3,3)))


# layout(matrix(c(1:7,7,8),3,3, byrow = T))
# par(mar = c(0,1,1,0))
# Insert png for the framework
# plot(c(0,452), c(0,868), asp = 1, type = 'n', xlab = "", ylab= "", xpd = T, axes = FALSE) # numbers come from pixels
# rasterImage(logo, 0, 0, 452, 868 , interpolate = T)

## first row 2 clds
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
par(fig = gridFIG(), new = TRUE)
plotnet(rs.net(dat, 27))
title('a)', adj = 0, cex=5)
popViewport()

pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
par(fig = gridFIG(), new = TRUE)
plotnet(rs.net(dat, 29))
popViewport()
## plot a bipartite network:
# manually adjust coordinates
network.layout.bip <- function (d, layout.par){
    y <- c(3,2,1,3,2)
    x <- c(1,1,1,2,2)
    cbind(x,y)
}

ex1 <- network(
    matrix(c(1,1,
             1,0,
             1,0), nrow = 3, ncol =2),
    bipartite = TRUE)

ex1 %v% "col" <- c(rep("#E41A1C",3),rep("blue",2))


pushViewport(viewport(layout.pos.row=1, layout.pos.col=3))
par(fig = gridFIG(), new = TRUE)
plot.network(ex1,
             vertex.col = ex1 %v% "col", vertex.cex = 4,
             vertex.border = "white",
             edge.col = "grey84", edge.lwd = 0.15,
             mode = "bip", jitter = FALSE,
             suppress.axes = TRUE, ylim = c(0.2,3.2)
)
text(x=0.8,y=0.5, labels = "Drivers", col = "grey24")
text(x=2.2,y=0.5, labels = "Regime shifts", col = "grey24")
popViewport()

## second panel
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
par(fig = gridFIG(), new = TRUE)
plotnet(rs.net(dat = dat, 27))
title ('b)', adj = 0, cex = 5)
popViewport()

pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
par(fig = gridFIG(), new = TRUE)
plot(0,0, type = 'n', xlim = c(0,1), ylim = c(0,1), axes = FALSE, ann = FALSE)
text(0.5,0.5, labels = "{sea surface temperature,\n climate change,\n precipitation,\n water stratification}", cex = 1)
popViewport()

pushViewport(viewport(layout.pos.row=2, layout.pos.col=3))
par(fig = gridFIG(), new = TRUE)
plotnet(rs.net(dat, 14))
popViewport()
## third row

pushViewport(viewport(layout.pos.row=3, layout.pos.col=1:2))
par(fig = gridFIG(), new = TRUE)
plotnet2(out_inc[[312]])
title ('c)', adj = 0, cex = 5)
popViewport()


x.long <- reshape2::melt(out_dat[[312]], id.var="feed.length",
                         measure.var= colnames(out_dat[[312]])[c(1:3,5,6)],
                         value.name='value')

x.long$value <- as.integer(x.long$value)
x.long$feed.length <- as.integer(x.long$feed.length)

levels(x.long$variable) <- c("Kelp transitions", "Thermohaline circulation", "Both", "Hidden", "Expected")

# plot it

g <- ggplot(filter(x.long, variable == "Kelp transitions" | variable == "Thermohaline circulation"| variable == "Hidden" ), aes(x=feed.length, y=value), group=variable)
g <- g + geom_bar(aes(fill= forcats::fct_rev(variable)), position = 'stack', stat = 'identity')  +
    ylab('Number of feedbacks') + xlab('Feedback length') +
    #ggtitle(out_inc[[312]] %n% 'name') +
    theme_minimal(base_size = 4) +
    theme(legend.position=c(0.75,0.8), legend.key.size  = unit(0.3, "cm")) + ### , plot.title=element_text(size=rel(0.85))
    scale_fill_manual("Feedbacks",values=c("#EE3B3B","#8A2BE2", "#FF7F00")) +
    xlim(0, max(x.long[x.long$value > 0, ]$feed.length))

pushViewport(viewport(layout.pos.row=3, layout.pos.col=3))
par(mar=c(2,2,2,2))
print(g, newpage = F)
popViewport()



################
# Figure 3 with circle
################


df1 <- tidy(fit1); df1$model <- 'OLS'
#df2 <- tidy(fit.null1); df2$model <- 'ergm null'
df3 <- tidy(fit.w1); df3$model <- 'ergm weighted'
df3$term[c(3:9,17,18)] <-  df1$term[c(2:8,11,12)]

df <- full_join(df1, df3) # %>% full_join(df3)
df <- mutate(df, conf.hi = estimate + std.error, conf.low = estimate - std.error)
df$P <- ifelse(df$p.value <= 0.05, "< 0.05", "> 0.05")
df$term <- as.factor(df$term)

# ## change names of places and reorder levels
df$term <- factor(df$term, levels(df$term)[c(1,15,20, 7,4,3,16,17,2,6,19,21,8,9,10:14,18,5)])
# levels(df$term) <- c("Intercept","Non zero",'Sum', "Landuse","Ecosystem type", "Ecosystem processes", "Provisioning services","Regulating services", "Cultural services", "Human wellbeing", "Spatial scale", "Temporal scale", "Reversibility", "Evidence type")
df$term <- factor(df$term, rev(levels(df$term)))

df$model <- as.factor(df$model)
# df$model <- factor(df$model, levels(df$model)[c(3,1,2)])

## plot results
p <- ggplot(df, aes(estimate, term))+
    geom_point(aes(shape = factor(P)), size = 2, show.legend = T) +
    scale_shape_manual(name = "p value", values = c(19,1)) +
    geom_errorbarh(aes(xmax = conf.hi, xmin = conf.low, height = .1),
                   show.legend = T) +
    geom_vline(xintercept = 0, color = 'grey', show.legend = F, linetype = 2)  +
    facet_wrap(~ model, scales = "free_x") + theme_light(base_size = 6) +
    theme(legend.position = "bottom")

## Setting the bipartite network
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



# bip_attr <- read.csv2('~/Documents/Projects/Cascading Effects/bip_attributes.csv')
#
# colors <- c( "#0000EE" ,"#FFB90F", "#EE3B3B", "#8A2BE2", "#FF7F00", "#9ACD32")

# bip1 %v% 'attr' <- as.vector(bip_attr$attribute)
# bip1 %v% 'col' <- colors[factor(bip_attr$attribute)]

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
library(circlize)

# quartz(width=8, height =4, family = 'Helvetica', pointsize = 6)
par(mfrow = c(1,2)) # layout(matrix(1:12,4,3, byrow=T))
plot.new()
# # grid.newpage() # I need this line to plot on quartz, but not on markdown pdf.
pushViewport(viewport(layout = grid.layout(1,2)))

## First plot the network
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
par(fig = gridFIG(), new = TRUE)

circos.par(gap.degree = c(rep(2, length(unique(bip.edgelist[[1]]))-1), 30,
                          rep(2, length(unique(bip.edgelist[[2]]))-1), 30),
           start.degree = 90, clock.wise = F)

chordDiagram(bip.edgelist, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.3), grid.col = bip1 %v% 'col')
#
## from tutorial
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    #if(abs(xplot[2] - xplot[1]) < 20) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5), cex = .75)#}
    # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
    #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
    # }
}
, bg.border = NA)
circos.clear()
detach(package:circlize)

### Add legends.
legend("bottomright", legend = c("Aquatic", "Terrestrial", "Subcontinental"),
       fill = c("#0000EE","#9ACD32", "#FF7F00"), border = FALSE,
       title = 'Regime shift type', bty = 'n', cex = 0.8)
legend("bottomleft", legend = c("International", "Regional", "Local"),
       fill = c("#FFB90F","#EE3B3B", "#8A2BE2"), border = FALSE,
       title = 'Drivers scale of\n management', bty = 'n', cex = 0.8)
popViewport()


pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
print(p, newpage = F)
popViewport()



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

### Figures of models
df1 <- tidy(fit2); df1$model <- 'OLS'
df2 <- tidy(fit.null2); df2$model <- 'ergm null'
df3 <- tidy(fit.w2); df3$model <- 'ergm weighted'
df3$term[3:13] <-  df1$term[2:12]

df <- full_join(df1, df3) # %>% full_join(df3)
df <- mutate(df, conf.hi = estimate + std.error, conf.low = estimate - std.error)
df$P <- ifelse(df$p.value <= 0.05, "< 0.05", "> 0.05")
df$term <- as.factor(df$term)

## change names of places and reorder levels
df$term <- factor(df$term, levels(df$term)[c(1,8,13,7,4,3,9,10,2,6,12,14,11,5)])
levels(df$term) <- c("Intercept","Non zero",'Sum', "Landuse","Ecosystem type", "Ecosystem processes", "Provisioning services","Regulating services", "Cultural services", "Human wellbeing", "Spatial scale", "Temporal scale", "Reversibility", "Evidence type")
df$term <- factor(df$term, rev(levels(df$term)))

df$model <- as.factor(df$model)
df$model <- factor(df$model, levels(df$model)[c(3,1,2)])


## plot results
p <- ggplot(df, aes(estimate, term))+
    geom_point(aes(shape = factor(P)), show.legend = T) +
    scale_shape_manual(name = "p value", values = c(19,1)) +
    geom_errorbarh(aes(xmax = conf.hi, xmin = conf.low, height = .1), show.legend = T) +
    geom_vline(xintercept = 0, color = 'grey', show.legend = F, linetype = 2) +
    facet_wrap(~model, scales = "free_x") + theme_light(base_size = 6) +
    theme(legend.position = "bottom")
# p

### figures for inconvenient feedback and model

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

df1 <- tidy(fit3); df1$model <- 'OLS'
# df2 <- tidy(fit.null3); df2$model <- 'ergm null'
df3 <- tidy(fit.w3); df3$model <- 'ergm weighted'
df3$term[3:13] <-  df1$term[2:12]

df <- full_join(df1, df3)# %>% full_join(df3)
df <- mutate(df, conf.hi = estimate + std.error, conf.low = estimate - std.error)
df$P <- ifelse(df$p.value <= 0.05, "< 0.05", "> 0.05")
df$term <- as.factor(df$term)

## change names of places and reorder levels
df$term <- factor(df$term, levels(df$term)[c(1,8,13,7,4,3,9,10,2,6,12,14,11,5)])
levels(df$term) <- c("Intercept","Non zero",'Sum', "Landuse","Ecosystem type", "Ecosystem processes", "Provisioning services","Regulating services", "Cultural services", "Human wellbeing", "Spatial scale", "Temporal scale", "Reversibility", "Evidence type")
df$term <- factor(df$term, rev(levels(df$term)))

df$model <- as.factor(df$model)
df$model <- factor(df$model, levels(df$model)[c(3,1,2)])

## plot results
q <- ggplot(df, aes(estimate, term))+
    geom_point(aes(shape = factor(P)), show.legend = T) +
    scale_shape_manual(name = "p value", values = c(19,1)) +
    geom_errorbarh(aes(xmax = conf.hi, xmin = conf.low, height = .1), show.legend = T) +
    geom_vline(xintercept = 0, color = 'grey', show.legend = F, linetype = 2) +
    facet_wrap(~model, scales = "free_x") + theme_light(base_size = 6) +
    theme(legend.position = "bottom")


### uggly matrix with all of them


fork <- as.sociomatrix(x)
domino <- as.sociomatrix(dom_net)
inconvenient <- as.sociomatrix(inc_net)

inconvenient <- cbind(inconvenient, rep(0,29))
inconvenient <- rbind(inconvenient, rep(0,30))

colnames(inconvenient)[30] <- "Sprawling vs compact city"
rownames(inconvenient)[30] <- "Sprawling vs compact city"

inconvenient <- clean.and.order(inconvenient)

# all <- as.data.frame(fork + domino + inconvenient)
# all$from <- rownames(all)
# df_all <- gather(all,  1:30, key = to, value = count)

df_fork <- as_tibble(fork) %>% mutate(from = colnames(fork)) %>%
    gather(key = to, value = fork, 1:30)
df_domino <- as_tibble(domino) %>% mutate(from = colnames(domino)) %>%
    gather(key = to, value = domino, 1:30)
df_inconvenient <- as_tibble(inconvenient) %>% mutate(from = colnames(inconvenient)) %>%
    gather(key = to, value = inc, 1:30)


df_all <- left_join(df_fork, df_domino) %>% left_join(df_inconvenient) %>%
    group_by(from,to) %>%
    mutate(all = sum(fork, domino, inc))


df_all %>% filter(fork == 1, domino ==0, inc ==0) %>%
    ggplot(aes(y = from, x = to)) +
    geom_raster(aes(fill= fork)) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ))


## Heatmap with ggplot
g <- ggplot(data = df_all, aes(y = from, x = to)) +
    geom_tile(aes(fill = count)) + theme_minimal(base_size = 6)

g + scale_fill_gradient(high = "#FFA50080", low = "#0000FF80", na.value = "grey50")+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 )) + xlab('') + ylab('')

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



######
## Old figure 7. Inconvenient feedbacks in drylands

## In analyizing a subset and produce barplos, uncomment the following.
# rs1 <- list()
# rs1 <- apply(rs.net, rs, )
#
# rs1 <- rs.net(rs, 1)
# rs2 <- rs.net(rs, 2)

#
# x <- melt(out_dat[[1]], id.var="feed.length",
#          measure.var= colnames(out_dat[[1]])[c(1:3,5,6)],
#          value.name='value') %>%
#          mutate(feed.length = as.integer(feed.length))
#
# x$variable <- factor(x$variable, levels = rev(levels(x$variable)))
#
# library(forcats)
# g1<-ggplot(filter(x, variable == 'RS1'),
#     aes(x=feed.length, y=value)) +
#     geom_bar( position = 'dodge', stat = 'identity')  + ylab('Number of feedbacks') + xlab('Feedback length') + xlim(0,max(out_graph[[1]]$data[out_graph[[1]]$data$value > 0, ]$feed.length)) + theme_minimal(base_size = 7) + ylim(0,5)
# g2<-ggplot(filter(x, variable == 'RS2'),
#     aes(x=feed.length, y=value)) + geom_bar( position = 'dodge', stat = 'identity')  + ylab('Number of feedbacks') + xlab('Feedback length') + xlim(0,max(out_graph[[1]]$data[out_graph[[1]]$data$value > 0, ]$feed.length)) + theme_minimal(base_size = 7) + ylim(0,5)
# g3 <- ggplot(filter(x,  variable == 'Inconvenient' | variable == 'Expected') ,
#     aes(x=feed.length, y=value)) +
#     geom_bar( position = 'stack', stat = 'identity', aes(fill = fct_rev(variable)))  +
#     ylab('Number of feedbacks') + xlab('Feedback length') + xlim(0,max(out_graph[[1]]$data[out_graph[[1]]$data$value > 0, ]$feed.length)) + theme_minimal(base_size = 7) + ylim(0,5) +
#     scale_fill_manual("New feedbacks",values=c("#E41A1C","#377EB8")) +
#     theme(legend.position=c(0.8,0.8))
#
# g <- list() # list of plots
# indx <- c(87,91,163,107,176,258,106,178,260,416) # the current list is only for drylands based on 'key'
#
# for (i in 2:length(out_dat)){    # works on all RS (i in 2:length(out_dat))
#     x <- reshape::melt(out_dat[[i]], id.var="feed.length",
#              measure.var= colnames(out_dat[[i]])[c(1:3,5,6)],
#              value.name='value') %>%
#              mutate(feed.length = as.integer(feed.length))
#     x$variable <- factor(x$variable, levels = rev(levels(x$variable)))
#     g[[i-1]] <- ggplot(filter(x,  variable == 'Inconvenient' | variable == 'Expected') , aes(x=feed.length, y=value)) +
#         geom_bar( position = 'stack', stat = 'identity', aes(fill = fct_rev(variable)), show.legend = F)  +
#         ylab('Number of feedbacks') + xlab('Feedback length') +
#         xlim(0,max(out_graph[[i]]$data[out_graph[[i]]$data$value > 0, ]$feed.length)) + theme_minimal(base_size = 4 ) + ylim(0,5) +
#         scale_fill_manual("New feedbacks",values=c("#E41A1C","#377EB8")) +
#         theme(legend.position=c(0.7,0.7)) + ggtitle(out[[i]] %n% 'name')
# }
#

# The graph g3 has the wrong order of bars but haven't been able to correct it. ON the to do list.
# ggplot(filter(x,  variable == 'Inconvenient' | variable == 'Expected') %>% droplevels(),
#     aes(x=feed.length, y=value)) +
#     geom_bar( aes(fill=variable), position = 'stack', stat = 'identity')  + ylab('Number of feedbacks') + xlab('Feedback length') + xlim(0,max(out_graph[[1]]$data[out_graph[[1]]$data$value > 0, ]$feed.length))  + ylim(0,5)

# library(gridBase)
# library(grid)
# library(gridExtra)
#
#
# # quartz(width = 6, height = 4, pointsize = 7)
# par(mfrow = c(2,3)) # layout(matrix(1:12,4,3, byrow=T))
# plot.new()
# # grid.newpage() # I need this line to plot on quartz, but not on markdown pdf.
# pushViewport(viewport(layout = grid.layout(2,3)))
#
# ## First networks
# pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
# par(fig = gridFIG(), new = TRUE)
# plotnet(rs1)
# popViewport()
# pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
# par(fig = gridFIG(), new = TRUE)
# plotnet(rs2)
# popViewport()
# pushViewport(viewport(layout.pos.row=1, layout.pos.col=3))
# par(fig = gridFIG(), new = TRUE)
# plotnet(out[[1]])
# popViewport()
#
# ## Then the feedback histograms
# pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
# print(g1, newpage = F)
# popViewport()
#
# pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
# print(g2, newpage = F)
# popViewport()
#
# pushViewport(viewport(layout.pos.row=2, layout.pos.col=3))
# print(g3, newpage = F)
# # popViewport()


# source('~/Dropbox/Code/multiplot.R')
# layout <- matrix(1:10, 2,5)
# multiplot(plotlist = g, layout = layout)



#### Second or third round of recycling


g1 <- ggplot(
    data = as.sociomatrix(x, "paths") %>% as_tibble() %>% mutate(tail = rownames(as.sociomatrix(x))) %>%
        gather(key = head, value = paths,  1:30) %>% mutate(tail = as.factor(tail) %>% forcats::fct_rev()),
    aes(tail, paths)) +
    geom_boxplot(fill = alpha("blue", 0.5), color = "blue", outlier.alpha = 0.5, 
                 outlier.size = 0.8) +
    geom_point(
        data = bip.edgelist %>% mutate(value = 1) %>%
            group_by(rs) %>%
            summarize(paths = sum(value)),
        aes(rs, paths), color = "purple", size = 1.5, shape = 12
    ) + coord_flip() +  xlab("regime shifts") + ylab("number of drivers") + ggtitle("a)") +
    theme_minimal(base_size = 6)
