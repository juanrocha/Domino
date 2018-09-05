## this script drafts a figure 6 for the Cascading paper. Following suggestions from Garry on how to make a more synthetic picture of the technical results.

# libraries
library(tidyverse)
library(ggplot2)
library(network)
library(ggrepel)


# dataset
setwd('~/Documents/Projects/Cascading Effects/Domino')
load('~/Documents/Projects/Cascading Effects/170525_ergm_data.RData')

# df_rs_type
df_rs_type <- tibble(
    tail = rsdb$name,
    type = c("Aquatic", "Earth", "Aquatic", "Terrestrial", "Terrestrial", "Aquatic",
             "Terrestrial", "Aquatic", "Aquatic", "Terrestrial", "Aquatic", "Earth",
             "Aquatic", "Aquatic", "Land-water interface", "Aquatic",
             "Aquatic", "Earth", "Land-water interface", "Aquatic","Land-water interface",
             "Land-water interface", "Aquatic","Terrestrial","Terrestrial","Terrestrial",
             "Earth","Land-water interface", "Terrestrial", "Earth")
)

## here is the sharing drivers data transformed:
df_drivers <- x %>%  # x is the netowrk of shared drivers.
    as.sociomatrix(., "paths") %>% as_tibble() %>%
    mutate(tail = rownames(as.sociomatrix(x))) %>%
    gather(key = head, value = paths,  1:30) %>%
    mutate(tail = as.factor(tail)) %>%
    mutate(tail = forcats::fct_reorder(
        tail, paths, fun = sum, .desc = FALSE)) %>%
    group_by(tail) %>%
    summarize(mean_driv = mean(paths, na.rm = TRUE),
            max_driv = max(paths, na.rm = TRUE),
            min_driv = min(paths, na.rm = TRUE)) %>%
    left_join(
        # you need this to capture the number of Drivers
        bip.edgelist %>% mutate(value = 1) %>%
            group_by(rs) %>%
            summarize(num_driv = sum(value)) %>%
            rename(tail = rs) %>%
            mutate(tail = as.factor(tail))
        ) %>%
    left_join(df_rs_type)

## Here is the dominoes data:
df_domino <- out %>%
        mutate(Tail = as.factor(Tail), Head = as.factor(Head)) %>%
        filter(weight > 0) %>% pull(Tail) %>% fct_count() %>% rename(outdegree = n)

df_domino <- left_join(df_domino,
        out %>%
        mutate(Tail = as.factor(Tail), Head = as.factor(Head)) %>%
        filter(weight > 0) %>% pull(Head) %>% fct_count() %>% rename(indegree = n)
)

df_domino <- left_join(df_domino,
    out %>% group_by(Tail) %>%
        summarize(out_avg = mean(weight), out_max = max(weight)) %>%
        rename(f = Tail)
)

df_domino <- left_join(df_domino,
    out %>% group_by(Head) %>%
        summarize(in_avg = mean(weight), in_max = max(weight)) %>%
        rename(f = Head)
)

#df_domino <- df_domino %>%
    # this was Garry suggestion, it wont work due to zero division.
    # mutate(ratio_in_out = indegree / outde)
## maybe use the domino as attributes that does not require any division.

## Here's the data for hidden feedbacks_
## I need to make the matrix symmetric so it does count the degrees correctly.
df_inc2 <- data.frame(inc = inc, Head = levels(dat$Regime.Shift)[key[1,]], Tail = levels(dat$Regime.Shift)[key[2,]])

df_inc3 <- bind_rows(df_inc, df_inc2)

df_hidden <- df_inc3 %>%
    mutate(Tail = as.factor(Tail), Head = as.factor(Head)) %>%
    mutate(Tail = fct_reorder(Tail, inc, fun = mean, .desc = FALSE )) %>% filter(inc > 0) %>% as_tibble() %>%
    group_by(Tail) %>%
    summarize(mean_hidden = mean(inc, na.rm = TRUE),
            max_hidden = max(inc, na.rm = TRUE),
            min_hidden = min(inc, na.rm = TRUE))%>%
    left_join(df_rs_type, by = c("Tail" = "tail"))

df_all <- left_join(df_drivers, df_domino, by = c('tail' = 'f')) %>%
    left_join(df_hidden, by = c('tail' = "Tail", "type" = "type"))

g <- ggplot(df_all, aes(mean_driv, mean_hidden)) +
    geom_point(aes(colour = factor(type)), shape = 21, size =1, stroke = 1 ,  show.legend = TRUE) +
    # geom_point(aes(colour = indegree, fill = outdegree),shape = 21, size = 2, stroke = 1, alpha = 0.9) +
    # scale_color_gradient(low = "#6495ED", high = "#00008B", guide = "colorbar", name = " Indegree") +
    # scale_fill_gradient(low = "#FFD700", high = "#8B0000", guide = "colorbar", name =  "Outdegree") +
    # geom_errorbar(aes(ymin = min_hidden, ymax = max_hidden, color = type), alpha = 0.5) +
    # geom_errorbarh(aes(xmin = min_driv, xmax = max_driv, color = type), alpha = 0.5) +
    geom_text_repel(aes(label = tail, color = type), size = 1.5) +
    scale_color_manual("Ecosystem type",values = c("blue", "purple", "red","orange")) +
    ggtitle("A") + #coord_equal() +
    labs(x = "mean drivers shared", y = "mean hidden feedbacks") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "right", legend.direction = "vertical",
          legend.text = element_text(size = 4), legend.title = element_text(size = 8))
    #       )
g

quartz(width = 3.5, height = 3.5)


### Figure with the balls.
### Here you need again the raw data and calculate connections between the broader categories.
df_drivers <- x %>%  # x is the netowrk of shared drivers.
    as.sociomatrix(., "paths") %>% as_tibble() %>%
    mutate(tail = rownames(as.sociomatrix(x))) %>%
    gather(key = head, value = paths,  1:30) %>%
    mutate(tail = as.factor(tail)) %>%
    mutate(tail = forcats::fct_reorder(
        tail, paths, fun = sum, .desc = FALSE)) %>%
    left_join(df_rs_type) %>% rename(link_from = type) %>%
    left_join(df_rs_type, by= c("head" = "tail")) %>% rename(link_to = type) %>%
    filter(paths > 0) %>%
    arrange(link_from) %>%
    select(link_from, link_to, paths, tail, head)

df_domino <- as_tibble(out) %>%
    select(Tail, Head, weight) %>%
    left_join(df_rs_type, by = c("Tail"="tail")) %>%
    rename(link_from = type) %>%
    left_join(df_rs_type, by = c("Head"="tail")) %>%
    rename(link_to = type) %>%
    arrange(link_from) %>%
    select(link_from, link_to, weight, Tail, Head)

df_hidden <- as_tibble(df_inc3) %>%
    select(Tail, Head, inc) %>%
    left_join(df_rs_type, by = c("Tail"="tail")) %>%
    rename(link_from = type) %>%
    left_join(df_rs_type, by = c("Head"="tail")) %>%
    rename(link_to = type) %>%
    arrange(link_from) %>%
    select(link_from, link_to, inc, Tail, Head)

library(circlize)
library(shape)

quartz(width=7.5, height =2.5, family = 'Helvetica', pointsize = 9)
layout(matrix(1:3, nrow = 1, ncol = 3))

## Figure 6a
par(mar = c(rep(1,4)))
# circos.par(cell.padding = c(rep(0,4)))
chordDiagram(
    df_drivers, annotationTrack = c("grid"),
    annotationTrackHeight = c( 0.1),
    #preAllocateTracks = list(track.height = 0.1),
    #directional = 1, direction.type = "arrows",link.arr.type = "big.arrow",
    link.arr.lwd = NA, grid.col = c("blue", "purple", "red","orange"))

## from tutorial
circos.trackPlotRegion(track.index = 1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
    #if(abs(xplot[2] - xplot[1]) < 20) {
        circos.text(mean(xlim), mean(ylim), sector.name, facing = "bending.outside", niceFacing = TRUE, #adj = c(0.5, 0),
        cex = 1, col = "white") #}
    # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
    #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
    # }
    } , bg.border = NA)
title("A", adj = 0, cex = 2)
circos.clear()


## Figure 6b
par(mar = c(rep(1,4)))
# circos.par(cell.padding = c(rep(0,4)))
chordDiagram(
    df_domino, annotationTrack = "grid",
    annotationTrackHeight = c( 0.1),
    #preAllocateTracks = list(track.height = 0.1),
    directional = 1, direction.type = "diffHeight+arrows",link.arr.type = "big.arrow",
    link.arr.lwd = NA, grid.col = c("blue", "purple", "red","orange"))

## from tutorial
circos.trackPlotRegion(track.index = 1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
    #if(abs(xplot[2] - xplot[1]) < 20) {
        circos.text(mean(xlim), mean(ylim), sector.name, facing = "bending.outside", niceFacing = TRUE, #adj = c(0.5, 0),
        cex = 1, col = "white") #}
    # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
    #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
    # }
    } , bg.border = NA)
title("B", adj = 0, cex = 2)
circos.clear()


## Figure 6c
par(mar = c(rep(1,4)))
# circos.par(cell.padding = c(rep(0,4)))
chordDiagram(
    df_hidden, annotationTrack = "grid",
    annotationTrackHeight = c( 0.1),
    #preAllocateTracks = list(track.height = 0.1),
    #directional = 1, direction.type = "arrows",link.arr.type = "big.arrow",
    link.arr.lwd = NA, grid.col = c("blue", "purple", "red","orange"))

## from tutorial
circos.trackPlotRegion(track.index = 1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
    #if(abs(xplot[2] - xplot[1]) < 20) {
        circos.text(mean(xlim), mean(ylim), sector.name, facing = "bending.outside", niceFacing = TRUE, #adj = c(0.5, 0),
        cex = 1, col = "white") #}
    # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
    #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
    # }
    } , bg.border = NA)
title("C", adj = 0, cex = 2)
circos.clear()

# detach(package:circlize)

#################################
#### Option B with regime shifts:
################################

# I will need a different colour palette, but keeping the original scheme.
library(RColorBrewer)
library(viridis)
## check the numbers, note that Sprawling to dense cities does not have shared drivers.
df_rs_type %>% group_by(type) %>% summarize (n = n())

cols <- c(
    viridis(12, begin = 0.1, end = 0.4), # aquatic
    plasma(5, begin = 0.2, end = 0.4), # Earth
    inferno(5, begin = 0.7, end = 0.5), # land-Water
    inferno(8, begin = 1, end = 0.8) # Terrestrial
)

quartz(width=7.5, height =2.5, family = 'Helvetica', pointsize = 6)
layout(matrix(1:3, nrow = 1, ncol = 3))

## Figure 6a
par(mar = c(rep(1,4)))
circos.par(cell.padding = c(rep(0,4)))
chordDiagram(
    df_drivers %>% select(tail, head, paths, link_from, link_to), annotationTrack = c("grid"),
    annotationTrackHeight = c( 0.05),
    preAllocateTracks = list(track.height = 0.4),
    #directional = 1, direction.type = "arrows",link.arr.type = "big.arrow",
    link.arr.lwd = NA,
    grid.col = cols[-30]
)

## from tutorial
circos.trackPlotRegion(track.index = 1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
    #if(abs(xplot[2] - xplot[1]) < 20) {
        circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5),
        cex = 0.8, col = "grey24") #}
    # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
    #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
    # }
    } , bg.border = NA)
title("A", adj = 0, cex = 2)
circos.clear()


## Figure 6b
par(mar = c(rep(1,4)))
# circos.par(cell.padding = c(rep(0,4)))
chordDiagram(
    df_domino %>% select(Tail, Head, weight, link_from, link_to),
    annotationTrack = "grid",
    annotationTrackHeight = c( 0.05),
    preAllocateTracks = list(track.height = 0.4),
    directional = 1, direction.type = "diffHeight+arrows",link.arr.type = "big.arrow",
    link.arr.lwd = NA, grid.col = cols)

## from tutorial
circos.trackPlotRegion(track.index = 1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
    #if(abs(xplot[2] - xplot[1]) < 20) {
        circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5),
        cex = 0.8, col = "grey24") #}
    # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
    #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
    # }
    } , bg.border = NA)
title("B", adj = 0, cex = 2)
circos.clear()


## Figure 6c
par(mar = c(rep(1,4)))
# circos.par(cell.padding = c(rep(0,4)))
chordDiagram(
    df_hidden %>% select(Tail, Head, inc, link_from, link_to),
    annotationTrack = "grid",
    annotationTrackHeight = c( 0.05),
    preAllocateTracks = list(track.height = 0.4),
    #directional = 1, direction.type = "arrows",link.arr.type = "big.arrow",
    link.arr.lwd = NA, grid.col = cols)

## from tutorial
circos.trackPlotRegion(track.index = 1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
    #if(abs(xplot[2] - xplot[1]) < 20) {
        circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5),
        cex = 0.8, col = "gray24") #}
    # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
    #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
    # }
    } , bg.border = NA)
title("C", adj = 0, cex = 2)
circos.clear()


#######
# Matrix figure with panel organized.
#######
df_fork <- as.sociomatrix(x, "paths") %>% as_tibble() %>% mutate(tail = rownames(as.sociomatrix(x))) %>%
        gather(key = head, value = drivers,  1:30) %>% mutate(tail = as.factor(tail) %>% forcats::fct_rev()) %>%
    mutate(drivers = ifelse(drivers > 0, 1, 0))
# domino <- as.sociomatrix(dom_net)
# inconvenient <- as.sociomatrix(inc_net)

# inconvenient <- cbind(inconvenient, rep(0,29))
# inconvenient <- rbind(inconvenient, rep(0,30))
#
# colnames(inconvenient)[30] <- "Sprawling vs compact city"
# rownames(inconvenient)[30] <- "Sprawling vs compact city"
#
# inconvenient <- clean.and.order(inconvenient)

# all <- as.data.frame(fork + domino + inconvenient)
# all$from <- rownames(all)
# df_all <- gather(all,  1:30, key = to, value = count)


df_domino <- as_tibble(out) %>% rename(tail = Tail, head = Head) %>%
    mutate(domino = ifelse(weight > 0, 1, 0)) %>% select(-driv2feed, -weight)
df_hidden <- as_tibble(df_inc3) %>% rename(tail = Tail, head = Head) %>%
    mutate(hidden = ifelse(inc > 0, 1, 0)) %>% select(-inc)


df_all <- left_join(df_fork, df_domino) %>% left_join(df_hidden) %>%
    group_by(tail,head) %>%
    mutate(all = sum(drivers, domino, hidden),
           type = ifelse(is.na(all), NA,
                         ifelse(all == 3, "all",
                                ifelse(all == 2, "multiple",
                                       ifelse(all == 1 && drivers == 1, "drivers sharing",
                                              ifelse(all == 1 && domino == 1, "domino effect",
                                                     ifelse(all == 1 && hidden == 1, "hidden feedback", NA)
                                                     )
                                              )
                                       )
                                )
           )
    )

df_all$type <- as.factor(df_all$type) %>% forcats::fct_relevel(
    "drivers sharing", "domino effect", "hidden feedback", "multiple", "all") %>%
    forcats::fct_rev() # %>%
    #fct_collapse(multiple = c('all', 'multiple')) #%>%
    # fct_explicit_na(na_level = "other") %>%
    # fct_collapse(other = c("drivers sharing", "other"))

m4 <- df_all %>% ungroup() %>% # filter(fork == 1, domino ==0, inc ==0) %>%
    mutate(tail = as_factor(tail) %>% forcats::fct_rev(),
               head = as_factor(head)) %>% #%>% forcats::fct_rev()),
    mutate(
        tail = fct_reorder(tail, all,
            .fun = function(x) {sum(x, na.rm = TRUE)}),
        head = fct_reorder(head, all,
            .fun = function(x) {sum(x, na.rm = TRUE)},
        .desc = TRUE)) %>%
    ggplot(aes(y = tail, x = head)) +
    geom_tile(aes(fill= type)) +
    scale_fill_brewer(type = "seq", palette = "YlOrBr", direction = -1, na.value = "gray57",
                      guide = guide_legend(title = "Cascading effects", barwidth = 0.5)) +
    theme_light(base_size = 10) + xlab('') + ylab('') + ggtitle("Summary matrix") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ),
          legend.position = "bottom")

m4

#################
# Figure with scales
#################

df_scales <- data_frame(
    rs = x %v% "vertex.names",
    space = x %v% "space_range",
    time = x %v% "time_range"
)

## make a drivers df again including the scales:
df_drivers <- x %>%  # x is the netowrk of shared drivers.
    as.sociomatrix(., "paths") %>% as_tibble() %>%
    mutate(tail = rownames(as.sociomatrix(x))) %>%
    gather(key = head, value = paths,  1:30) %>%
    mutate(tail = as.factor(tail)) %>%
    mutate(tail = forcats::fct_reorder(
        tail, paths, fun = sum, .desc = FALSE)) %>%
    left_join(df_scales, by = c("tail" = "rs"))%>% rename(link_from = time) %>%
    left_join(df_scales, by= c("head" = "rs")) %>% rename(link_to = time) %>%
    filter(paths > 0) %>%
    arrange(link_from) %>%
    select(link_from, link_to, paths, tail, head)

df_domino <- as_tibble(out) %>%
    select(Tail, Head, weight) %>%
    left_join(df_scales, by = c("Tail"="rs")) %>%
    rename(link_from = time) %>%
    left_join(df_scales, by = c("Head"="rs")) %>%
    rename(link_to = time) %>%
    arrange(link_from) %>%
    select(link_from, link_to, weight, Tail, Head)

## I need to make the matrix symmetric so it does count the degrees correctly.
df_inc2 <- data.frame(inc = inc, Head = levels(dat$Regime.Shift)[key[1,]], Tail = levels(dat$Regime.Shift)[key[2,]])

df_inc3 <- bind_rows(df_inc, df_inc2)

df_hidden <- as_tibble(df_inc3) %>%
    select(Tail, Head, inc) %>%
    left_join(df_scales, by = c("Tail"="rs")) %>%
    rename(link_from = time) %>%
    left_join(df_scales, by = c("Head"="rs")) %>%
    rename(link_to = time) %>%
    arrange(link_from) %>%
    select(link_from, link_to, inc, Tail, Head)

### cicos:
quartz(width=7.5, height =2.5, family = 'Helvetica', pointsize = 9)
layout(matrix(1:3, nrow = 1, ncol = 3))

## Figure 6a
par(mar = c(rep(1,4)))
# circos.par(cell.padding = c(rep(0,4)))
chordDiagram(
    df_drivers, annotationTrack = c("grid"),
    annotationTrackHeight = c( 0.1),
    #preAllocateTracks = list(track.height = 0.1),
    #directional = 1, direction.type = "arrows",link.arr.type = "big.arrow",
    link.arr.lwd = NA , grid.col = brewer.pal(4, "Dark2")
)

## from tutorial
circos.trackPlotRegion(track.index = 1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
    #if(abs(xplot[2] - xplot[1]) < 20) {
        circos.text(mean(xlim), mean(ylim), sector.name, facing = "bending.outside", niceFacing = TRUE, #adj = c(0.5, 0),
        cex = 1, col = "white") #}
    # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
    #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
    # }
    } , bg.border = NA)
title("A", adj = 0, cex = 2)
circos.clear()


## Figure 6b
par(mar = c(rep(1,4)))
# circos.par(cell.padding = c(rep(0,4)))
chordDiagram(
    df_domino, annotationTrack = "grid",
    annotationTrackHeight = c( 0.1),
    #preAllocateTracks = list(track.height = 0.1),
    directional = 1, direction.type = "diffHeight+arrows",link.arr.type = "big.arrow",
    link.arr.lwd = NA, grid.col = brewer.pal(4, "Dark2"))

## from tutorial
circos.trackPlotRegion(track.index = 1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
    #if(abs(xplot[2] - xplot[1]) < 20) {
        circos.text(mean(xlim), mean(ylim), sector.name, facing = "bending.outside", niceFacing = TRUE, #adj = c(0.5, 0),
        cex = 1, col = "white") #}
    # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
    #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
    # }
    } , bg.border = NA)
title("B", adj = 0, cex = 2)
circos.clear()


## Figure 6c
par(mar = c(rep(1,4)))
# circos.par(cell.padding = c(rep(0,4)))
chordDiagram(
    df_hidden, annotationTrack = "grid",
    annotationTrackHeight = c( 0.1),
    #preAllocateTracks = list(track.height = 0.1),
    #directional = 1, direction.type = "arrows",link.arr.type = "big.arrow",
    link.arr.lwd = NA, grid.col = brewer.pal(4, "Dark2"))

## from tutorial
circos.trackPlotRegion(track.index = 1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
    #if(abs(xplot[2] - xplot[1]) < 20) {
        circos.text(mean(xlim), mean(ylim), sector.name, facing = "bending.outside", niceFacing = TRUE, #adj = c(0.5, 0),
        cex = 1, col = "white") #}
    # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
    #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
    # }
    } , bg.border = NA)
title("C", adj = 0, cex = 2)
circos.clear()


#####################
# some summary stats for writing
#####################

df_one <- df_drivers %>% 
    filter(tail != head) %>%
    group_by(link_from, link_to) %>%
    summarize(driv_sum = sum(paths, na.rm = TRUE)/2) %>% ungroup() %>%
    mutate(all= sum(driv_sum)) %>% # Divide by 2 because it's a symmetric matrix
    mutate(driv_prop = driv_sum / all) %>% 
    arrange(desc(driv_prop)) 
    
df_two <- df_domino %>% 
    filter(Tail != Head) %>%
    group_by(link_from, link_to) %>%
    summarize(dom_sum = sum(weight, na.rm = TRUE)) %>% ungroup() %>%
    mutate(all= sum(dom_sum)) %>% 
    mutate(dom_prop = dom_sum / all)%>% 
    arrange(desc(dom_prop))

df_three <- df_hidden %>% 
    filter(Tail != Head) %>%
    group_by(link_from, link_to) %>%
    summarize(hidden_sum = sum(inc, na.rm = TRUE)/2) %>% ungroup() %>%
    mutate(all= sum(hidden_sum)) %>% 
    mutate(hidden_prop = hidden_sum / all)%>%  # Divide by 2 because it's a symmetric matrix
    arrange(desc(hidden_prop))

df_one <- df_one %>%
    select(link_from, link_to, proportion = driv_prop) %>%
    add_column(class = "driver sharing")

df_two <- df_two %>%
    select(link_from, link_to, proportion = dom_prop) %>%
    add_column(class = "domino effects")

df_three <- df_three %>%
    select(link_from, link_to, proportion = hidden_prop) %>%
    add_column(class = "hidden feedbacks")

## I need to run the selection three times and then isolate the interesting variable
df_time <- bind_rows(df_one,df_two,df_three)
df_space <- bind_rows(df_one,df_two,df_three)
df_eco <-  bind_rows(df_one,df_two,df_three)

mat3 <- df_time %>% 
    unite(link, link_from, link_to) %>%
    select(link, proportion, class) %>%
    spread(., key = class, value = proportion) %>%
    select(-link) %>%
    as.matrix()

rownames(mat3) <- df_time %>% 
        unite(link, link_from, link_to) %>%
        select(link, proportion, class) %>%
        spread(., key = class, value = proportion) %>%
        pull(link)

## heatmap
library(iheatmapr)

hm3 <- main_heatmap(data = t(mat3)) %>%
    add_col_annotation(
        df_time %>% 
            spread(., key = class, value = proportion) %>%
            select(link_from, link_to),
        colors = list("Dark2", "Dark2")
    ) %>%
    add_row_labels()

hm3

## can you add the three?

hm %>%
    add_iheatmap(hm2)
## Nope

# Save the datasets and draft figures
save(hm, hm2, hm3, mat, mat2, mat3, df_time, df_space, df_eco,
     file = "heatmaps_cross_scales.RData")

save_iheatmap(hm, filename = "heatmap_space.png")
save_iheatmap(hm2, filename = "heatmap_eco.png" )
save_iheatmap(hm3, filename = "heatmap_time.png" )

