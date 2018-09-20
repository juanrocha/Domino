### Figures for Science paper
## Juan Rocha
## J180913

library(tidyverse)
library (network)
library (sna)
library(RColorBrewer)
library(ggplot2)
library(GGally)
library(viridis)
library(ggrepel)

# for plotting
library(ggmap)
library(maptools)
library(maps)
library(mapproj)

library(gridBase)
library(grid)
library(gridExtra)
library(png)


## for cleaning models
library(car)
library(broom)
library(sandwich); library(lmtest)

### ERGMS for Regime Shifts
library(ergm.count)

## for tables
library(kableExtra)

## load main datasets
# this loads the data, it was cleaned and prepared with script 170329_read_data.R
# and load the ergm models fitted with script 170329_ergms.R

load('~/Documents/Projects/Cascading Effects/170525_ergm_data.RData')


#### Figure 1. Scheme with minimal examples:
## Changes needed: thicker lines, change orientation to align all networks.

# load mini data
mini_dat <- readr::read_csv2("~/Documents/Projects/Cascading Effects/minimal_example_cld_cascading_effects.csv")

setwd("~/Documents/Projects/Cascading Effects/Figures")

# mini_dat %>% filter (Regime.Shift == 'rs1') %>%
#     select(tail, head, color) %>%
#     # network(directed = T, ignore.eval=FALSE, matrix.type = 'edgelist') %>%
#     # ggnet2(edge.color = "color", arrow.size = 1) # no curved edges, I need them
#     igraph::graph_from_data_frame(directed = TRUE) %>%
#     igraph::plot.igraph(edge.curved = c(0,0,0.5,0.5),
#         vertex.color = alpha(c(rep("red", 2), rep("purple",2)), 0.7 ),
#         vertex.frame.color = NA, vertex.label.color = "grey84",
#         layout = matrix(c(rep(0,4), 4,1,3,2), ncol = 2, nrow = 4),
#         main = "regime shift 1")


# quartz(width = 6, height = 4, pointsize = 7)
# par(mfrow = c(3,4), mar = c(2,2,2,2)) # layout(matrix(1:12,4,3, byrow=T))
# plot.new()
# # grid.newpage() # I need this line to plot on quartz, but not on markdown pdf.
# pushViewport(viewport(layout = grid.layout(3,4)))
#
# ## first row 2 clds
# pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
# par(fig = gridFIG(), new = TRUE)

quartz(width = 2, height=2, pointsize = 6)
mini_dat %>% filter (Regime.Shift == 'rs1') %>%
    select(tail, head, color) %>%
    # network(directed = T, ignore.eval=FALSE, matrix.type = 'edgelist') %>%
    # ggnet2(edge.color = "color", arrow.size = 1) # no curved edges, I need them
    igraph::graph_from_data_frame(directed = TRUE) %>%
    igraph::plot.igraph(edge.curved = c(0,0,0.5,0.5), edge.arrow.size = 0.5, edge.width = c(2,2,1,1), vertex.size = 25, vertex.label.cex = 2,
        vertex.color = alpha(c(rep("red", 2), rep("purple",2)), 1 ),
        vertex.frame.color = NA, vertex.label.color = "white",
        layout = matrix(c(4,1,3,2, 4,1,3,2), ncol = 2, nrow = 4), #layout_with_kk,
        margin = c(rep(-0.45,4))
        )

quartz.save("fig1a.png", type = "png", width = 2, height=2, pointsize = 7, dpi = 600, bg = "white", canvas = "white")

# title('a)', adj = 0, cex=5)

# popViewport()

# pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
# par(fig = gridFIG(), new = TRUE)
# # quartz(width = 2, height=2, pointsize = 6)
mini_dat %>% filter (Regime.Shift == 'rs2') %>%
    select(tail, head, color) %>%
    igraph::graph_from_data_frame(directed = TRUE) %>%
    igraph::plot.igraph(edge.curved = c(0,0.5,0.5), edge.arrow.size = 0.5,edge.width = c(2,1,1), vertex.size = 25, vertex.label.cex = 2,
        vertex.color = alpha(c(rep("red", 1), rep("purple",2)), 1 ),
        vertex.frame.color = NA, vertex.label.color = "white",
        layout = layout_with_kk ,
        margin = c(rep(-0.45,4))
        #matrix(c(rep(1,3), 3,2,1), ncol = 2, nrow = 3),
        )
quartz.save("fig1b.png", type = "png", width = 2, height=2, pointsize = 6, dpi = 300, bg = "white", canvas = "white")
# popViewport()

#
# pushViewport(viewport(layout.pos.row=1, layout.pos.col=3))
# par(fig = gridFIG(), new = TRUE)
quartz(width = 2, height=2, pointsize = 6)
mini_dat %>% filter (RS_joined == 'rs1_2') %>%
    select(tail, Regime.Shift, color) %>%
    # network(directed = T, ignore.eval=FALSE, matrix.type = 'edgelist') %>%
    # ggnet2(edge.color = "color", arrow.size = 1) # no curved edges, I need them
    igraph::graph_from_data_frame(directed = TRUE) %>%
    igraph::plot.igraph(edge.curved = FALSE, vertex.shape = c("circle", "circle", "square", "square"),
        vertex.color = alpha(c(rep("red", 2), rep("blue",2)), 1 ),
        vertex.size = 40, vertex.label.cex = 2,
        vertex.frame.color = NA, vertex.label.color = "white",
        edge.width = 2, edge.arrow.size = 0.5,
        layout = matrix(c(1,1,2,2, 2,1,2,1), ncol = 2, nrow = 4),
        margin = c(rep(-0.2,4))
    )
quartz.save("fig1c.png", type = "png", width = 2, height=2, pointsize = 6, dpi = 300, bg = "white", canvas = "white")
# popViewport()


m1 <- ggplot(
    data = as.sociomatrix(x, "paths") %>% as_tibble() %>% mutate(tail = rownames(as.sociomatrix(x))) %>%
        gather(key = head, value = paths,  1:30)  %>%
        mutate(tail = as.factor(tail), # %>% forcats::fct_rev(),
               head = as.factor(head) %>% forcats::fct_rev()),
    aes(tail, head)) +
    geom_tile(aes(fill = paths), show.legend = FALSE) + ylab("") + xlab("") +
    scale_fill_gradient2(low = "#FFA50080", mid = "gray84" ,high = "#0000FF80",
                         midpoint = 0, na.value = "grey50",
                         guide = guide_colorbar(title = " ", barheight = 3, barwidth = 0.3)) +
    theme_light (base_size=4) +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
          legend.position = c(0.9,0.5), legend.direction = "vertical")

# pushViewport(viewport(layout.pos.row=1, layout.pos.col=4))
# par(fig = gridFIG(), new = TRUE)
# print(m1, newpage = FALSE)
# popViewport()

## second panel
# pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
# par(fig = gridFIG(), new = TRUE)
# quartz(width = 2, height=2, pointsize = 6)
mini_dat %>% filter (Regime.Shift == 'rs3') %>%
    select(tail, head) %>%
    igraph::graph_from_data_frame(directed = TRUE) %>%
    igraph::plot.igraph(edge.curved = FALSE,
        vertex.color = alpha(c("red", rep("purple",3), "red"), 1 ), vertex.size = 25,  vertex.label.cex = 2,
        vertex.frame.color = NA, vertex.label.color = "white",
        edge.arrow.size = 0.5, edge.width = 1, margin = c(rep(-0.45,4))
        # layout = matrix(c(1,1,0.5,1,1, 4,3,2.5,2,1), ncol = 2, nrow = 5) ,
        )
quartz.save("fig1d.png", type = "png", width = 2, height=2, pointsize = 6, dpi = 300, bg = "white", canvas = "white")
# title ('b)', adj = 0, cex = 5)
# popViewport()

# pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
# par(fig = gridFIG(), new = TRUE)
mini_dat %>% filter (Regime.Shift == 'rs4') %>%
    select(tail, head) %>%
    igraph::graph_from_data_frame(directed = TRUE) %>%
    igraph::plot.igraph(edge.curved = FALSE,
        vertex.color = alpha(c("red", rep("purple",4)), 1 ), vertex.size = 25,
        vertex.frame.color = NA, vertex.label.color = "white",
        edge.width = 1, edge.arrow.size = 0.5, vertex.label.cex = 2,
        margin = c(rep(-0.45,4))
        #layout = matrix(c(0,0,-1,0,1, 3,2,1,0,1), ncol = 2, nrow = 5) ,
    )
quartz.save("fig1e.png", type = "png", width = 2, height=2, pointsize = 6, dpi = 300, bg = "white", canvas = "white")
# popViewport()

# pushViewport(viewport(layout.pos.row=2, layout.pos.col=3))
# par(fig = gridFIG(), new = TRUE)
mini_dat %>% filter (RS_joined == 'rs3_4') %>%
    select(tail, head, color) %>%
    igraph::graph_from_data_frame(directed = TRUE) %>%
    igraph::plot.igraph(edge.curved = FALSE, edge.arrow.size = 0.5,
        vertex.color = alpha(c("red","gray","red", "gray", "red", rep("gray",4)), 1 ),
        edge.width = c(rep(1,5),2,rep(1,4)),vertex.size = 25, vertex.label.cex = 2,
        vertex.frame.color = NA, vertex.label.color = "white",
        margin = c(rep(-0.45,4))
        #layout = matrix(c(0,0,1,0,0,1,2,3,2,  4,3,2.5,2,1, 2.5, 1, 2.5, 3), ncol = 2, nrow = 9) ,
    )
quartz.save("fig1f.png", type = "png", width = 2, height=2, pointsize = 6, dpi = 300, bg = "white", canvas = "white")
# popViewport()

m2 <- ggplot(
    data = out %>%
         mutate(Tail = as.factor(Tail), # %>% forcats::fct_rev(),
               Head = as.factor(Head) %>% forcats::fct_rev()),
    aes( Head, Tail)) +
    geom_tile(aes(fill = weight), show.legend = FALSE) + ylab("") + xlab("") +
    scale_fill_gradient(high = "red", low = "gray84", na.value = "grey50",
                         guide = guide_colorbar(title = " ", barheight = 3, barwidth = 0.3)) +
    theme_light(base_size=4) +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
          legend.position = c(0.9,0.5), legend.direction = "vertical")

# pushViewport(viewport(layout.pos.row=2, layout.pos.col=4))
# par(fig = gridFIG(), new = TRUE)
# print(m2, newpage = FALSE)
# popViewport()




## third row

# pushViewport(viewport(layout.pos.row=3, layout.pos.col=1))
# par(fig = gridFIG(), new = TRUE)
mini_dat %>% filter (Regime.Shift == 'rs5') %>%
    select(tail, head) %>%
    igraph::graph_from_data_frame(directed = TRUE) %>%
    igraph::plot.igraph(edge.curved = c(0,0.5,0.5), edge.arrow.size = 0.5,
                        edge.width = 1,
        vertex.color = alpha(c("red", rep("purple",2)), 1), vertex.size = 25,
        vertex.frame.color = NA, vertex.label.color = "white",
        vertex.label.cex = 2, margin = c(rep(-0.45,4)),
        layout = layout_with_kk #matrix(c(1,1,1, 3,2,1), ncol = 2, nrow = 3) ,
        )
quartz.save("fig1g.png", type = "png", width = 2, height=2, pointsize = 6, dpi = 300, bg = "white", canvas = "white")
# title ('c)', adj = 0, cex = 5)
# popViewport()
#
# pushViewport(viewport(layout.pos.row=3, layout.pos.col=2))
# par(fig = gridFIG(), new = TRUE)
mini_dat %>% filter (Regime.Shift == 'rs6') %>%
    select(tail, head) %>%
    igraph::graph_from_data_frame(directed = TRUE) %>%
    igraph::plot.igraph(edge.curved = c(0,0.5,0.5), edge.arrow.size = 0.5, edge.width = 1, vertex.label.cex = 2,
        vertex.color = alpha(c("red", rep("purple",2)), 1 ), vertex.size = 25,
        vertex.frame.color = NA, vertex.label.color = "white",
        layout = layout_with_kk , margin = c(rep(-0.45,4))
        #matrix(c(1,1,1, 1,2,3), ncol = 2, nrow = 3),
        )
quartz.save("fig1h.png", type = "png", width = 2, height=2, pointsize = 6, dpi = 300, bg = "white", canvas = "white")
# popViewport()
#
# pushViewport(viewport(layout.pos.row=3, layout.pos.col=3))
# par(fig = gridFIG(), new = TRUE)
mini_dat %>% filter (RS_joined == 'rs5_6') %>%
    select(tail, head, color) %>%
    igraph::graph_from_data_frame(directed = TRUE) %>%
    igraph::plot.igraph(edge.curved = -0.8, edge.arrow.size = 0.5,
        vertex.color = alpha(c("red","purple","red", "purple"), 1 ),
        edge.width = c(2,2,1,2,2,1), vertex.size = 25,
        vertex.frame.color = NA, vertex.label.color = "white",
        vertex.label.cex = 2, margin = c(rep(-0.45,4))
        #layout = matrix(c(0,0,1,0,0,1,2,3,2,  4,3,2.5,2,1, 2.5, 1, 2.5, 3), ncol = 2, nrow = 9) ,
        )
quartz.save("fig1i.png", type = "png", width = 2, height=2, pointsize = 6, dpi = 300, bg = "white", canvas = "white")
# popViewport()

m3 <- ggplot(data = df_inc %>%
        mutate(Tail = as.factor(Tail) %>% forcats::fct_rev(),
          Head = as.factor(Head)), #%>% forcats::fct_rev()),
         aes(Tail, Head)) +
    geom_tile(aes(fill = log(inc)), show.legend = FALSE) + ylab("") + xlab("") +
    scale_fill_gradient(low = "#FFA50080", high = "#0000FF80", na.value = "grey50",
                        guide = guide_colorbar(title = "[log]", barheight = 3, barwidth = 0.3)) +
    theme_light(base_size=4) +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
          legend.position = c(0.9,0.5), legend.direction = "vertical")


# pushViewport(viewport(layout.pos.row=3, layout.pos.col=4))
# par(mar=c(2,2,2,2))
# print(m3, newpage = F)
# popViewport()

# detach(package:igraph)

# quartz.save(file="methods_scheme.png", width=7, height=7, pointsize = 7, dpi = 300)

### Attempt with GGally
## First recover the png networks into ggplots objects.
p1 <- ggplot(data = data_frame(x = 0, y= 0), aes(x,y)) + geom_blank() +
    theme_void() +
    annotation_custom(
        grob = grid::rasterGrob( image = readPNG(
            "~/Documents/Projects/Cascading Effects/Figures/fig1a.png"), interpolate = TRUE),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
        )

p2 <- ggplot(data = data_frame(x = 0, y= 0), aes(x,y)) + geom_blank() +
    theme_void() +
    annotation_custom(
        grob = grid::rasterGrob( image = readPNG(
            "~/Documents/Projects/Cascading Effects/Figures/fig1b.png"), interpolate = TRUE),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
        )

p3 <- ggplot(data = data_frame(x = 0, y= 0), aes(x,y)) + geom_blank() +
    theme_void() +
    annotation_custom(
        grob = grid::rasterGrob( image = readPNG(
            "~/Documents/Projects/Cascading Effects/Figures/fig1c.png"), interpolate = TRUE),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
        )
p4 <- ggplot(data = data_frame(x = 0, y= 0), aes(x,y)) + geom_blank() +
    theme_void() +
    annotation_custom(
        grob = grid::rasterGrob( image = readPNG(
            "~/Documents/Projects/Cascading Effects/Figures/fig1d.png"), interpolate = TRUE),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
        )

p5 <- ggplot(data = data_frame(x = 0, y= 0), aes(x,y)) + geom_blank() +
    theme_void() +
    annotation_custom(
        grob = grid::rasterGrob( image = readPNG(
            "~/Documents/Projects/Cascading Effects/Figures/fig1e.png"), interpolate = TRUE),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
        )

p6 <- ggplot(data = data_frame(x = 0, y= 0), aes(x,y)) + geom_blank() +
    theme_void() +
    annotation_custom(
        grob = grid::rasterGrob( image = readPNG(
            "~/Documents/Projects/Cascading Effects/Figures/fig1f.png"), interpolate = TRUE),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
        )
p7 <- ggplot(data = data_frame(x = 0, y= 0), aes(x,y)) + geom_blank() +
    theme_void() +
    annotation_custom(
        grob = grid::rasterGrob( image = readPNG(
            "~/Documents/Projects/Cascading Effects/Figures/fig1g.png"), interpolate = TRUE),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
        )

p8 <- ggplot(data = data_frame(x = 0, y= 0), aes(x,y)) + geom_blank() +
    theme_void() +
    annotation_custom(
        grob = grid::rasterGrob( image = readPNG(
            "~/Documents/Projects/Cascading Effects/Figures/fig1h.png"), interpolate = TRUE),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
        )

p9 <- ggplot(data = data_frame(x = 0, y= 0), aes(x,y)) + geom_blank() +
    theme_void() +
    annotation_custom(
        grob = grid::rasterGrob( image = readPNG(
            "~/Documents/Projects/Cascading Effects/Figures/fig1i.png"), interpolate = TRUE),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
        )


plot_list <- list(p1,p2,p3, m1, p4,p5,p6,m2, p7,p8,p9,m3)
pp <- ggmatrix(
    plot_list, ncol = 4, nrow = 3,
    xAxisLabels = c("regime shift 1", "regime shift 2", "joint regime shifts", "response variable"),
    yAxisLabels = c("drivers sharing", "domino effects", "hidden feedbacks"),
    byrow = TRUE, showAxisPlotLabels = FALSE #, switch = 'y'
)

quartz(width = 4.75, height = 3.75, pointsize = 7)
pp + theme(strip.text = element_text(size = 7))
ggsave("Cascading_Fig1.pdf", device = "pdf")

#################################################
####### Figure 2
#################################################

## note that from the ergms script, x is the one mode network of the bipartite network
#x <- mode.1(bip1)[[2]] # x is the regime shifts one-mode network
z <- mode.1(bip1)[[1]]
# and
# bip.edgelist <- bind_rows(bip.edgelist)
# bipmat <- as.matrix(table(bip.edgelist))

df_rs_type <- tibble(
    tail = rsdb$name,
    type = c("Aquatic", "Earth", "Aquatic", "Terrestrial", "Terrestrial", "Aquatic",
             "Terrestrial", "Aquatic", "Aquatic", "Terrestrial", "Aquatic", "Earth",
             "Aquatic", "Aquatic", "Land-water\n interface", "Aquatic",
             "Aquatic", "Earth", "Land-water\n interface", "Aquatic","Land-water\n interface",
             "Land-water\n interface", "Aquatic","Terrestrial","Terrestrial","Terrestrial",
             "Earth","Land-water\n interface", "Terrestrial", "Earth")
)

g1 <- ggplot(
    data = as.sociomatrix(x, "paths") %>%
        as_tibble() %>%
        mutate(tail = rownames(as.sociomatrix(x))) %>%
        gather(key = head, value = paths,  1:30) %>%
        left_join(df_rs_type) %>%
        mutate(tail = as.factor(tail)) %>%
        mutate(tail = forcats::fct_reorder(tail, paths, fun = sum, .desc = FALSE)),
    aes(tail, paths, fill = type, color = type)) +
    geom_boxplot(outlier.alpha = 0.5, alpha=0.5,
                 outlier.size = 0.8, size = 0.1) +
    geom_point(
        data = bip.edgelist %>% mutate(value = 1) %>%
            group_by(rs) %>%
            summarize(paths = sum(value)) %>%
            left_join(df_rs_type, by = c("rs" = "tail")) ,
        aes(rs, paths),
        color = "black", size = 1, show.legend = FALSE) +
    scale_fill_manual(
        "Ecosystem type",values = c("blue", "purple", "red","orange")) +
    scale_color_manual(
        "Ecosystem type",values = c("blue", "purple", "red","orange")) +
    coord_flip() +  xlab("Regime shifts") + ylab("Number of drivers") + labs(tag = "A") +
    theme_minimal(base_size = 5) +
    theme(legend.position = c(0.85, 0.2),
        #title = element_text(size = 9, face = "bold"),
          legend.background = element_rect(fill = "white", linetype = 0),
          legend.text = element_text(size = 5), legend.title = element_text(size = 5), legend.key.size = unit(1, 'line'),
    )

# g2 <- ggplot(
#     data = as.sociomatrix(z, "paths") %>% as_tibble() %>% mutate(tail = rownames(as.sociomatrix(z))) %>%
#         gather(key = head, value = paths,  1:79) %>%
#         group_by(tail) %>% filter(mean(paths) >  1 ) %>% ungroup() %>%
#         mutate(tail = as.factor(tail)) %>%
#         mutate(tail = forcats::fct_reorder(tail, paths, fun = sum, .desc = TRUE)),
#     aes(tail, paths)) +
#     geom_boxplot(
#         fill = alpha("grey", 0.9), color = "black", outlier.alpha = 0.5,
#         outlier.size = 0.8, size = 0.1) +
#     geom_point(
#         data = bip.edgelist %>% mutate(value = 1) %>%
#             group_by(drivers) %>%
#             summarize(paths = sum(value)) %>% filter(paths > 5) %>%
#             mutate(tail = as.factor(drivers)) %>%
#             mutate(tail = forcats::fct_reorder(tail, paths, fun = sum, .desc = TRUE)),
#         aes(tail, paths), color = "purple"
#     ) +
#     coord_flip() +  xlab("Most shared drivers\n [mean > 1]") + ylab("Number of regime shifts") +  ggtitle("B") +
#     theme_minimal(base_size = 6)


g2 <- ggplot(
    data = bip.edgelist %>% mutate(value = 1) %>%
                group_by(drivers) %>%
                summarize(paths = sum(value)) %>% filter(paths > 5) %>%
                mutate(tail = as.factor(drivers)) %>%
                mutate(tail = forcats::fct_reorder(tail, paths, fun = sum, .desc = FALSE)),
    aes(tail, paths) ) +
    geom_col(fill = "grey", color = "white", alpha = 0.9) + coord_flip() +
    # geom_boxplot(
    #     data = as.sociomatrix(z, "paths") %>% as_tibble() %>% mutate(tail = rownames(as.sociomatrix(z))) %>%
    #             gather(key = head, value = paths,  1:79) %>%
    #             group_by(tail) %>% filter(mean(paths) >  1 ) %>%
    #             ungroup() %>%
    #             mutate(tail = forcats::fct_reorder(tail, paths, fun = sum, .desc = FALSE)) %>% droplevels() ,
    #     aes(tail, paths), fill = alpha("grey"), color = "black",
    #     outlier.alpha = 0.5, outlier.size = 0.8, size = 0.1) +
    xlab("Most shared drivers") +
    ylab("Number of regime shifts") +  labs(tag="B") +
    theme_minimal(base_size = 5)



m1 <- ggplot(
    data = as.sociomatrix(x, "paths") %>% as_tibble() %>% mutate(tail = rownames(as.sociomatrix(x))) %>%
        gather(key = head, value = paths,  1:30)  %>%
        mutate(tail = as.factor(tail), # %>% forcats::fct_rev(),
               head = as.factor(head) %>% forcats::fct_rev()),
    aes(tail, head)) +
    geom_tile(aes(fill = paths)) +
    ylab("") + xlab("") + ggtitle("Drivers sharing") +
    scale_fill_gradient(low = "gray84" ,high = "#0000FF80", na.value = "grey50",
                         guide = guide_colorbar("shared drivers", barheight = 0.7)) +
    theme_light (base_size = 6) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ),
          legend.position = "bottom")

## Heatmap with ggplot
m2 <- ggplot(
    data = out %>%
         mutate(Tail = as.factor(Tail) %>% forcats::fct_rev(),
               Head = as.factor(Head)), #%>% forcats::fct_rev()),
    aes( Head, Tail)) +
    geom_tile(aes(fill = weight)) + ggtitle("Domino effects") +
    ylab("Independent regime shift") + xlab("Dependent regime shift") +
    scale_fill_gradient(high = "red", low = "gray84", na.value = "grey50",
                         guide = guide_colorbar("domino effects", barheight = 0.7))  +
    theme_light(base_size = 6) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ),
          legend.position = "bottom")

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

df_poly <- data_frame(
    id = rep(factor(c("1", "2")), each = 3),
    x = c(0,0,10, 0,25,25),
    y = c(0,25,25, 0,10,0),
    value = as.factor(c(rep(alpha("orange", 0.4),3), rep(alpha("green", 0.4),3)))
)

g3 <- ggplot(
    data = df_domino %>%
        left_join(df_rs_type, by = c("f" = "tail")) ,
    aes(y = outdegree, x = indegree)) +
    geom_point( aes(color = type), size = 1.5, show.legend = FALSE, alpha = 0.5) +
    # geom_errorbar(aes(ymin = out_avg, ymax = out_max, color = type)) +
    # geom_errorbarh(aes(xmin = in_avg, xmax = in_max, color = type)) +
    geom_text_repel(
        data = df_domino %>%
            filter(out_max > 3 | in_max > 3),
        aes(label = f), size = 1.5) +
    #geom_polygon(data = df_poly,  mapping = aes(x, y, group = id, fill = value), show.legend = FALSE, position = "identity", stat = "identity", inherit.aes = FALSE) +
    #scale_fill_manual(guide = "none", values = c(alpha("orange", 0.2),alpha("green", 0.2))) +
    scale_color_manual("Ecosystem type",values = c("blue", "purple", "red","orange")) +
    labs(x = "Dependent regime shifts\n [Indegree]", y = "Independent regime shifts\n [Outdegree]", tag = 'C') +
    theme_minimal(base_size = 5) #+
    # theme(legend.position = c(0.8, 0.8),
    # #title = element_text(size = 9, face = "bold"),
    #       legend.background = element_rect(fill = "white", linetype = 0),
    #       legend.text = element_text(size = 5), legend.title = element_text(size =5)
    #       )


g4 <- ggplot(
    out %>% filter(weight > 0) %>%
        pull(driv2feed) %>% stringr::str_split(pattern = ", ") %>%
        unlist() %>% as_tibble() %>% rename(driver = value) %>%
        group_by(driver) %>% summarize(n = n()) %>% ungroup() %>%
        mutate(driver = as_factor(driver)) %>%
        mutate(driver = fct_reorder(driver, n, .desc = FALSE)) %>%
        filter(n > 1),
    aes(driver, n)) +
    geom_bar(stat = "identity", fill = alpha("grey", 0.9), color = "white", size = 0.1) +
    coord_flip() + ylab("Number of domino effects") + xlab("Key variables")+
    labs(tag='D') + theme_minimal(base_size = 5)



## Heatmap with ggplot
m3 <- ggplot(
    data = df_inc %>%
         mutate(Tail = as.factor(Tail) %>% forcats::fct_rev(),
               Head = as.factor(Head)), #%>% forcats::fct_rev()),
    aes( Head, Tail)) +
    geom_tile(aes(fill = log(inc)))  + ggtitle("Hidden feedbacks") +
    ylab("") + xlab("") +
    scale_fill_gradient(low = "#FFA50080", high = "#0000FF80", na.value = "grey50",
                        guide = guide_colorbar("hidden feedbacks [log]", barheight = 0.7)) +
    theme_minimal(base_size = 6) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ),
          legend.position = "bottom")  +
    inset(
        grob = ggplotGrob(
            ggplot(data = df_inc, aes(inc)) +
                geom_histogram() + xlab('Hidden feedbacks') +
                theme_light(base_size = 6) ) , xmin = 0, xmax =15 , ymin = 0, ymax = 15)

## I need to make the matrix symmetric so it does count the degrees correctly.
df_inc2 <- data.frame(inc = inc, Head = levels(dat$Regime.Shift)[key[1,]], Tail = levels(dat$Regime.Shift)[key[2,]])

df_inc3 <- bind_rows(df_inc, df_inc2)

g5 <- ggplot(
    data = df_inc3 %>%
        mutate(Tail = as.factor(Tail), Head = as.factor(Head)) %>%
        left_join (df_rs_type, by = c("Tail" = "tail")) %>%
        mutate(Tail = fct_reorder(Tail, inc, fun = mean, .desc = FALSE )) %>% filter(inc > 0)  ,
    aes(Tail, inc, color = type, fill = type)) +
    geom_boxplot( outlier.alpha = 0.5, alpha = 0.5,
                 outlier.size = 0.8, show.legend = FALSE, size = 0.1) +
    # geom_point(
    #     data = df_inc3 %>%
    #         group_by(Tail) %>%
    #         summarize(outdegree = sum(inc > 0)),
    #     aes(Tail, outdegree), color = "orange", size = 1.5, shape = 12
    # ) +
    scale_y_log10()+
    scale_fill_manual("Ecosystem type",values = c("blue", "purple", "red","orange")) +
    scale_color_manual("Ecosystem type",values = c("blue", "purple", "red","orange")) +
    coord_flip() +
    xlab("Regime shifts") + ylab("Hidden feedbacks") + labs(tag="E") +
    theme_minimal(base_size = 5) #+
    #theme(legend.position = "right", #c(0.85, 0.2),
    #title = element_text(size = 9, face = "bold"),
          # legend.background = element_rect(fill = "white", linetype = 0),
          # legend.text = element_text(size = 4), legend.title = element_text(size = 3)
          # )

z <- bind_rows(out_dat)

z <- gather(z,`Inconvenient`,`Expected`, key = Feedbacks, value = count)

z$Feedbacks <- factor(z$Feedbacks)
levels(z$Feedbacks)[2] <- "Hidden"

## J171213: Alternative to g6

 brain <- network(select(dat, Tail, Head, Polarity, col), directed = T, ignore.eval=FALSE, matrix.type = 'edgelist')
 df_brain <- tibble(
     variable = network.vertex.names(brain),
     betweenness = sna::betweenness(brain, cmode = "directed", rescale = TRUE),
     indeg = sna::degree(brain, cmode ="indegree"),
     outdeg = sna::degree(brain, cmode = "outdegree"),
     closeness = sna::closeness(brain, cmode = "suminvdir", rescale = TRUE))

g6 <- ggplot(data = df_brain, aes(betweenness, closeness)) +
    geom_point(#aes(color = indeg, fill = outdeg),
            color = "grey", fill = "grey",
               shape = 19, size = 1, alpha = 0.5) +
    geom_text_repel(data = filter(df_brain, betweenness > 0.015 | closeness > 0.0052),
                    aes(label = variable), size = 1.5) +
    #scale_color_gradient(low = "#6495ED", high = "#00008B", guide = "colorbar", name = " Indegree") +
    #scale_fill_gradient(low = "#FFD700", high = "#8B0000", guide = "colorbar", name =  "Outdegree") +
    theme_minimal(base_size = 5) + labs(tag="F") +
    xlab("Betweenness") + ylab("Closeness")
    # theme(legend.position = "right", legend.direction = "vertical",
    # #title = element_text(size = 9, face = "bold"),
    #       legend.text = element_text(size = 4), legend.title = element_text(size = 4)) #+
    # inset(
    #     grob = ggplotGrob(
    #         ggnet2(brain, size=1, arrow.size = 1.5, arrow.gap = 0.01, edge.alpha = 0.5,
    #                edge.size = 0.1)
    #     ), xmin = 0.015, xmax =0.04 , ymin = 0, ymax = 0.004
    # )


### Insert figures with models here.
## Models for sharing drivers
df1 <- tidy(fit.w1); df1$model <- 'Driver sharing'
df1$term <- c("Non zero",'Sum', "Land use","Ecosystem type", "Ecosystem processes", "Provisioning services","Regulating services", "Cultural services", "Human well-being", "Spatial scale: national", "Spatial scale: subcontinental", "Temporal scale: month - year", "Temporal scale: week - month", "Temporal scale: year - decade", "Match spatial scale", "Match temporal scale", "Reversibility", "Evidence type")
## model for domino effects
df2 <- tidy(fit.w2); df2$model <- 'Domino effects'
df2$term <- c("Non zero",'Sum', "Land use","Ecosystem type", "Ecosystem processes", "Provisioning services","Regulating services", "Cultural services", "Human well-being",
               #"Indegree - Spatial scale: national", "Indegree - Spatial scale: subcontinental", "Indegree - Temporal scale: month - year", "Indegree - Temporal scale: week - month", "Indegree - Temporal scale: year - decade",
               #"Outdegree - Spatial scale: national", "Outdegree - Spatial scale: subcontinental", "Outdegree - Temporal scale: month - year", "Outdegree - Temporal scale: week - month", "Outdegree - Temporal scale: year - decade",
              #"Match spatial scale: local","Match spatial scale: national","Match spatial scale: sub-continental", "Match temporal scale",
              "Spatial scale", "Temporal scale",
              "Reversibility", "Evidence type")
## model for hidden feedbacks
df3 <- tidy(fit.w3a); df3$model <- 'Hidden feedbacks'
df3$term <- c("Non zero",'Sum', "Land use","Ecosystem type", "Ecosystem processes", "Provisioning services","Regulating services", "Cultural services", "Human well-being",
              "Spatial scale: national", "Spatial scale: subcontinental", "Temporal scale: month - year", "Temporal scale: week - month", "Temporal scale: year - decade", "Match spatial scale", "Match temporal scale",
              "Reversibility", "Evidence type")

df <- full_join(df1, df2) %>% full_join(df3) %>% as_tibble()
# df <-df %>% mutate(estimate = exp(estimate), std.error = exp(std.error)) ## In case you want to read probabilities directly instead of log odds
df <- mutate(df, conf.hi = estimate + std.error, conf.low = estimate - std.error)
df$P <- ifelse(df$p.value <= 0.05, "< 0.05", "> 0.05")
df$term <- as.factor(df$term)

# ## change names of places and reorder levels
df$term <- factor(df$term, levels(df$term)[c(9,16,6,3,2,10,11,1,5, 13,17,14,15,19,18,20,7,8, 12,4)] )

### include another category to explain the type of terms (Garry suggestion):
df_term_type <- data_frame(
    term = as_factor(levels(df$term)),
    type = fct_rev(as_factor(c(rep("Context",4), rep("Impacts", 5), rep("Cross-scale interactions", 9), rep("Context",2))) )
)

df <- df %>% left_join(df_term_type)

# df$term <- factor(df$term, levels(df$term)[c(17, 28, 11,3,2,23,24,1,5, 26,27, 30,29,31, 6:10,18:22,  12:16, 25,4)] )
# df$term <- factor(df$term, rev(levels(df$term)))

df$model <- as.factor(df$model)
df$model <- factor(df$model, levels(df$model)[c(2,1,3)])
# df$type <- factor(df$type, levels(df$type)[3:1])

### model results move down for panel figure 3

## Combine the figure old way
# quartz(width = 4.75, height = 6, pointsize = 6)
# gg <- list (g1,g3,g5,g2,g4,g6, p)
# source('~/Dropbox/Code/multiplot.R')
# layout <- matrix(c(1:6,7,7,8), ncol = 3, nrow = 3, byrow = T)
# multiplot(plotlist = gg, layout = layout)


## New way:
gg <- list (g1,g2, g3, g4, g5, g6)
source('~/Dropbox/Code/multiplot.R')
layout <- matrix(c(1:6), ncol = 2, nrow = 3, byrow = T)
quartz(width = 4.75, height = 6, pointsize = 5)
multiplot(plotlist = gg, layout = layout)


# ggsave("Cascading_Fig2.pdf", plot = multiplot(plotlist = gg, layout = layout))
quartz.save("Cascading_Fig2.pdf", type = "pdf", dpi = 600, bg = "white")


### For SI, make a worked examples with two real networks.


###############################################################
#### Figure 3
###############################################################\


df_fork <- as.sociomatrix(x, "paths") %>% as_tibble() %>% mutate(tail = rownames(as.sociomatrix(x))) %>%
        gather(key = head, value = drivers,  1:30) %>% mutate(tail = as.factor(tail) %>% forcats::fct_rev()) %>%
    mutate(drivers = ifelse(drivers > 0, 1, 0))

df_domino <- as_tibble(out) %>% rename(tail = Tail, head = Head) %>%
    mutate(domino = ifelse(weight > 0, 1, 0)) %>% select(-driv2feed, -weight)
df_hidden <- as_tibble(df_inc3) %>% rename(tail = Tail, head = Head) %>%
    mutate(hidden = ifelse(inc > 0, 1, 0)) %>% select(-inc)


df_all <- left_join(df_fork, df_domino) %>% left_join(df_hidden) %>%
    filter(tail != head) %>%
    group_by(tail,head) %>%
    mutate(
        all = sum(drivers, domino, hidden),
        type = ifelse(is.na(all), 'None',
            ifelse(all == 3, "All",
                ifelse(all == 2 && drivers == 1 && domino == 1, "Domino effect and\n driver sharing",
                ifelse(all == 2 && drivers == 1 && hidden == 1, "Hidden feedback and\n driver sharing",
                ifelse(all == 2 && hidden == 1 && domino == 1, "Domino effect and\n hidden feedback",
                ifelse(all == 1 && drivers == 1, "Driver sharing",
                ifelse(all == 1 && domino == 1, "Domino effect",
                ifelse(all == 1 && hidden == 1, "Hidden feedback", "None")
                ))))))
           )
    )  %>% ungroup() %>%
    mutate(
        type = as.factor(type),
        num = 1) %>%
    # mutate(type = forcats::fct_reorder(.f = type, .x = num, .fun = sum, .desc = FALSE)) %>%
    mutate(
        type = forcats::fct_relevel(.f = type,
            "None", "Driver sharing", "Domino effect", "Hidden feedback",
            "Hidden feedback and\n driver sharing",
            "Domino effect and\n driver sharing",
            "Domino effect and\n hidden feedback",
            "All"
        )
    )


# data = df_all, aes(type)
g7 <- ggplot(data = df_all, aes(type)
    # data = df_all %>%
    # group_by(type) %>%
    # summarize(
    #     count = sum(num),
    #     percent = sum(num)/870
# ) %>%
) +
    geom_bar(aes(fill = type), show.legend = FALSE) + coord_flip() +
    scale_fill_manual(
        values = rev(brewer.pal(11, "RdYlBu")[-c(9,10,11)])
    ) +
    labs(tag = "B") +
    xlab("") + ylab("Pair-wise\n regime shifts")+
    theme_minimal(base_size = 6)


g8 <- df_all %>%
    mutate(dep = ifelse(
    type == "None" | type == "Driver sharing",
    "Structurally\n independent", "Structural\n dependency")) %>%
    group_by(type, dep) %>%
    summarize(N = sum(num), prop = N/870) %>%
    ggplot(aes(dep, prop)) +
    geom_col(aes(fill = type), position = "stack", show.legend = FALSE) + coord_flip() + xlab("") + ylab("") +
    scale_y_continuous(labels = scales::percent ) +
    scale_fill_manual(
        values = rev(brewer.pal(11, "RdYlBu")[-c(9,10,11)])
    ) + labs(tag = "C") +
    theme_minimal(base_size = 6)

#
# df_all$type <- as.factor(df_all$type) %>% forcats::fct_relevel(
#     "drivers sharing", "domino effect", "hidden feedback", "multiple", "all") %>%
#     forcats::fct_rev() # %>%
#     #fct_collapse(multiple = c('all', 'multiple')) #%>%
#     # fct_explicit_na(na_level = "other") %>%
#     # fct_collapse(other = c("drivers sharing", "other"))

m4 <- df_all %>% ungroup() %>%
    mutate(tail = as.factor(tail) %>% forcats::fct_rev(),
               head = as.factor(head)) %>%
    ggplot(aes(y = tail, x = head)) +
    geom_tile(aes(fill= type), show.legend = TRUE) +
    scale_fill_manual(
        #type = "seq", palette = "RdYlBu", #direction = -1, na.value = "gray57",
        values = rev(brewer.pal(11, "RdYlBu")[-c(9,10,11)]),
        guide = guide_legend(title = "Cascading effects", barwidth = 0.3)) +
    theme_light(base_size = 6) + xlab('') + ylab('') + labs(tag = "A") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ),
          legend.position = c(-0.35 ,- 0.3),
      legend.key.size = unit(0.25,"cm"))

quartz(width = 4.75, height = 3, pointsize = 6)
gg <- list(m4, g7, g8)
layout <- matrix(c(rep(1,6), 2,2,3), ncol = 3, nrow = 3, byrow = FALSE)
multiplot(plotlist = gg, layout = layout)


quartz.save("Cascading_FigSummary.pdf", type = "pdf", dpi = 600, bg = "white")
###### Circlos


library(circlize)
library(shape)

quartz(width=4.75, height = 3, family = 'Helvetica', pointsize = 8)
layout(matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE))
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

## First legend
par(mar = c(rep(0.5,4)))
plot.new()
legend(
    "center",
    title = "Ecosystem types",
    legend = c("Aquatic", "Earth", "Land-water\n interface", "Terrestrial"),
    fill = c("blue", "purple", "red","orange"),
    border = c("blue", "purple", "red","orange"),
    bty = "n"
)

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
        # circos.text(mean(xlim), mean(ylim), sector.name, facing = "bending.outside", niceFacing = TRUE, #adj = c(0.5, 0),
        # cex = 1, col = "white") #}
    # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
    #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
    # }
    } , bg.border = NA)
title("B", adj = 0, cex = 2, font = 1) # font 1 is plain text, 2 is bold, 3 italics... see par()
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
        # circos.text(mean(xlim), mean(ylim), sector.name, facing = "bending.outside", niceFacing = TRUE, #adj = c(0.5, 0),
        # cex = 1, col = "white") #}
    # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
    #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
    # }
    } , bg.border = NA)
title("C", adj = 0, cex = 2, font =1)
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
        # circos.text(mean(xlim), mean(ylim), sector.name, facing = "bending.outside", niceFacing = TRUE, #adj = c(0.5, 0),
        # cex = 1, col = "white") #}
    # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
    #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
    # }
    } , bg.border = NA)
title("D", adj = 0, cex = 2, font = 1)
circos.clear()

#################
# Figure with scales
#################

df_scales <- data_frame(
    rs = x %v% "vertex.names",
    space = x %v% "space_range",
    time = x %v% "time_range"
)

## second legend
par(mar = c(rep(1,4)))
plot.new()
legend(
    "center",
    title = "Spatial scales",
    legend = c("Local", "National", "Subcontinental"),
    fill = brewer.pal(4, "RdBu")[-4],
    border = brewer.pal(4, "RdBu")[-4], bty = "n"
)
### Space
## make a drivers df again including the scales:
df_drivers <- x %>%  # x is the netowrk of shared drivers.
    as.sociomatrix(., "paths") %>% as_tibble() %>%
    mutate(tail = rownames(as.sociomatrix(x))) %>%
    gather(key = head, value = paths,  1:30) %>%
    mutate(tail = as.factor(tail)) %>%
    mutate(tail = forcats::fct_reorder(
        tail, paths, fun = sum, .desc = FALSE)) %>%
    left_join(df_scales, by = c("tail" = "rs"))%>% rename(link_from = space) %>%
    left_join(df_scales, by= c("head" = "rs")) %>% rename(link_to = space) %>%
    filter(paths > 0) %>%
    arrange(link_from) %>%
    select(link_from, link_to, paths, tail, head)

df_domino <- as_tibble(out) %>%
    select(Tail, Head, weight) %>%
    left_join(df_scales, by = c("Tail"="rs")) %>%
    rename(link_from = space) %>%
    left_join(df_scales, by = c("Head"="rs")) %>%
    rename(link_to = space) %>%
    arrange(link_from) %>%
    select(link_from, link_to, weight, Tail, Head)

df_hidden <- as_tibble(df_inc3) %>%
    select(Tail, Head, inc) %>%
    left_join(df_scales, by = c("Tail"="rs")) %>%
    rename(link_from = space) %>%
    left_join(df_scales, by = c("Head"="rs")) %>%
    rename(link_to = space) %>%
    arrange(link_from) %>%
    select(link_from, link_to, inc, Tail, Head)

## Figure 6d
par(mar = c(rep(1,4)))
# circos.par(cell.padding = c(rep(0,4)))
chordDiagram(
    df_drivers, annotationTrack = c("grid"),
    annotationTrackHeight = c( 0.1),
    #preAllocateTracks = list(track.height = 0.1),
    #directional = 1, direction.type = "arrows",link.arr.type = "big.arrow",
    link.arr.lwd = NA , grid.col = brewer.pal(4, "RdBu")[-4]
)

## from tutorial
circos.trackPlotRegion(track.index = 1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
    #if(abs(xplot[2] - xplot[1]) < 20) {
        # circos.text(mean(xlim), mean(ylim), sector.name, facing = "bending.outside", niceFacing = TRUE, #adj = c(0.5, 0),
        # cex = 1, col = "white") #}
    # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
    #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
    # }
    } , bg.border = NA)
title("E", adj = 0, cex = 2, font = 1)
circos.clear()


## Figure 6e
par(mar = c(rep(1,4)))
# circos.par(cell.padding = c(rep(0,4)))
chordDiagram(
    df_domino, annotationTrack = "grid",
    annotationTrackHeight = c( 0.1),
    #preAllocateTracks = list(track.height = 0.1),
    directional = 1, direction.type = "diffHeight+arrows",link.arr.type = "big.arrow",
    link.arr.lwd = NA, grid.col = brewer.pal(4, "RdBu")[-4])

## from tutorial
circos.trackPlotRegion(track.index = 1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
    #if(abs(xplot[2] - xplot[1]) < 20) {
        # circos.text(mean(xlim), mean(ylim), sector.name, facing = "bending.outside", niceFacing = TRUE, #adj = c(0.5, 0),
        # cex = 1, col = "white") #}
    # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
    #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
    # }
    } , bg.border = NA)
title("F", adj = 0, cex = 2, font =1 )
circos.clear()


## Figure 6f
par(mar = c(rep(1,4)))
# circos.par(cell.padding = c(rep(0,4)))
chordDiagram(
    df_hidden, annotationTrack = "grid",
    annotationTrackHeight = c( 0.1),
    #preAllocateTracks = list(track.height = 0.1),
    #directional = 1, direction.type = "arrows",link.arr.type = "big.arrow",
    link.arr.lwd = NA, grid.col = brewer.pal(4, "RdBu")[-4])

## from tutorial
circos.trackPlotRegion(track.index = 1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
    #if(abs(xplot[2] - xplot[1]) < 20) {
        # circos.text(mean(xlim), mean(ylim), sector.name, facing = "bending.outside", niceFacing = TRUE, #adj = c(0.5, 0),
        # cex = 1, col = "white") #}
    # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
    #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
    # }
    } , bg.border = NA)
title("G", adj = 0, cex = 2, font = 1)
circos.clear()
###########################
### time
###########################

## third legend
par(mar = c(rep(1,4)))
plot.new()
legend(
    "center",
    title = "Temoral scales",
    legend = c("Decade-century", "Month-year", "Week-month", "Year-decade"),
    fill = brewer.pal(4, "PuOr"),
    border = brewer.pal(4, "PuOr"), bty = "n"
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

df_hidden <- as_tibble(df_inc3) %>%
    select(Tail, Head, inc) %>%
    left_join(df_scales, by = c("Tail"="rs")) %>%
    rename(link_from = time) %>%
    left_join(df_scales, by = c("Head"="rs")) %>%
    rename(link_to = time) %>%
    arrange(link_from) %>%
    select(link_from, link_to, inc, Tail, Head)
## Figure 6g
par(mar = c(rep(1,4)))
# circos.par(cell.padding = c(rep(0,4)))
chordDiagram(
    df_drivers, annotationTrack = c("grid"),
    annotationTrackHeight = c( 0.1),
    #preAllocateTracks = list(track.height = 0.1),
    #directional = 1, direction.type = "arrows",link.arr.type = "big.arrow",
    link.arr.lwd = NA , grid.col = brewer.pal(4, "PuOr")
)

## from tutorial
circos.trackPlotRegion(track.index = 1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
    #if(abs(xplot[2] - xplot[1]) < 20) {
        # circos.text(mean(xlim), mean(ylim), sector.name, facing = "bending.outside", niceFacing = TRUE, #adj = c(0.5, 0),
        # cex = 1, col = "white") #}
    # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
    #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
    # }
    } , bg.border = NA)
title("H", adj = 0, cex = 2, font = 1)
circos.clear()


## Figure 6h
par(mar = c(rep(1,4)))
# circos.par(cell.padding = c(rep(0,4)))
chordDiagram(
    df_domino, annotationTrack = "grid",
    annotationTrackHeight = c( 0.1),
    #preAllocateTracks = list(track.height = 0.1),
    directional = 1, direction.type = "diffHeight+arrows",link.arr.type = "big.arrow",
    link.arr.lwd = NA, grid.col = brewer.pal(4, "PuOr"))

## from tutorial
circos.trackPlotRegion(track.index = 1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
    #if(abs(xplot[2] - xplot[1]) < 20) {
        # circos.text(mean(xlim), mean(ylim), sector.name, facing = "bending.outside", niceFacing = TRUE, #adj = c(0.5, 0),
        # cex = 1, col = "white") #}
    # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
    #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
    # }
    } , bg.border = NA)
title("I", adj = 0, cex = 2, font = 1)
circos.clear()


## Figure 6i
par(mar = c(rep(1,4)))
# circos.par(cell.padding = c(rep(0,4)))
chordDiagram(
    df_hidden, annotationTrack = "grid",
    annotationTrackHeight = c( 0.1),
    #preAllocateTracks = list(track.height = 0.1),
    #directional = 1, direction.type = "arrows",link.arr.type = "big.arrow",
    link.arr.lwd = NA, grid.col = brewer.pal(4, "PuOr"))

## from tutorial
circos.trackPlotRegion(track.index = 1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
    #if(abs(xplot[2] - xplot[1]) < 20) {
        # circos.text(mean(xlim), mean(ylim), sector.name, facing = "bending.outside", niceFacing = TRUE, #adj = c(0.5, 0),
        # cex = 1, col = "white") #}
    # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
    #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
    # }
    } , bg.border = NA)
title("J", adj = 0, cex = 2, font = 1)
circos.clear()


# ggsave("Cascading_Fig6.png")
quartz.save("Cascading_Fig3_b.png", type = "png", dpi = 600, width = 4.75, height = 3)

## plot results
p <- ggplot(df, aes(estimate, term))+
    geom_vline(xintercept = 0, color = 'grey', show.legend = F, linetype = 2, size = 0.5)  +
    geom_point(aes(shape = factor(P)), size = 1, show.legend = T) +
    scale_shape_manual(name = "p value", values = c(19,1)) +
    geom_errorbarh(aes(xmax = conf.hi, xmin = conf.low, height = 0.1),
                   show.legend = T, size = 0.2) +
    facet_grid(type ~ model, scales = "free", drop = TRUE, switch = "y") + theme_light(base_size = 6) + labs(tag="A") + xlab("") + ylab("") +
    theme(
        legend.position = "top",
        plot.margin = margin(t=2,r=5,b=2,l=5,"pt"),
        title = element_text(face = 'bold')
    )


pp <- ggplot(data = data_frame(x = 0, y= 0), aes(x,y)) + geom_blank() +
    theme_void() +
    annotation_custom(
        grob = grid::rasterGrob( image = readPNG(
            "~/Documents/Projects/Cascading Effects/Figures/Cascading_Fig3_b.png"),
            interpolate = TRUE),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
        )

### Ensamble

quartz(width = 4.75, height = 6, pointsize = 8)
gg <- list(p,pp)
layout <- matrix(c(1,2), ncol = 1, nrow = 2)
multiplot(plotlist = gg, layout = layout)

quartz.save("Cascading_Fig3.pdf", type = "pdf", width = 4.75, height = 6, pointsize = 8)
