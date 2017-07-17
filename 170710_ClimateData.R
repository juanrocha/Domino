rm(list = ls())
library(tidyverse)
library(lubridate)


#######
# Import climate data to control for climate oscillations. For a short description of all of them: http://www.cpc.ncep.noaa.gov/data/teledoc/telecontents.shtml
# Note they have a complete list of all of them plus a column "Explained variance" but I don't use it in my analysis. Also they note: Values are set to -99.90 for calendar months when the pattern is not normally a leading mode of variability.

#North Atlantic Oscillation
nao <- as_tibble(read.table(
    "ftp://ftp.cpc.ncep.noaa.gov/wd52dg/data/indices/nao_index.tim",
    skip = 8, header = TRUE)) %>%
    rename(nao = INDEX)
# East Atlantic
ea <- as_tibble(read.table(
    "ftp://ftp.cpc.ncep.noaa.gov/wd52dg/data/indices/ea_index.tim",
    skip = 8, header = TRUE)) %>%
    rename(ea = INDEX)
# East Atlantic and Western Russia
eawr <- as_tibble(read.table(
    "ftp://ftp.cpc.ncep.noaa.gov/wd52dg/data/indices/eawr_index.tim",
    skip = 8, header = TRUE)) %>%
    rename(eawr = INDEX)
# Scandinavia
scand <- as_tibble(read.table(
    "ftp://ftp.cpc.ncep.noaa.gov/wd52dg/data/indices/scand_index.tim",
    skip = 8, header = TRUE)) %>%
    rename(scand = INDEX)
# Polar Eurasia
poleur <- as_tibble(read.table(
    "ftp://ftp.cpc.ncep.noaa.gov/wd52dg/data/indices/poleur_index.tim",
    skip = 8, header = TRUE)) %>%
    rename(poleur = INDEX)
# West Pacific
wp <- as_tibble(read.table(
    "ftp://ftp.cpc.ncep.noaa.gov/wd52dg/data/indices/wp_index.tim",
    skip = 8, header = TRUE)) %>%
    rename(wp = INDEX)
# East Pacific - North Pacific
epnp <- as_tibble(read.table(
    "ftp://ftp.cpc.ncep.noaa.gov/wd52dg/data/indices/epnp_index.tim",
    skip = 8, header = TRUE)) %>%
    rename(epnp = INDEX)
#Pacific north american
pna <- as_tibble(read.table(
    "ftp://ftp.cpc.ncep.noaa.gov/wd52dg/data/indices/pna_index.tim",
    skip = 8, header = TRUE)) %>%
    rename(pna = INDEX)
# Tropical northern hemisphere
tnh <- as_tibble(read.table(
    "ftp://ftp.cpc.ncep.noaa.gov/wd52dg/data/indices/tnh_index.tim",
    skip = 8, header = TRUE)) %>%
    rename(tnh = INDEX)
# Pacific transition
pt <- as_tibble(read.table(
    "ftp://ftp.cpc.ncep.noaa.gov/wd52dg/data/indices/pt_index.tim",
    skip = 8, header = TRUE)) %>%
    rename(pt = INDEX)

clim <- left_join(nao, ea) %>% left_join(eawr) %>%
    left_join(scand) %>% left_join(poleur)%>% left_join(wp)%>%
    left_join(epnp)%>% left_join(pna)%>% left_join(tnh)%>%
    left_join(pt)

rm(nao,ea,eawr,scand,poleur,wp,epnp,pna,tnh,pt)

setwd("~/Documents/Projects/Cascading Effects")
save(clim, file = "NOAA_ClimateData.rda", compress = FALSE)
