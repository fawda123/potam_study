######
# process MN lakes for potamogeton study
# other processing in M:/docs/veg_indics/veg_analyses/R

######
# get potamogeton frequency occurence by lake

rm(list = ls())

library(data.table)

source('R/funcs.r')

load(file = 'M:/docs/veg_indics/veg_analyses/data/mndat_ls.RData')

res <- plyr::llply(mndat_ls, 
  .fun = function(x){
    out <- try(pot_freq(x))
    if('try-error' %in% class(out)) out <- NA
    out
  })

res_melt <- reshape2::melt(res)

tmp <- reshape2::dcast(L1 + variable ~ pot_sp, data = res_melt, value = 'value')
tmp$variable <- NULL
tmp <- data.frame(
  do.call('rbind', strsplit(tmp$L1, '_')), 
  tmp[, !names(tmp) %in% 'L1']
)
names(tmp)[names(tmp) %in% c('X1', 'X2')] <- c('lake', 'YYYYMMDD')

# remove species not found
no_miss <- colSums(tmp[, -c(1, 2)]) > 0
tmp<- data.frame(tmp[, c(1, 2)], tmp[, -c(1, 2)][, no_miss])

mn_potam <- tmp
mn_potam$lake <- as.numeric(as.character(mn_potam$lake))
mn_potam <- data.table(mn_potam, key = 'lake')

save(mn_potam, file = 'data/mn_potam.Rdata')

######
# get morpho variables

rm(list = ls())

# master MN file
load(file = 'data/mn_potam.RData')

# MN metadata
load(file = 'data/mnmet_dat.RData')

names(mnmet_dat)[names(mnmet_dat) %in% c('DOW', 'depth_m')] <- c('lake', 'depth')
keep_dat <- c('lake', 'hectares', 'depth')
mnmet_dat <- mnmet_dat[, names(mnmet_dat) %in% keep_dat]
mnmet_dat <- data.table(mnmet_dat, key = 'lake')

mn_potam <- merge(mn_potam, mnmet_dat, by = 'lake')

# lake area as km2
mn_potam$area <- mn_potam$hectares * 0.01
mn_potam$hectares <- NULL

# get perim
library(maptools)

lake_poly <- readShapeSpatial('M:/GIS/DNR_veg/lake_dnrpy2.shp')

# get veg lakes
names(data.frame(lake_poly))
get_veg <- as.numeric(as.character(data.frame(lake_poly)$DOWLKNUM)) %in% mn_potam$lake
lake_poly <- lake_poly[get_veg, ]

perim <- data.frame(lake_poly)[, c('DOWLKNUM', 'SHORE_MI')]
perim$perim <- perim$SHORE_MI * 1.60934
perim$lake <- as.numeric(as.character(perim$DOWLKNUM))
perim <- data.table(perim[, c('lake', 'perim')], key = 'lake')

mn_potam <- merge(mn_potam, perim, all.x = T)

# 56076002 perim measured manually in arcgis
mn_potam[mn_potam$lake %in% '56076002', 'perim'] <- 15.058

save(mn_potam, file = 'data/mn_potam.RData')

##
# get wq variables

rm(list = ls())

load(file = 'data/mn_potam.RData')


# get climate variables
