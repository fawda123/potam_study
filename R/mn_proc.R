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
    out <- try(pot_freq(x, counts = F))
    if('try-error' %in% class(out)) out <- NA
    out
  })

res_melt <- reshape2::melt(res)

tmp <- tidyr::spread(res_melt, pot_sp, value)
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

######
# get wq variables

rm(list = ls())

load(file = 'data/mn_potam.RData')

# wq data from STORET, see 'storet_proc.r'
load(file = 'data/allmn_wq.RData')
  
# merge the data
mn_potam <- dplyr::left_join(data.frame(mn_potam), allmn_wq, by = 'lake')

save(mn_potam, file = 'data/mn_potam.RData')

######
# spatial lat/long

rm(list = ls())

# load data
data(mn_potam)
mn_poly <- foreign::read.dbf('M:/GIS/MN/lake_dnrpy2_geocoord.dbf')
names(mn_poly)[names(mn_poly) %in% 'DOWLKNUM'] <- 'lake'
mn_poly <- dplyr::select(mn_poly, lake, Latitude, Longitude)
mn_poly$lake <- as.numeric(as.character(mn_poly$lake))

# merge and save
mn_potam <- dplyr::left_join(mn_potam, mn_poly, by = 'lake')

save(mn_potam, file = 'data/mn_potam.RData')

######
# get climate variables

rm(list = ls())

source('R/funcs.r')

# data and separate object for locations
data(mn_potam)
dat <- dplyr::select(mn_potam, lake, Longitude, Latitude)
dat <- na.omit(dat)
coords <- dplyr::select(dat, Longitude, Latitude)
dat <- dplyr::select(dat, lake)
dat <- SpatialPointsDataFrame(coords, dat, proj4string = CRS("+proj=longlat +datum=WGS84"))

# location of met data
rast_path <- 'M:/GIS/climate'

## 
# mean annual temp
# get by month, then average
tmean<- clim_fun(paste0(rast_path, '/tmean/tmean_'), dat)

# get annual means
tmean <- rowMeans(tmean)

# add to dat
dat$tmean <- tmean / 10

##
# max temp of warmest month
tmax <- clim_fun(paste0(rast_path, '/tmax/tmax_'), dat)

# index of warmest month
tmax<- apply(tmax, 1, function(x) x[which.max(x)])

# add to dat
dat$tmax <- tmax / 10

##
# minimum temp of coolest month
tmin <- clim_fun(paste0(rast_path, '/tmin/tmin_'), dat)

# index of coolest month
tmin <- apply(tmin, 1, function(x) x[which.min(x)])

# add to dat
dat$tmin <- tmin /10

##
# precipitation of driest month mm
prec <- clim_fun(paste0(rast_path, '/prec/prec_'), dat)

# index of warmest month
prec<- apply(prec, 1, function(x) x[which.min(x)])

# add to dat
dat$prec <- prec

##
# lake altitude
alt <- raster::raster(paste0(rast_path, '/alt/alt'))
alt <- raster::extract(alt, dat)

# add to dat
dat$alt <- alt

##
# merge with mn_potam, save
dat <- dplyr::select(data.frame(dat), -c(Longitude, Latitude))
mn_potam <- left_join(mn_potam, data.frame(dat), by = 'lake')

save(mn_potam, file = 'data/mn_potam.RData')
