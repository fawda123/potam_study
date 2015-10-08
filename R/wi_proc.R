#####
# process WI lakes for potamogeton spp freq

library(dplyr)
library(plyr)
library(reshape2)
library(data.table)

source('R/funcs.r')

load(file = 'M:/docs/veg_indics/veg_analyses/data/widat_ls.RData')

res <- llply(widat_ls, 
  .fun = function(x){
    out <- try(pot_freq(x, counts = T))
    if('try-error' %in% class(out)) out <- NA
    out
  })

res_melt <- melt(res)

# make wide format
tmp <- tidyr::spread(res_melt, pot_sp, value)
tmp$variable <- NULL

# remove species not found
no_miss <- colSums(tmp[, -1]) > 0
tmp<- data.frame(lake = tmp[, 1], tmp[, -1][, no_miss])

# get lake dates
wi_dates <- read.table('M:/docs/veg_indics/macrophyte_data/WI lakes/WI_lake_dates.txt', sep = ',', header = F)
wi_dates[, 2] <- gsub('-', '', wi_dates[, 2])
names(wi_dates) <- c('lake', 'YYYYMMDD')
tmp <- merge(wi_dates, tmp, by = 'lake')

# output
tmp$lake <- as.numeric(as.character(tmp$lake))
wi_potam <- tmp
save(wi_potam, file = 'data/wi_potam.RData')

# write.csv(wi_potam, 'wi_potam.csv', quote = F, row.names = F)

######
# get morpho variables
# lake area (km2), perimeter (km), and max depth (m)

rm(list = ls())

# master WI file
data(wi_potam)

# WI metadata w/ morpho info
data(wimet_dat)
wi_morpho <- select(wimet_dat, WBIC, area_m2, SDI, Maxdepthm)

# area, depth
wi_morpho <- mutate(wi_morpho, 
  area = area_m2 / 1e6, 
  depth = Maxdepthm,
  perim = SDI * 2 * sqrt(pi * area_m2) / 1e6
)
wi_morpho <- select(wi_morpho, WBIC, area, depth, perim)
names(wi_morpho)[names(wi_morpho) %in% 'WBIC'] <- 'lake'

# combine and save
wi_potam <- left_join(wi_potam, wi_morpho, by = 'lake')

save(wi_potam, file = 'data/wi_potam.RData')

######
# get wq variables

rm(list = ls())

# load data
data(wi_potam)
data(allwi_wq)

# merge the data
wi_potam <- dplyr::left_join(wi_potam, allwi_wq, by = 'lake')

save(wi_potam, file = 'data/wi_potam.RData')

######
# spatial lat/long

rm(list = ls())

# load data
data(wi_potam)
wi_poly <- foreign::read.dbf('M:/GIS/WI_WBIC_geocoord.dbf')
names(wi_poly)[names(wi_poly) %in% 'WATERBODY_'] <- 'lake'
wi_poly <- dplyr::select(wi_poly, lake, Latitude, Longitude)

# merge and save
wi_potam <- dplyr::left_join(wi_potam, wi_poly, by = 'lake')

save(wi_potam, file = 'data/wi_potam.RData')

######
# get climate variables

rm(list = ls())

source('R/funcs.r')

# data and separate object for locations
data(wi_potam)
dat <- select(wi_potam, lake, Longitude, Latitude)
dat <- na.omit(dat)
coords <- select(dat, Longitude, Latitude)
dat <- select(dat, lake)
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
# merge with wi_potam, save
dat <- select(data.frame(dat), -c(Longitude, Latitude))
wi_potam <- left_join(wi_potam, data.frame(dat), by = 'lake')

save(wi_potam, file = 'data/wi_potam.RData')

