######
# processing of STORET wq data for MN and WI
# January 2015

######
# get legacy storet data (up to 1998) from downloaded files for MN
# downloaded from here: http://www.epa.gov/storpubl/legacy/gateway.htm

rm(list = ls())

library(foreach)
library(doParallel)
library(dplyr)

load('data/mn_potam.RData')

get_files <- list.files(path = 'M:/docs/veg_indics/STORET_data/MN_legacy_storet/Minnesota/', 
  pattern = '^.*_res_.*\\.txt', recursive = T, full.names = T)

strt <- Sys.time()
mn_dows <- mn_potam$lake

cl <- makeCluster(8)
registerDoParallel(cl)

out_dat <- foreach(fl = get_files) %dopar% {
  
  # counter
  sink('C:/Users/mbeck/Desktop/log.txt')
  cat(basename(fl), '\n')
  print(Sys.time() - strt)
  sink()
  
  # get file
  tmp <- readLines(fl)
  heads <- strsplit(tmp[1], '\t')[[1]]
  heads <- gsub('^[ ]*|[ ]*$', '', heads)
  
  # filter by lakes
  tmp <- tmp[grepl('LAKE:', tmp)]
  
  # if no lakes, go to next file
  if(length(tmp) == 0){ tmp <- NULL
  
  } else {
    # parse, clear trailing/leading spaces
    tmp <- strsplit(tmp, '\\t')
    tmp <- suppressWarnings(do.call('rbind', tmp))
    tmp <- gsub('^[ ]*|[ ]*$', '', tmp)
    tmp <- data.frame(tmp, stringsAsFactors = F)[, 1:19]
    names(tmp) <- heads[1:19]
    
    # filter by phosphorus, color, secchi, and alkalinity
    # secchi: 77-78
    # color: 79-84
    # alkalinity: 409-431
    # phosphorus: 650-678
    get_parms <- as.numeric(as.character(tmp$Param)) %in% c(77:84, 409:431, 650:678)
    tmp <- tmp[get_parms, ]
    
    # convert station to numeric, 7 or 8 digit
    tmp$Station <- gsub('-', '', as.character(tmp$Station))
    tmp$Station <- as.numeric(as.character(tmp$Station))
    tmp$Station[nchar(tmp$Station) %in% c(5, 6)] <- 100 * tmp$Station[nchar(tmp$Station) %in%  c(5, 6)]
    
    tmp$result <- as.numeric(as.character(tmp[, 'Result Value']))
    
    # retain columns of interest
    keep_cols <- c('Station', 'Param', 'Start Date', 'result')
    tmp <- tmp[, keep_cols]
    names(tmp) <- c('lake', 'param', 'date', 'value')
    
    # subset by veg dows
    tmp <- tmp[tmp$lake %in% mn_dows, ]
  
  }
  
  tmp
    
}


# combine, average by day for each lake, make wide format
tmp <- do.call('rbind', out_dat)
tmp <- tmp[as.numeric(tmp$param) %in% c(78, 80, 410, 665), ]
tmp <- dcast(tmp, lake + date ~ param, value.var = 'value', 
  fun.aggregate = function(x) mean(x, na.rm = T))
names(tmp) <- c('lake', 'date', 'secchi', 'color', 'alk', 'tp')

######
# get recent storet data for MN
# raw data from custom query of tp, transparency, alkalinity, color for all MN lakes in database
# download form here: http://ofmpub.epa.gov/storpubl/dw_pages.querycriteria

# import raw data
raw_dat <- 'M:/docs/veg_indics/STORET_data/MN_current_storet.txt'
raw_dat <- read.table(raw_dat, sep = '\t', header = T)

# select columns of interest
keep_cols <- c('Station.ID', 'Activity.Start', 'Characteristic.Name', 'Result.Value.as.Text', 'Units')
dat <- raw_dat[, keep_cols]
names(dat) <- c('lake', 'date', 'param', 'value', 'units')
    
# selects lakes that have DOW
dat$lake <- gsub('-', '', as.character(dat$lake))
dat <- dat[!grepl('[a-z,A-Z]', dat$lake) & nchar(dat$lake) >= 7, ]
dat$lake <- as.numeric(substr(dat$lake, 1, 8))

# no need to convert units for this example, they are okay
# table(dat$units, dat$param)

# aggregate by day
dat$date <- as.POSIXct(as.character(dat$date), format = '%Y-%m-%d %H:%M:%S')
dat$date <- as.Date(dat$date)
dat <- dcast(dat, lake + date ~ param, value.var = 'value', 
  fun.aggregate = function(x) mean(as.numeric(as.character(x)), na.rm = T))

# get only the lakes that I have
load('data/mn_potam.RData')
dat <- dat[dat$lake %in% mn_potam$lake, ]

# retain relevant results, rearrange for combine with legacy data
# apply(dat, 2, function(x) sum(is.na(x))/length(x))
keep_cols <- c(1, 2, 4, 5, 6, 7)
dat <- dat[, keep_cols]
names(dat) <- c('lake', 'date', 'alk', 'color', 'tp', 'secchi')
dat <- dat[, c('lake', 'date', 'secchi', 'color', 'alk', 'tp')]

## 
# combine legacy with updated
# average across dates
# save

# all dates.... need to describe this distribution in the manuscript
allmn_wq <- rbind(dat, tmp)

# aggregated 
allmn_wq <- aggregate(. ~ lake, allmn_wq[, -2], FUN = function(x) mean(x, na.rm = T), na.action = na.pass)

save(allmn_wq, file = 'data/allmn_wq.RData')

######
# WI storet data
# get legacy storet data (up to 1998) from downloaded files for WI
# downloaded from here: http://www.epa.gov/storpubl/legacy/gateway.htm

rm(list = ls())

library(foreach)
library(doParallel)
library(dplyr)

load('data/wi_potam.RData')

get_files <- list.files(path = 'M:/docs/veg_indics/STORET_data/WI_legacy_storet/Wisconsin', 
  pattern = '^.*_res_.*\\.txt', recursive = T, full.names = T)

strt <- Sys.time()
wi_lakes <- wi_potam$lake

cl <- makeCluster(8)
registerDoParallel(cl)

# get all data for each of four wq measurements
out_dat <- foreach(fl = get_files) %dopar% {
  
  # counter
  sink('C:/Users/mbeck/Desktop/log.txt')
  cat(basename(fl), '\n')
  print(Sys.time() - strt)
  sink()
  
  # get file
  tmp <- readLines(fl)
  heads <- strsplit(tmp[1], '\t')[[1]]
  heads <- gsub('^[ ]*|[ ]*$', '', heads)

  # parse, clear trailing/leading spaces
  tmp <- strsplit(tmp, '\\t')
  tmp <- suppressWarnings(do.call('rbind', tmp))
  tmp <- gsub('^[ ]*|[ ]*$', '', tmp)
  tmp <- data.frame(tmp[-c(1:2), ], stringsAsFactors = F)[, 1:19]
  names(tmp) <- heads[1:19]
  
  # filter by phosphorus, color, secchi, and alkalinity
  # secchi: 77-78
  # color: 79-84
  # alkalinity: 409-431
  # phosphorus: 650-678
  get_parms <- as.numeric(as.character(tmp$Param)) %in% c(77:84, 409:431, 650:678)
  tmp <- tmp[get_parms, ]
  
  # format stations
  tmp$Station <- gsub('^0*|-', '', as.character(tmp$Station))
  tmp <- tmp[!grepl('[A-Z,a-z]', tmp$Station), ]
  tmp$Station <- as.numeric(tmp$Station)
  
  tmp$result <- as.numeric(as.character(tmp[, 'Result Value']))
  
  # retain columns of interest
  keep_cols <- c('Station', 'Param', 'Start Date', 'result', 'Latitude', 'Longitude')
  tmp <- tmp[, keep_cols]
  names(tmp) <- c('lake', 'param', 'date', 'value', 'Latitude', 'Longitude')
  
  tmp
    
}

# format output list
tmp <- do.call('rbind', out_dat)
tmp <- tmp[as.numeric(tmp$param) %in% c(78, 80, 410, 665), ]

# # get lat lon for georeferencing to wbic
# library(dplyr)
# stat_pts <- select(tmp, lake, Latitude, Longitude) %>% mutate(stat = lake)
# stat_pts$lake <- NULL
# stat_pts <- unique(stat_pts)
# stat_pts$Latitude <- as.numeric(stat_pts$Latitude)
# stat_pts$Longitude <- as.numeric(stat_pts$Longitude)
# stat_pts <- na.omit(stat_pts)
# write.table(stat_pts, 'C:/Users/mbeck/Desktop/stat_pts.txt', quote = F, row.names = F, sep = ',')
# 
# # then some crap in arcmap
# 
# keys <- foreign::read.dbf('M:/GIS/WI_legacy_storet_int.dbf')
# keys <- select(keys, stat, WATERBODY_, Latitude, Longitude)
# keys <- unique(keys)
# names(keys) <- c('stat', 'WBIC', 'Latitude', 'Longitude')
# legacy_wi_storet_keys <- keys
# save(legacy_wi_storet_keys, file = 'data/legacy_wi_storet_keys.RData')

# station and wbic info
data('legacy_wi_storet_keys')

# average by day for each lake, make wide format
tmp <- dcast(tmp, lake + date ~ param, value.var = 'value', 
  fun.aggregate = function(x) mean(x, na.rm = T))
names(tmp) <- c('stat', 'date', 'secchi', 'color', 'alk', 'tp')

# merge with storet keys
tmp <- left_join(tmp, legacy_wi_storet_keys, by = 'stat')

# now get the wbics I have
data('wi_potam')
wi_wbic <- wi_potam$lake

# done
wi_legacy <- tmp %>% filter(WBIC %in% wi_wbic)
wi_legacy <- select(wi_legacy, WBIC, date, alk, color, secchi, tp)
names(wi_legacy) <- c('wbic', 'date', 'alk', 'color', 'secchi', 'TP')

######
# current WI storet
# import raw data
raw_dat <- 'M:/docs/veg_indics/STORET_data/WI_current_storet.txt'
raw_dat <- read.table(raw_dat, sep = '\t', header = T)

# station and wbic matches from GIS intersect, only for recent STORET data
# named 'keys'
load(file = 'M:/docs/veg_indics/STORET_data/wi_recentstoret_keys.RData')

# select columns of interest from STORET
keep_cols <- c('Station.ID', 'Activity.Start', 'Characteristic.Name', 'Result.Value.as.Text', 'Units')
dat <- raw_dat[, keep_cols]
names(dat) <- c('station', 'date', 'param', 'value', 'units')
    
# merge station data with keys
dat <- dplyr::inner_join(dat, keys, by = 'station')

# convert param column
dat$param <- as.character(dat$param)
dat$param[grepl('Alkalinity', dat$param)] <- 'alk'
dat$param[grepl('Phosphorus', dat$param)] <- 'TP'
dat$param[grepl('Color|color', dat$param)] <- 'color'
dat$param[grepl('Transparency|Secchi', dat$param)] <- 'secchi'
dat <- dat[dat$param %in% c('alk', 'TP', 'color', 'secchi'), ]

##
# aggregate data by day but need to sort out unit conversions

# remove leading/trailing white space for labels
dat$units <- as.character(dat$units)
dat$units <- gsub('^[[:space:]]*|[[:space:]]*$', '', dat$units)

# values to numeric, remove NA
dat$value <- as.numeric(as.character(dat$value))
dat <- dat[!is.na(dat$value), ]

# check distribution
table(dat[, c('param', 'units')])

# values to remove because there are only a few - 'ueq/L', 'mm', 'm', 'mg/kg'
dat <- dat[!dat$units %in% c('ueq/L', 'mm', 'm', 'mg/kg'), ]

##
# conversions
# secchi as cm
# color as PCU
# alk as mg/l
# TP as mg/L

##
# TP.....

# 1 ppb = 0.001 mg/L 
sel_dat <- dat$param %in% 'TP' & dat$units %in% 'ppb'
dat[sel_dat, 'value'] <- dat[sel_dat, 'value'] * 0.001

# 1 ug/L = 0.001 mg/L 
sel_dat <- dat$param %in% 'TP' & dat$units %in% 'ug/l'
dat[sel_dat, 'value'] <- dat[sel_dat, 'value'] * 0.001

##
# aggregate by day
dat$date <- as.POSIXct(as.character(dat$date), format = '%Y-%m-%d %H:%M:%S')
dat$date <- as.Date(dat$date)
dat <- dcast(dat, wbic + date ~ param, value.var = 'value', 
  fun.aggregate = function(x) mean(as.numeric(as.character(x)), na.rm = T))

# get only the lakes that I have
load('data/wi_potam.RData')
wi_recent <- dat[dat$wbic %in% wi_potam$lake, ]

## 
# combine legacy with updated
# average across dates
# save

# all dates.... need to describe this distribution in the manuscript
allwi_wq <- rbind(wi_legacy, wi_recent)
names(allwi_wq)[1] <- 'lake'

# aggregated 
allwi_wq<- select(allwi_wq, -date) %>% 
  group_by(lake) %>% 
  summarise_each(funs(mean(., na.rm = T)))

names(allwi_wq) <- c('lake', 'alk', 'color', 'secchi', 'tp')

save(allwi_wq, file = 'data/allwi_wq.RData')


