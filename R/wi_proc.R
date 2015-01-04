#####
# process WI lakes for potamogeton spp freq

# path <- 'M:/docs/veg_indics/macrophyte_data/WI lakes/converted/'
# files <- dir(path)
# 
# widat_ls <- vector('list', length = length(files))
# names(widat_ls) <- gsub('\\.txt$', '', files)
# for(fl in files){
#   nm <- gsub('\\.txt', '', fl)
#   dat <- read.table(paste0(path, fl), header = T, sep = ',')
#   dat[is.na(dat)] <- 0
#   dat <- dat[, !names(dat) %in% c('Point', 'Latitude', 'Longitude')]
#   names(dat)[names(dat) %in% 'Depth_ft'] <- 'AQPNT_Depth'
#   widat_ls[[nm]] <- dat
# }
# save(widat_ls, file = 'data/widat_ls.RData')

load(file = 'data/widat_ls.RData')

res <- llply(widat_ls, 
  .fun = function(x){
    out <- try(pot_freq(x))
    if('try-error' %in% class(out)) out <- NA
    out
  })

res_melt <- melt(res)

# make wide format
tmp <- reshape2::dcast(L1 + variable ~ pot_sp, data = res_melt, value = 'value')
tmp$variable <- NULL

# remove species not found
no_miss <- colSums(tmp[, -1]) > 0
tmp<- data.frame(lake = tmp[, 1], tmp[, -1][, no_miss])

# get lake dates
wi_dates <- read.table('M:/docs/veg_indics/macrophyte_data/WI lakes/WI_lake_dates.txt', sep = ',', header = F)
wi_dates[, 2] <- gsub('-', '', wi_dates[, 2])
names(wi_dates) <- c('lake', 'YYYYMMDD')
tmp <- merge(wi_dates, tmp, by = 'lake')

# metadata
load(file = 'data/wimet_dat.RData')

keep_dat <- c('WBIC', 'area_m2', 'SDI', 'Maxdepthm', 'type', 'TSI', 'Ag01', 'Urban01', 'Forest01', 'Ag06', 'Urban06', 'Forest06', 'road_dens', 'type')
wimet_dat <- wimet_dat[, names(wimet_dat) %in% keep_dat]

tmp$lake <- as.numeric(as.character(tmp$lake))
wi_potam <- merge(tmp, wimet_dat, by.x = 'lake', by.y = 'WBIC')

save(wi_potam, file = 'data/wi_potam.RData')

write.csv(wi_potam, 'wi_potam.csv', quote = F, row.names = F)
