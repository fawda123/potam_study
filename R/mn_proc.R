######
# process MN lakes for potamogeton spp freq

# path <- 'M:/docs/veg_indics/macrophyte_data/all_data/'
# files <- dir(path)
# 
# mndat_ls <- vector('list', length = length(files))
# names(mndat_ls) <- gsub('\\.txt$', '', files)
# for(fl in files){
#   nm <- gsub('\\.txt', '', fl)
#   dat <- read.table(paste0(path, fl), header = T, sep = ',')
#   mndat_ls[[nm]] <- dat
# }
# save(mndat_ls, file = 'data/mndat_ls.RData')

load(file = 'data/mndat_ls.RData')

res <- llply(mndat_ls, 
  .fun = function(x){
    out <- try(pot_freq(x))
    if('try-error' %in% class(out)) out <- NA
    out
  })

res_melt <- melt(res)

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

# metadata
load(file = 'data/mnmet_dat.RData')

keep_dat <- c('DOW', 'hectares', 'prop_litt', 'SDI', 'points', 'tsi', 'alk', 'lat', 'lon', 'lake_elev', 'GDD', 'july_temp', 'urban', 'ag', 'forest', 'wshed_imperv', 'wshed_area')
mnmet_dat <- mnmet_dat[, names(mnmet_dat) %in% keep_dat]

tmp$lake <- as.numeric(as.character(tmp$lake))
mn_potam <- merge(tmp, mnmet_dat, by.x = 'lake', by.y = 'DOW')

save(mn_potam, file = 'data/mn_potam.RData')

write.csv(mn_potam, 'mn_potam.csv', quote = F, row.names = F)
