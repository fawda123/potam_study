##
# mn data

# # import raw data, save as binary
# raw_dat <- readLines('M:/docs/veg_indics/wq_data/FISH_LAKES_JO_NF_JP_FINAL.txt')
# tmp <- strsplit(raw_dat, '\t') %>% 
#   do.call('rbind', .)
# heads <- tmp[1, ]
# tmp <- data.frame(tmp[-1, ], stringsAsFactors = F)
# names(tmp) <- heads
# 
# mn_wqdat <- tmp
# save(mn_wqdat, file = 'M:/docs/veg_indics/wq_data/mn_wqdat.RData')
# 
# # import binary, subset by potam lakes, save as binary
# load('M:/docs/veg_indics/wq_data/mn_wqdat.RData')
# 
# data(mn_potam)
# 
# mn_wqdat$DOWLKNUM <- as.numeric(mn_wqdat$DOWLKNUM)
# 
# tmp <- mn_wqdat[mn_wqdat$DOWLKNUM %in% mn_potam$lake, ]
# 
# # format - keep relevant cols, summarize by lake/parameter, checked units
# sel_cols <- c('Depth, Secchi disk depth', 'Apparent color', 'Alkalinity, total', 'Phosphorus')
# tmp <- tmp %>% 
#   filter(CHEMICAL_NAME %in% sel_cols & !UNIT %in% 'mg/g') %>% 
#   select(DOWLKNUM, CHEMICAL_NAME, SAMPLEDATE, RESULT_NUMERIC, UNIT) %>% 
#   group_by(DOWLKNUM, CHEMICAL_NAME) %>% 
#   summarize(mean_res = mean(as.numeric(RESULT_NUMERIC), na.rm = T))
# 
# # make wide format
# library(tidyr)
# tmp <- spread(tmp, key = 'CHEMICAL_NAME', value = 'mean_res')
# 
# # changes names to match potam data, change order
# names(tmp) <- c('lake', 'alk', 'color', 'secchi', 'tp')
# tmp <- tmp[, c('lake', 'secchi', 'color', 'alk', 'tp')]
# 
# # save
# mn_wqdat_potam <- tmp
# save(mn_wqdat_potam, file = 'M:/docs/veg_indics/wq_data/mn_wqdat_potam.RData')

##
# load data for combine
# load data from storet (see 'storet_proc.R')
load('M:/docs/veg_indics/wq_data/mn_wqdat_potam.RData')
data(allmn_wq)

# combine 
tmp <- full_join(allmn_wq, mn_wqdat_potam, by = 'lake')

# make vector of values
tmp <- melt(tmp, id.var = 'lake')

storet <- tmp[grepl('\\.x$', tmp$variable), 'value']
fishwq <- tmp[grepl('\\.y$', tmp$variable), 'value']

# get missing values in storet
sel <- is.na(storet)
storet[sel] <- fishwq[sel]

# replace old storet with filled storet, format
tmp <- tmp[grepl('\\.x$', tmp$variable), ]
tmp$newq <- storet  

tmp <- select(tmp, lake, variable, newq)
tmp$variable <- gsub('\\.x$', '', tmp$variable)

tmp <- spread(tmp, key = 'variable', value = 'newq')
tmp <- tmp[, c('lake', 'secchi', 'color', 'alk', 'tp')]

# save, replace old allmn_wq
allmn_wq <- tmp
save(allmn_wq, file = 'data/allmn_wq.RData')

######
# wisconsin