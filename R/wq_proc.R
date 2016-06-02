######
# this file was used to supplement storet wq data with other wq files for MN and WI

library(tidyr)
library(dplyr)
library(reshape2)

rm(list = ls())

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
#   dplyr::select(DOWLKNUM, CHEMICAL_NAME, SAMPLEDATE, RESULT_NUMERIC, UNIT) %>% 
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

tmp <- dplyr::select(tmp, lake, variable, newq)
tmp$variable <- gsub('\\.x$', '', tmp$variable)

tmp <- spread(tmp, key = 'variable', value = 'newq')
tmp <- tmp[, c('lake', 'secchi', 'color', 'alk', 'tp')]

# save, replace old allmn_wq
allmn_wq <- tmp
save(allmn_wq, file = 'data/allmn_wq.RData')

######
# wisconsin

rm(list = ls())

# # # load raw LTER data
# # raw_dat <- read.csv('M:/docs/veg_indics/wq_data/wilakeslimnoparams.csv', 
# #   stringsAsFactors = F)
# # 
# # wi_wqdat <- raw_dat
# # save(wi_wqdat, file = 'M:/docs/veg_indics/wq_data/wi_wqdat.RData')
# 
# ##
# # load binary data
# load('M:/docs/veg_indics/wq_data/wi_wqdat.RData')
# 
# # get only the lakes I have
# data(wi_potam)
# lks <- wi_potam$lake
# 
# tmp <- wi_wqdat[wi_wqdat$wbic %in% lks, ]
# 
# # get relevant parameters
# # checked metadata to ensure all units were the same for each
# # http://tropical.lternet.edu/knb/metacat?action=read&qformat=lter&sessionid=&docid=knb-lter-ntl.263.4&displaymodule=entity&entitytype=dataTable&entityindex=1
# nms <- c('wbic', '_p_total$', 'sr6_color$', 'sr7_color$', 'alkalinity', 'secchi')
# nms <- paste(nms, collapse = '|')
# tmp <- tmp[, grepl(nms, names(tmp))]
# 
# # remove empty columns
# empts <- apply(tmp, 2, function(x) sum(is.na(x))) == nrow(tmp)
# tmp <- tmp[, !empts]
# 
# # average by parameter across sources
# nms <- c('secchi', 'alkalinity', 'p_total', 'color')
# res <- vector('list', length(nms))
# names(res) <- nms
# for(nm in nms){
#   to_ave <- tmp[, grepl(nm, names(tmp))]
#   res[[nm]] <- rowMeans(to_ave, na.rm = T)
# }
# 
# # format
# res <- do.call('cbind', res)
# tmp <- data.frame(lake = tmp$wbic, res)
# names(tmp) <- c('lake', 'secchi', 'alk', 'tp', 'color')
# tmp <- tmp[, c('lake', 'alk', 'color', 'secchi', 'tp')]
# 
# # save
# wi_wqdat_potam <- tmp
# save(wi_wqdat_potam, file = 'M:/docs/veg_indics/wq_data/wi_wqdat_potam.RData')

##
# load data from storet (see 'storet_proc.R')
load('M:/docs/veg_indics/wq_data/wi_wqdat_potam.RData')
data(allwi_wq)

# combine 
tmp <- full_join(allwi_wq, wi_wqdat_potam, by = 'lake')

# make vector of values
tmp <- melt(tmp, id.var = 'lake')

storet <- tmp[grepl('\\.x$', tmp$variable), 'value']
lterwq <- tmp[grepl('\\.y$', tmp$variable), 'value']

# get missing values in storet
sel <- is.na(storet)
storet[sel] <- lterwq[sel]

# replace old storet with filled storet, format
tmp <- tmp[grepl('\\.x$', tmp$variable), ]
tmp$newq <- storet  

tmp <- dplyr::select(tmp, lake, variable, newq)
tmp$variable <- gsub('\\.x$', '', tmp$variable)

tmp <- tidyr::spread(tmp, key = 'variable', value = 'newq')
tmp <- tmp[, c('lake', 'secchi', 'color', 'alk', 'tp')]

# save, replace old allmn_wq
allwi_wq <- tmp

save(allwi_wq, file = 'data/allwi_wq.RData')
