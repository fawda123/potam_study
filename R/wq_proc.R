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

# import binary, subset by potam lakes, save as binary
load('M:/docs/veg_indics/wq_data/mn_wqdat.RData')

data(mn_potam)

mn_wqdat$DOWLKNUM <- as.numeric(mn_wqdat$DOWLKNUM)

tmp <- mn_wqdat[mn_wqdat$DOWNLKNUM %in% mn_potam$lake, ]

mn_wqdat_potam <- tmp

save(mn_wqdat_potam, file = 'M:/docs/veg_indics/wq_data/mn_wqdat_potam.RData')

# format data
load('M:/docs/veg_indics/wq_data/mn_wqdat_potam.RData')

# format - keep relevant cols, summarize by lake/parameter, checked units
sel_cols <- c('Depth, Secchi disk depth', 'Apparent color', 'Alkalinity, total', 'Phosphorus')
tmp <- mn_wqdat_potam %>% 
  filter(CHEMICAL_NAME %in% sel_cols & !UNIT %in% 'mg/g') %>% 
  select(DOWLKNUM, CHEMICAL_NAME, SAMPLEDATE, RESULT_NUMERIC, UNIT) %>% 
  group_by(DOWLKNUM, CHEMICAL_NAME) %>% 
  summarize(mean_res = mean(as.numeric(RESULT_NUMERIC), na.rm = T))

# make wide format
library(tidyr)
tmp <- spread(tmp, key = 'CHEMICAL_NAME', value = 'mean_res')

# changes names to match potam data
names(tmp) <- c('lake', 'alk', 'color', 'secchi', 'tp')

dim(na.omit(tmp))

# combine this with other wq data, any missing?

######
# wisconsin