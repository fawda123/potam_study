library(dplyr)
library(vegan)
library(packfor)


load(file = 'data/all_potam.RData')

# get species distribution/abund matrix
# use hellinger transformation (divide by margin totals then sqrt)
pot <- select(all_potam, matches('^P', ignore.case = F)) %>% 
  decostand(method = 'hellinger')

# select groups of vars
loc <- select(all_potam, alk, color, tp, secchi, area, depth, perim)
cli <- select(all_potam, tmean, tmax, tmin, prec, alt)
spa <- select(all_potam, matches('^V', ignore.case = F))

# use forward selection from packfor to id important vars
loc_sel <- forward.sel(pot, loc)
loc <- loc[, names(loc) %in% loc_sel$variables]
cli_sel <- forward.sel(pot, cli)
cli <- cli[, names(cli) %in% cli_sel$variables]
spa_sel <- forward.sel(pot, spa)
spa <- spa[, names(spa) %in% spa_sel$variables]

cc_mod <- varpart(Y = pot, X = loc, cli, spa)
  
