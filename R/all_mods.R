######
# reproducing pRDA, pLR, and GLM for potamogeton analysis
# this is not exactly Janne's analysis but similar for my knowledge
# it's generally complete except for tabulation of variance partitioning
# he used an additional step using adjusted R-squared

library(dplyr)
library(vegan)
library(packfor)
library(MASS)

load(file = 'data/all_potam.RData')

######
# community composition model

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
  
######
# richness model

# Janne used sum of all abundances as richness
rich <- select(all_potam, matches('^P', ignore.case = F)) %>% 
  rowSums %>% 
  data.frame(all_potam, S = .)

##
# separate mods  

# local mod
rich_modloc <- glm(S ~ color + depth + tp + alk + secchi, family = poisson(link = 'log'), 
  data = rich, offset = log(tot))

# climate mod
rich_modcli <- glm(S ~ prec + tmax + tmean + alt + tmin, family = poisson(link = 'log'), 
  data = rich, offset = log(tot))

# spatial mod
form <- grep('^V', names(rich), value = T) %>% 
  paste(., collapse = ' + ') %>% 
  paste0('S ~ ', .) %>% 
  as.formula
rich_modspa <- glm(form, data = rich, family = poisson(link = 'log'), offset = log(tot))

##
# partial least squares, using varpart 
rich_mod <- varpart(Y = rich$S, X = loc, cli, spa)

######
# individual species mods, repeated for each species

# local mod
PA_modloc <- glm(PA ~ color + depth + tp + alk + secchi, family = poisson(link = 'log'), 
  data = rich, offset = log(tot)) %>% 
  stepAIC(PA_modloc, direction = 'both', criterion = 'AIC')
PA_modloc <- formula(PA_modloc)[c(1, 3)]

# climate mod
PA_modcli <- glm(PA ~ prec + tmax + tmean + alt + tmin, family = poisson(link = 'log'), 
  data = rich, offset = log(tot)) %>% 
  stepAIC(., direction = 'both', criterion = 'AIC')
PA_modcli <- formula(PA_modcli)[c(1, 3)]

# spatial mod, stepwise, then get formula
form <- grep('^V', names(rich), value = T) %>% 
  paste(., collapse = ' + ') %>% 
  paste0('PA ~ ', .) %>% 
  as.formula
PA_modspa <- glm(form, data = rich, family = poisson(link = 'log'), offset = log(tot)) %>% 
  stepAIC(., direction = 'both', criterion = 'AIC')
PA_modspa <- formula(PA_modspa)[c(1, 3)]

varpart(rich$PA, PA_modloc, PA_modcli, PA_modspa, transfo = 'hel', data = rich)
