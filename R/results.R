######
# random stuff to help with results section

library(dplyr)
library(tidyr)

data(all_potam)

##
# mean richness by ecoregion, state
rich <- dplyr::select(all_potam, S, Ecoregion, state) %>% 
  mutate(rich = 10^S) %>% 
  select(-S)

ecorich <- group_by(rich, Ecoregion) %>% 
  summarize(rich = mean(rich))

staterich <- group_by(rich, state, Ecoregion) %>% 
  summarize(rich = mean(rich)) 

##
# most abundant species

abundeco <- dplyr::select(all_potam, matches('^P|Ecoregion', ignore.case = F)) %>% 
  gather('spp', 'abund', -Ecoregion) %>% 
  group_by(Ecoregion, spp) %>% 
  summarize(abund = sum(abund)) %>% 
  filter(abund > 0) %>% 
  arrange(Ecoregion, abund) %>% 
  filter(abund == max(abund))

abundecost <- dplyr::select(all_potam, matches('^P|Ecoregion|state', ignore.case = F)) %>% 
  gather('spp', 'abund', -Ecoregion, -state) %>% 
  group_by(Ecoregion, state, spp) %>% 
  summarize(abund = sum(abund)) %>% 
  filter(abund > 0) %>% 
  filter(abund == max(abund))

##
# variation in lake characteristics
locchrs <- select(all_potam, depth, area, perim, secchi, color, alk, tp, state, Ecoregion) %>% 
  group_by(state, Ecoregion) %>% 
  summarize_each(funs(mean(., na.rm = T)))
   
clichrs <- select(all_potam, tmean, tmax, tmin, prec, alt, state) %>% 
  group_by(state) %>% 
  summarize_each(funs(mean(., na.rm = T)))
     
ggplot(all_potam, aes(x = Longitude, y = Latitude, size = tmean)) + 
  geom_point()

##
# model eval
data(spp_var)
data(spp_varmod)

##
# assemb comp, rich
spp_var[, 1:3]

spp_varmod[[1]]
spp_varmod[[2]]

## 
# potam in dataset
pots <- grep('^P', names(all_potam), value = T)
pots <- pot_nms(pots, to_spp = T) %>% 
  grep('^Floating|Narrow|Broad', ., invert = T, value = T)

# species not modelled
# counts by lake of those not modelled
notmod <- sort(pots[!pots %in% names(spp_var)])
notmodcts <- all_potam[, pot_nms(notmod, to_spp = F)] %>% 
  apply(., 2, function(x) x > 0) %>% 
  colSums %>% 
  .[.>0]
notmod <- pot_nms(names(notmodcts), to_spp = T)

# where are they located
toget <- rowSums(all_potam[, names(notmodcts)]) > 0
all_potam$rares <- 'black'
all_potam[toget, 'rares'] <- 'blue'

# are the explanatory variables different between rare/not rare?
tomod <- dplyr::select(all_potam, depth, area, perim, secchi, color, alk, tp, Latitude, Longitude, tmean, tmax, tmin, prec, alt, rares) %>% 
  gather('var', 'val', -rares) %>% 
  split(., .$var) %>% 
  lapply(., function(x){
    t.test(val ~ rares, data = x)
    })

##
# exp var by species
meantot <- filter(spp_var, var == 'Total') %>% 
  dplyr::select(-var, -`Assemb. comp.`, -Richness) %>% 
  rowMeans
