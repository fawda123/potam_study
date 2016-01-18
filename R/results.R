library(dplyr)
library(tidyr)

data(all_potam)

##
# mean richness by ecoregion, state
rich <- dplyr::select(all_potam, matches('^P', ignore.case = F)) %>% 
  apply(., 2, function(x) pmin(1, x)) %>% 
  rowSums %>% 
  data.frame(Ecoregion = all_potam$Ecoregion, state = all_potam$state, rich = .) %>% 
  
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
# lakechr <- select(all_potam, 
