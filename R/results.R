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
locchrs <- select(all_potam, depth, area, perim, secchi, color, alk, tp, state, Ecoregion) %>% 
  group_by(state, Ecoregion) %>% 
  summarize_each(funs(mean(., na.rm = T)))
   
clichrs <- select(all_potam, tmean, tmax, tmin, prec, alt, state) %>% 
  group_by(state) %>% 
  summarize_each(funs(mean(., na.rm = T)))
     
ggplot(all_potam, aes(x = Longitude, y = Latitude, size = tmean)) + 
  geom_point()
