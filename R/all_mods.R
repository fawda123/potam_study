######
# reproducing pRDA, pLR, and GLM for potamogeton analysis

library(vegan)
library(packfor)
library(dplyr)
library(venneuler)

load(file = 'data/all_potam.RData')

# add richness
# Janne used sum of all abundances as richness
all_potam <- select(all_potam, matches('^P', ignore.case = F)) %>% 
  rowSums %>% 
  data.frame(all_potam, S = .)

##
# community composition model
cc_mod <- pot_var(all_potam, '^P')

## 
# richness model
rich_mod <- pot_var(all_potam, '^S$')

##
# individual species mods, repeated for each species

pot_nms <- grep('^P', names(all_potam), ignore.case = F, value = T)

pot_mod <- vector('list', length(pot_nms))
names(pot_mod) <- pot_nms
for(pot in pot_nms){
 
  cat(pot, '\t') 
  tmp_mod <- try({pot_var(all_potam, paste0('^', pot, '$'))})
  
  # go to next variable if error
  if(inherits(tmp_mod, 'try-error')) next
  
  # append results
  pot_mod[[pot]] <- tmp_mod
  
}


