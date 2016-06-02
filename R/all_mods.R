######
# reproducing pRDA, pLR, and GLM for potamogeton analysis

rm(list = ls())

library(vegan)
library(packfor)
library(venneuler)
library(PCNM)
library(dplyr)
source('R/funcs.R')

set.seed(531)

load(file = 'data/all_potam.RData')

##
# community composition model
cc_mod <- pot_var_bla(all_potam, '^P')

## 
# richness model
rich_mod <- pot_var_bla(all_potam, '^S$')

##
# individual species mods, repeated for each species

pots <- grep('^P', names(all_potam), ignore.case = F, value = T)

# may have to run this a few times, POFR and PF should work
pot_mod <- vector('list', length(pots))
names(pot_mod) <- pots
for(pot in pots){
 
  # run spp mods
  cat(pot, '\t')
  tmp_mod <- try({pot_var_bla(all_potam, paste0('^', pot, '$'))})
  
  # did mod work?
  chk <- inherits(tmp_mod, 'try-error')
  
  # run again if it didn't and spp is POFR
  # it should work
  if(pot == 'POFR' & chk){
    while(chk){
      
      cat(pot, '\t')
      
      tmp_mod <- try({pot_var_bla(all_potam, paste0('^', pot, '$'))})
  
      # did mod work?
      chk <- inherits(tmp_mod, 'try-error')
      
    }
      
  }
    
  # otherwise go to next
  if(chk) next

  # append results if it worked
  pot_mod[[pot]] <- tmp_mod
  
}

##
# combine all results for those that worked
spp_var <- do.call('rbind', pot_mod) %>% 
  na.omit %>% 
  .[sort(row.names(.)), ] %>% 
  rbind(cc_mod, rich_mod, .) %>% 
  t %>% 
  data.frame(
    var = row.names(.), 
    row.names = seq(1, nrow(.)),
    .
    ) %>% 
  mutate(
    var = factor(
      var, 
      levels = c('loc', 'cli', 'spa', 'loc + cli', 'cli + spa', 'loc + spa', 'loc + cli + spa', 'res', 'tot'),
      labels = c('Local', 'Climate', 'Space', 'Local + Climate', 'Climate + Space', 'Local + Space', 'Local + Climate + Space', 'Unexplained', 'Total')
    )
  )

# col names formatting
names(spp_var)[grep('^P', names(spp_var))] <- pot_nms(names(spp_var)[grep('^P', names(spp_var))])
names(spp_var)[names(spp_var) %in% c('cc_mod', 'rich_mod')] <- c('Assemb. comp.', 'Richness')

# remove stupid species (groups and p. alpinus with very low total explained var
tokp <- c('^Narrow|^P\\. alpinus$|^Floating|^Broad')
tokp <- grep(tokp, names(spp_var), invert = T, ignore.case = F)
spp_var <- spp_var[, tokp]  

# save
save(spp_var, file = 'data/spp_var.RData')

######
# get actual models, only single category models (e.g., local, climate, or spatial)

##
# community composition model
cc_mod <- pot_var_bla(all_potam, '^P', mod_out = TRUE)

## 
# richness model
rich_mod <- pot_var_bla(all_potam, '^S$', mod_out = TRUE)

##
# individual species mods, repeated for each species
data(spp_var)
pots <- grep('^P\\.', names(spp_var), value = TRUE)
pots <- pot_nms(pots, to_spp = F)

# may have to run this a few times, sometimes forward.sel doesn't work
pot_mod <- list()
for(pot in pots){
 
  # run spp mods
  cat(pot, '\t')
  tmp_mod <- try({pot_var_bla(all_potam, paste0('^', pot, '$'), mod_out = TRUE)})
  
  # did mod work?
  chk <- inherits(tmp_mod, 'try-error')
  
  # run again if it didn't and spp is POFR
  # it should work
  if(pot == 'POFR' & chk){
    while(chk){
      
      cat(pot, '\t')
      
      tmp_mod <- try({pot_var_bla(all_potam, paste0('^', pot, '$'), mod_out = TRUE)})
  
      # did moded work?
      chk <- inherits(tmp_mod, 'try-error')
      
    }
      
  }
    
  # otherwise go to next
  if(chk) next

  # append results if it worked
  pot_mod[[pot]] <- tmp_mod
  
}

# combine all into list, save
spp_varmod <- c(cc_mod = list(cc_mod), rich_mod = list(rich_mod), pot_mod)

save(spp_varmod, file = 'data/spp_varmod.RData')

######
# save PCNM results

potam_xy <- all_potam[, c('Longitude', 'Latitude')]
potam_xy_d1 <- dist(potam_xy)
potam_PCNM <- PCNM(potam_xy_d1)

save(potam_PCNM, file = 'data/potam_PCNM.RData')
