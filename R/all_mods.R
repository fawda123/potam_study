######
# reproducing pRDA, pLR, and GLM for potamogeton analysis

library(vegan)
library(packfor)
library(venneuler)
library(PCNM)
library(dplyr)

load(file = 'data/all_potam.RData')

# add richness
# Janne used sum of all abundances as richness
all_potam <- dplyr::select(all_potam, matches('^P', ignore.case = F)) %>% 
  rowSums %>% 
  data.frame(all_potam, S = .)

##
# community composition model
cc_mod <- pot_var_bla(all_potam, '^P')

## 
# richness model
rich_mod <- pot_var_bla(all_potam, '^S$')

##
# individual species mods, repeated for each species

pots <- grep('^P', names(all_potam), ignore.case = F, value = T)

pot_mod <- vector('list', length(pots))
names(pot_mod) <- pots
for(pot in pots){
 
  cat(pot, '\t') 
  tmp_mod <- try({pot_var_bla(all_potam, paste0('^', pot, '$'))})
  
  # go to next variable if error
  if(inherits(tmp_mod, 'try-error')) next
  
  # append results
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

pot_mod <- list()
for(pot in pots){
 
  cat(pot, '\t') 
  tmp_mod <- try({pot_var_bla(all_potam, paste0('^', pot, '$'), mod_out = TRUE)})
  
  # go to next variable if error
  if(inherits(tmp_mod, 'try-error')) next
  
  # append results
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
