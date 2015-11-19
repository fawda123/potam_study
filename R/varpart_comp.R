library(tidyr)
library(dplyr)
library(ggplot2)

load(file = 'C:/Users/mbeck/Desktop/spp_var_old.RData')
spp_var_old <- spp_var

data(spp_var)

spp_var <- gather(spp_var, 'spp', 'exp', -var)

spp_var_old <- gather(spp_var_old, 'spp', 'exp', -var) %>% 
  mutate(
    spp = gsub('Assemb\\.\\.', 'Assemb\\. ', spp),
    spp = gsub('P\\.\\.', 'P\\. ', spp), 
    var = gsub('All three groups', 'Local + Climate + Space', var)
  )

# combine my data and Janne's for comparison
comps <- full_join(spp_var, spp_var_old, by = c('var', 'spp')) %>% 
  rename(
    exp_mb = exp.x,
    exp_ja = exp.y
  ) %>% 
  mutate(
    exp_mb = 100 * exp_mb
  )
  
ggplot(comps, aes(x = exp_ja, y = exp_mb)) + 
  geom_point() +
  theme_bw() + 
  facet_wrap(~var)

ggplot(comps, aes(x = exp_ja, y = exp_mb)) + 
  geom_point() +
  theme_bw() + 
  facet_wrap(~spp)


