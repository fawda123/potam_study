######
# figs and tables

library(maptools)
library(ggplot2)
library(dplyr)
library(tidyr)
library(vegan)

######
# figs

##
# map

load(file = 'data/all_potam.RData')

# get legend from an existing ggplot object
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# spatial data
ecoregs <- readShapeSpatial('M:/GIS/mnwi_eco3utm.shp')
mnstate <- readShapeSpatial('M:/GIS/MN/state.shp')
wistate <- readShapeSpatial('M:/GIS/WI/WI.shp')
mncounties <- readShapeSpatial('M:/GIS/MN/bdry_counpy2.shp')
wicounties <- readShapeSpatial('M:/GIS/WI/WI_counties.shp')
country <- readShapeSpatial('M:/GIS/usa48_utm15.shp')
country <- thinnedSpatialPoly(country, tolerance = 10000, topologyPreserve = TRUE, 
  avoidGEOS = FALSE)
potams <- readShapeSpatial('M:/GIS/all_potam.shp')

# make ggplot format
mnstate <- fortify(mnstate)
wistate <- fortify(wistate)
ecoregs <- fortify(ecoregs)
mncounties <- fortify(mncounties)
wicounties <- fortify(wicounties)
ecoregs$Ecoregion <- factor(ecoregs$id, levels = c(0:8),
  labels = c('CCBP', 'DA', 'LAP', 'NCHF', 'NGP', 'NLF', 'NMW','SWTP', 'WCBP'))

# get richness from potams
toplo <- data.frame(potams) %>% 
  mutate(Richnum = rowSums(.[, grepl('^P', names(.), ignore.case = F)] > 0)) %>% 
  mutate(Richcat = cut(Richnum, breaks = c(-Inf, 1, 3, 6, 9, Inf), 
    labels = c('1', '<3', '<6', '<9', '>9'))
    ) %>% 
  data.frame
labs <- list(expression(1), expression(phantom('')<=3), expression(phantom('')<=6), expression(phantom('')<=9), expression(phantom('')>9))

# MN and WI maps
p1 <- ggplot(mncounties, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = NA, colour = 'grey') +
  geom_polygon(data = wicounties, aes(group = group), 
    fill = NA, colour = 'grey') +
  geom_polygon(data = wistate, aes(x = long, y = lat, group = group), fill = NA, 
    colour = 'black') +
  geom_polygon(data = mnstate, aes(x = long, y = lat), fill = NA, colour = 'black') + 
  geom_polygon(data = ecoregs, aes(x = long, y = lat, group = group, fill= Ecoregion),  
    alpha = 0.5) +
  geom_point(data = toplo, aes(x = Longitude, y = Latitude, 
    size = Richcat, colour = Richcat), alpha = 0.8) +
  scale_size_discrete('Richness', range = c(2, 8), labels = labs) + 
  scale_colour_manual('Richness', values = brewer.pal(9, 'Reds')[3:8], labels = labs) +
  theme_classic() + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),
            panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank(), 
          legend.box.just = 'right'
    ) +
  coord_equal()

# inset map
pinset <- ggplot(country, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = NA, colour = 'grey') +
  geom_polygon(data = mnstate, aes(x = long, y = lat, group = group), 
    fill = 'tomato1') + 
  geom_polygon(data = wistate, aes(x = long, y = lat, group = group), 
    fill = 'tomato1') +
  theme_classic() + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
    axis.text.y=element_blank(),axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.background=element_blank(),panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),plot.background=element_blank()#, 
          # legend.position = 'none'
    ) +
  coord_equal()

# save
tiff('figs/fig1.tif', height = 6, width = 8, units = 'in', compression = 'lzw', res = 300, family = 'serif')
grid.newpage()
v1 <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) 
v2 <- viewport(width = 0.25, height = 0.25, x = 0.6, y = 0.87)
print(p1, vp = v1) 
print(pinset, vp = v2)
dev.off()

##
# barplot of var part by species

load(file = 'data/spp_var.RData')

# long format, minor name formatting, neg exp var floored at zero
toplo <- gather(spp_var, 'spp', 'exp', -var) %>% 
  mutate(
    exp = pmax(0, exp), 
    var = gsub('^Local \\+ Climate \\+ Space$', 'All', var),
    var = factor(
      var, 
      levels = c('Local', 'Climate', 'Space', 'Local + Climate', 'Climate + Space', 'Local + Space', 'All', 'Unexplained')
    )
  ) %>% 
  filter(var != 'Unexplained') %>% 
  filter(!spp %in% c('Narrow-leaf Pondweed Group', 'Floating-leaf Water Smartweed Group')) %>% 
  group_by(spp) %>% 
  mutate(Total = sum(exp)) %>% 
  ungroup %>% 
  mutate(
    var = droplevels(var),
    spp = droplevels(spp),
    var_comb = factor(var)
  ) %>% 
  data.frame
levels(toplo$var_comb) <- c('Pure', 'Pure', 'Pure', 'Shared', 'Shared', 'Shared', 'Shared', 'Total')

p <- ggplot(toplo, aes(x = spp, y = exp, fill = var)) + 
  geom_bar(stat = 'identity') + 
  theme_bw() +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
    ) + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(toplo$var)), 'Spectral')) + 
  scale_y_continuous('% explained')
  
# save
tiff('figs/fig2.tif', height = 4, width = 7, units = 'in', compression = 'lzw', res = 300, family = 'serif')
print(p)
dev.off()

##
# rda biplots of species by local and climate variables

data(all_potam)
data(spp_var)

# select species in spp_var (models that worked) from all_potam, hellinger transform
pots <- grep('^P\\.', names(spp_var), ignore.case = F, value = T) %>% 
  gsub('^P\\.', 'Potamogeton', .) %>% 
  pot_nms(., to_spp = FALSE)
spp <- select(all_potam, matches(paste(pots, collapse = '|'), ignore.case = F)) %>% 
  decostand(., method = 'hellinger')

# exp variable columns
loc_nm <- c('alk','color', 'tp', 'secchi', 'area', 'depth', 'perim')
cli_nm <- c('tmean', 'tmax', 'tmin', 'prec', 'alt')
spa_nm <- '^V'
loc <- select(all_potam, matches(paste(loc_nm, collapse = '|')))
cli <- select(all_potam, matches(paste(cli_nm, collapse = '|')))
spa <- select(all_potam, matches(spa_nm, ignore.case = F))

# rda mods
mod_loc <- rda(spp, loc)
mod_cli <- rda(spp, cli)

# biplots
tiff('figs/fig3.tif', height = 8, width = 5, units = 'in', compression = 'lzw', res = 500, family = 'serif')
par(mfrow = c(2, 1), mar = c(4.5, 4.5, 0.5, 0.5))

plot(mod_loc, type = 'n', xlim = c(-1, 1))
points(mod_loc, pch=21, col=scales::alpha("red", 0.4), bg=scales::alpha("green", 0.4), cex=0.8)
text(mod_loc, dis = 'cn',  axis.bp = FALSE)
text(mod_loc, "species", col="blue", cex=0.8)

plot(mod_cli, type = 'n', xlim = c(-1, 1))
points(mod_cli, pch=21, col=scales::alpha("red", 0.4), bg=scales::alpha("green", 0.4), cex=0.8)
text(mod_cli, dis = 'cn', axis.bp = FALSE)
text(mod_cli, "species", col="blue", cex=0.8)
dev.off()

# ##
# #  cumulative species plot by lakes
# library(dplyr)
# 
# data(all_potam)
# 
# toplo <- select(all_potam, matches('^P', ignore.case = FALSE)) %>% 
#   apply(., 2, pmin, 1) %>% 
#   .[, colSums(.) > 5] %>% 
#   apply(., 2, function(x) cumsum(x)/(max(cumsum(x)))) %>% 
#   as.data.frame %>% 
#   mutate(lakes = 1:nrow(.)) %>% 
#   gather('spp', 'cumsum', -lakes) %>% 
#   mutate(spp = pot_nms(spp)) %>% 
#   filter(spp != 'Narrow-leaf Pondweed Group')
# 
# ggplot(toplo, aes(x = lakes, y = cumsum, colour = spp)) +
#   geom_line(size = 1) + 
#   # geom_point() + 
#   theme_bw()


######
# tables

##
# tab 1 summary table

load(file = 'data/all_potam.RData')

# summarize variables by category
loc <- select(all_potam, alk, color, tp, secchi, area, depth, perim) %>% 
  gather('var', 'val') %>% 
  group_by(var) %>% 
  summarise(
    mean = mean(val, na.rm = T), 
    min = min(val, na.rm = T),
    max = max(val, na.rm = T), 
    sd = sd(val, na.rm = T)
  )
cli <- select(all_potam, tmean, tmax, tmin, prec, alt) %>% 
  gather('var', 'val') %>% 
  group_by(var) %>% 
  summarise(
    mean = mean(val, na.rm = T), 
    min = min(val, na.rm = T),
    max = max(val, na.rm = T), 
    sd = sd(val, na.rm = T)
  )

out <- rbind(loc, cli) %>% 
  data.frame

write.csv(out, 'tabs/tab1.csv', quote = F, row.names = F)

##
# tab 2 explained variance

load(file = 'data/spp_var.RData')

# long format and minor name formatting
totab <- gather(spp_var, 'spp', 'exp', -var) %>% 
  mutate(
    var = gsub('^Local \\+ Climate \\+ Space$', 'All', var),
    var = factor(
      var, 
      levels = c('Local', 'Climate', 'Space', 'Local + Climate', 'Climate + Space', 'Local + Space', 'All', 'Unexplained')
    ), 
    exp = 100 * exp
  ) %>% 
  filter(!spp %in% c('Narrow-leaf Pondweed Group', 'Floating-leaf Water Smartweed Group')) %>% 
  spread(var, exp)

names(totab)[names(totab) %in% 'spp'] <- ''

write.csv(totab, 'tabs/tab2.csv', quote = F, row.names = F)

##
# tab 3 significant variables for individual rda or glm mods used in varpart

data(spp_varmod)

totab <- pot_summ(spp_varmod)

write.csv(totab, 'tabs/tab3.csv', row.names = F, quote = F)
