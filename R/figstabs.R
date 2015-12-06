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

# subset ecoregs
ecoregs <- ecoregs[!ecoregs$Ecoregion %in% c('DA', 'CCBP', 'LAP', 'NMW', 'SWTP'), ]
ecoregs$Ecoregion <- droplevels(ecoregs$Ecoregion)

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
  scale_colour_manual('Richness', values = rep('black', 5), labels = labs) +
  scale_fill_manual(values = brewer.pal(9, 'Greys')[c(3, 5, 7, 9)]) +
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
    fill = 'black') + 
  geom_polygon(data = wistate, aes(x = long, y = lat, group = group), 
    fill = 'black') +
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
# separte barplots by pure, shared, and total 

load(file = 'data/spp_var.RData')

# long format, minor name formatting, neg exp var floored at zero
toplo <- gather(spp_var, 'spp', 'exp', -var) %>% 
  mutate(
    exp = pmax(0, exp), 
    var = gsub('^Local \\+ Climate \\+ Space$', 'All', var),
    var = factor(
      var, 
      levels = c('Local', 'Climate', 'Space', 'Local + Climate', 'Climate + Space', 'Local + Space', 'All', 'Unexplained', 'Total')
    )
  ) %>% 
  filter(var != 'Unexplained') %>% 
  filter(!spp %in% c('Narrow-leaf Pondweed Group', 'Floating-leaf Water Smartweed Group')) %>% 
  mutate(
    var = droplevels(var),
    spp = droplevels(spp),
    var_comb = factor(var)
  ) %>% 
  data.frame
levels(toplo$var_comb) <- c('Pure', 'Pure', 'Pure', 'Shared', 'Shared', 'Shared', 'Shared', 'Total')

# color vectors
cols <- paste0('grey', c('90', '70', '50', '60', '20', '40', '10'))

# y limits
ylims <- c(0, 0.45)

# plot margins
margs <- grid::unit(c(0.1,0.1,0.1,0.1), "cm")

# bar width
bwid <- 0.8 

# total explained variance
toplo1 <- filter(toplo, var_comb == 'Total')

# get rank of total explained variance for each spp, keep assemb comp and richness first
levs <- c(1, 2, rev(2 + order(toplo1$exp[-c(1,2)])))
toplo1$spp <- factor(toplo1$spp, levels = levels(toplo1$spp)[levs])
p1 <- ggplot(toplo1, aes(x = spp, y = exp)) + 
  geom_bar(stat = 'identity', fill = NA, colour = 'black', width = bwid) + 
  theme_bw() +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), # element_text(angle = 90, hjust = 1, vjust = 0), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin = margs
    ) + 
  facet_wrap(~var_comb) + 
  geom_vline(xintercept = 2.5, size = 1) + 
  scale_y_continuous('% explained', limits = ylims)
  
# pure effects
toplo2 <- filter(toplo, var_comb == 'Pure')
toplo2$spp <- factor(toplo2$spp, levels = levels(toplo2$spp)[levs]) # sort levels by order
p2 <- ggplot(toplo2, aes(x = spp, y = exp, fill = var, order = -as.numeric(var))) + 
  geom_bar(stat = 'identity', width = bwid) + 
  theme_bw() +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), #element_text(angle = 90, hjust = 1, vjust = 0), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = c(1, 1), legend.justification = c(1, 1),
    plot.margin = margs
    ) + 
  facet_wrap(~var_comb) + 
  geom_vline(xintercept = 2.5, size = 1) + 
  scale_fill_manual(values = cols[1:3]) + 
  scale_y_continuous('% explained', limits = ylims)

# shared effects  
toplo3 <- filter(toplo, var_comb == 'Shared')
toplo3$spp <- factor(toplo3$spp, levels = levels(toplo3$spp)[levs])
p3 <- ggplot(toplo3, aes(x = spp, y = exp, fill = var, order = -as.numeric(var))) + 
  geom_bar(stat = 'identity', width = bwid) + 
  theme_bw() +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.position = c(1, 1), legend.justification = c(1, 1),
    plot.margin = margs
    ) + 
  facet_wrap(~var_comb) + 
  geom_vline(xintercept = 2.5, size = 1) + 
  scale_fill_manual(values = cols[4:7]) + 
  scale_y_continuous('% explained', limits = ylims)

# save
tiff('figs/fig2.tif', height = 8, width = 6, units = 'in', compression = 'lzw', res = 300, family = 'serif')
grid.arrange(p1, p2, p3, ncol = 1, heights = c(0.75, 0.75, 1))
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
