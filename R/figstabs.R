######
# figs and tables

library(ggplot2)
library(maptools)
library(sp)
library(RColorBrewer)

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
  mutate(Richcat = cut(Richnum, breaks = c(-Inf, 1, 3, 6, 9, 12, Inf), 
    labels = c('1', '<3', '<6', '<9', '<12', '>12'))
    ) %>% 
  data.frame
labs <- list(expression(1), expression(phantom('')<=3), expression(phantom('')<=6), expression(phantom('')<=9), expression(phantom('')<=12), expression(phantom('')>12))

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
  scale_size_discrete('Richness', range = c(5, 13), labels = labs) + 
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