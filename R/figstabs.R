######
# figs and tables

library(ggplot2)
library(maptools)
library(sp)

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

ecoregs <- readShapeSpatial('M:/GIS/mnwi_eco3utm.shp')
mnstate <- readShapeSpatial('M:/GIS/MN/state.shp')
wistate <- readShapeSpatial('M:/GIS/WI/WI.shp')
mncounties <- readShapeSpatial('M:/GIS/MN/bdry_counpy2.shp')
wicounties <- readShapeSpatial('M:/GIS/WI/WI_counties.shp')
country <- readShapeSpatial('M:/GIS/usa48_utm15.shp')
country <- thinnedSpatialPoly(country, tolerance = 10000, topologyPreserve = TRUE, 
  avoidGEOS = FALSE)

mnstate <- fortify(mnstate)
wistate <- fortify(wistate)
ecoregs <- fortify(ecoregs)
mncounties <- fortify(mncounties)
wicounties <- fortify(wicounties)
# ecoregs <- ecoregs[ecoregs$id %in% c(2, 3, 4), ]
# ecoregs$Ecoregion <- factor(ecoregs$id, levels = c(2, 3, 4), 
#   labels = c("NF", "ETF", "GP"))

toplo <- all_potam

p1 <- ggplot(mncounties, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = NA, colour = 'grey') +
  geom_polygon(data = wicounties, aes(group = group), 
    fill = NA, colour = 'grey') +
  geom_polygon(data = wistate, aes(x = long, y = lat, group = group), fill = NA, 
    colour = 'black') +
  geom_polygon(data = mnstate, aes(x = long, y = lat), fill = NA, colour = 'black') + 
  geom_polygon(data = ecoregs, aes(x = long, y = lat, group = group, fill= id),  
    alpha = 0.5) +
#   geom_point(data = toplo, aes(x = UTM_X, y = UTM_Y, colour = CarpKg, size = CarpKg), alpha = 0.8) +
#   scale_size_discrete('Carp kg/net', range = c(4, 12), labels = labs) + 
#   scale_colour_manual('Carp kg/net', values = terrain.colors(6), labels = labs) +
  theme_classic() + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank(), 
          legend.box.just = 'right'
    ) +
  coord_equal()

p1leg <- g_legend(p1)
p1 <- p1 + theme(legend.position = 'none')

p2 <- ggplot(state, aes(x = long, y = lat)) + 
  geom_polygon(data = counties, aes(x = long, y = lat, group = group), 
    fill = NA, colour = 'grey') +
  geom_polygon(data = ecoregs, aes(x = long, y = lat, group = group, fill= Ecoregion),  
    alpha = 0.5, show_guide = FALSE) +
  geom_polygon(fill = NA, colour = 'black') +
  geom_point(data = toplo, aes(x = UTM_X, y = UTM_Y, colour = S_rich, size = S_rich), alpha = 0.8) +
  guides(colour = guide_legend("Plant\nrichness"), size = guide_legend("Plant\nrichness")) +
  scale_size(range = c(4, 12), breaks = c(1, 5, 10, 15, 25)) + 
  scale_colour_gradientn(colours = c(RColorBrewer::brewer.pal(9, 'Blues')[4:9]),
    breaks = c(1, 5, 10, 15, 25)) + 
  theme_classic() + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank()#, 
          # legend.position = 'none'
    ) +
  coord_equal()

p2leg <- g_legend(p2)
p2 <- p2 + theme(legend.position = 'none')

pinset <- ggplot(country, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = NA, colour = 'grey') +
  geom_polygon(data = state, aes(x = long, y = lat, group = group), 
    fill = 'tomato1') + 
  theme_classic() + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank()#, 
          # legend.position = 'none'
    ) +
  coord_equal()

tiff('figs/fig1.tif', height = 9, width = 6, units = 'in', compression = 'lzw', res = 300, family = 'serif')
grid.newpage()
v1 <- viewport(width = 1, height = 0.55, x = 0.4, y = 0.75) 
v2 <- viewport(width = 0.35, height = 0.35, x = 0.86, y = 0.92)
v3 <- viewport(width = 1, height = 0.55, x = 0.4, y = 0.25)
v4 <- viewport(width = 1, height = 1, x = 0.86, y = 0.6)
v5 <- viewport(width = 1, height = 1, x = 0.86, y = 0.25)
print(p1, vp = v1) 
print(pinset, vp = v2)
print(p2, vp = v3)
upViewport(0)
pushViewport(v4)
grid.draw(p1leg)
upViewport(0)
pushViewport(v5)
grid.draw(p2leg)
dev.off()