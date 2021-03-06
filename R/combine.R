######
# combines finished mn_potam and wi_potam datasets 
# respective files were created in mn_proc.R and wi_proc.r
# the final file includes all lakes from MN and WI that:
#  - have complete supporting data, and
#  - contain potamogeton species

######
# combine datasets

# load data
data(mn_potam)
data(wi_potam)

# create state variable
mn_potam$state <- 'MN'
wi_potam$state <- 'WI'

# wide format by species, combine
library(reshape2)
library(tidyr)

mn_var <- grep('^P|^tot$', names(mn_potam), value = T)
mn_potam <- melt(mn_potam, measure.vars = mn_var)
wi_var <- grep('^P|^tot$', names(wi_potam), value = T)

wi_potam <- melt(wi_potam, measure.vars = wi_var)

all_potam <- rbind(mn_potam, wi_potam)

# back to long format
all_potam <- spread(all_potam, key = 'variable', value = 'value')

# replace NA potams with zero
all_var <- grep('^P', names(all_potam), value = T)

pots <- all_potam[, all_var]
pots[is.na(pots)] <- 0

all_potam[, all_var] <- pots

# remove lakes w/o potams
to_keep <- rowSums(pots) > 0
all_potam <- all_potam[to_keep, ]

# retain lakes w/ complete data
all_potam <- na.omit(all_potam)
row.names(all_potam) <- seq(1, nrow(all_potam))

# remove optional column
all_potam$optional <- NULL

# remove nuisance lake
all_potam <- all_potam[!all_potam$lake %in% '73019900', ]
row.names(all_potam) <- 1:nrow(all_potam)

##
# get PCNM eigen vectors with positive Moran
# see pcnm.R

library(PCNM)

potam.xy <- all_potam[, c('Longitude', 'Latitude')]

potam.xy.d1 <- dist(potam.xy)

potam.PCNM <- PCNM(potam.xy.d1)
potam.PCNM <- potam.PCNM$vectors[, potam.PCNM$Moran_I$Positive]
potam.PCNM <- data.frame(potam.PCNM)
names(potam.PCNM) <- paste0('V', 1:ncol(potam.PCNM))

all_potam <- data.frame(all_potam, potam.PCNM)

# save
save(all_potam, file = 'data/all_potam.RData')

##
# add ecoregions to the data
# remove lakes with very low sample effort (tot < 20)
# add total richness (S)

library(maptools)
library(sp)

data(all_potam)

# get spatial data, then overlay
ecoregs <- readShapeSpatial('M:/GIS/mnwi_eco3utm.shp')
potams <- readShapeSpatial('M:/GIS/all_potam.shp') 
tmp <- sp::over(potams, ecoregs)

# add overlay column to all_potam (order is the same)
all_potam$Ecoregion <- tmp$US_L3NAME

# keep total survey points > 20
all_potam <- all_potam[all_potam$tot > 20, ]
row.names(all_potam) <- 1:nrow(all_potam)

# subsets columns with potam, change freq occurrence to binary, take sum, add to all_potam
# take log10
S <- dplyr::select(all_potam, matches('^P', ignore.case = F)) %>% 
  as.matrix(.)
S[S > 0] <- 1
S <- log10(rowSums(S))
all_potam$S <- S

# arcsin square root P data
toget <- grep('^P', names(all_potam))
all_potam[, toget] <- asin(sqrt(all_potam[, toget]))
all_potam[, toget] <- vegan::decostand(all_potam[, toget], method = 'hellinger')

# save
save(all_potam, file = 'data/all_potam.RData')
