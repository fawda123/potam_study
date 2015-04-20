library(ape)
library(spdep)
library(vegan)
library(ade4)

# these were downloaded from r-forge and instaled as source files
library(packfor)
library(spacemakeR)
library(AEM)
library(PCNM)

# mite data from vegan
data(mite.xy) # coords
data(mite) # species data at each site, counts?
data(mite.env) # environmental data

# hellinger transform of mite
mite.h <- decostand(mite, 'hellinger')
