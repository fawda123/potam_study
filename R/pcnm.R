library(ape)
library(spdep)
library(vegan)
library(ade4)

# these were downloaded from r-forge and installed as source files
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


######
# do PCNM by hand

# make a distance matrix (euclidean)
xy.d1 <- dist(mite.xy)

# this creates a tree that connects all points
# the minimum spanning tree is the tree that has the minimum total distance
# that is, multiple spanning trees can be created but they vary in total length
# the largest spanning distance is the greatest distance from one point to another, 
# within the spanning tree, this is used to define the neighborhood for PCNM as four times that value
spanning <- spantree(xy.d1)
par(mfrow = c(1, 3))
plot(mite.xy, type = 'n')
text(mite.xy, labels = 1:nrow(mite.xy))
plot(spanning, type = 't', labels = 1:nrow(mite.xy), cex = 1)
plot(spanning, mite.xy, type = 't', labels = 1:nrow(mite.xy), cex = 1)

# truncate the distance matrix by max
# p. 244 in Borcard ch 7, all point distance combos larger than the thresholds are given a large value
dmin <- max(spanning$dist)
xy.d1[xy.d1 < dmin] <- 4 * dmin

# then do PCNM on the truncated matrix
# k is the number of dimensions, eig says return the eigen values
xy.PCOA <- cmdscale(xy.d1, k = nrow(mite.xy) - 1, eig = TRUE)

# these are the number of eigenvectors that are positive
# only keep the positive ones, by default cmdscale only returns the positive values
nb.ev <- length(which(xy.PCOA$eig > 0.0000001))

mite.PCNM <- xy.PCOA$points

######
# do PCNM automatically, basically combines all the above steps
# truncation distance is still the max distance in the minimum spanning tree
libary(PCNM)

# calculate and get summary
xy.d1 <- dist(mite.xy)
mite.PCNM.auto <- PCNM(xy.d1)
summary(mite.PCNM.auto)

# expected Moran I value and all values for each of 43 eigenvectors
mite.PCNM.auto$expected_Moran
mite.PCNM.auto$Moran_I

# get only the eigenvectors with positive Moran I
sel <- which(mite.PCNM.auto$Moran_I$Positive == TRUE)
mite.PCNM.pos <- as.data.frame(mite.PCNM.auto$vectors[, sel])


######
# for potam data
data(all_potam)

potam.xy <- all_potam[, c('Longitude', 'Latitude')]

potam.xy.d1 <- dist(potam.xy)

potam.PCNM <- PCNM(potam.xy.d1)
spanning <- potam.PCNM$spanning

# spanning tree
par(mfrow = c(1, 2))
plot(potam.xy, type = 'n')
text(potam.xy, labels = 1:nrow(potam.xy))
# plot(spanning, type = 't', labels = 1:nrow(potam.xy), cex = 1)
plot(spanning, potam.xy, type = 't', labels = 1:nrow(potam.xy), cex = 1)

