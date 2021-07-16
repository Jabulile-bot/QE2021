#Quatntitative Ecology
#Topic 8: Correspondence Analysis
#Author: Esther(Jabulile)Leroko

#Load packages
library(tidyverse)
library(vegan)

# The Doubs River Data (Spp) ----------------------------------------------
setwd("C:/QE2021/Quantitative_Ecology-main/Num_Ecol_R_book_ed1")
spe <- read.csv("C:/QE2021/Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsSpe.csv")
spe <- dplyr::select(spe, -1)
head(spe, 8)

#Do the CA
spe_ca <- cca(spe)
#error because there is a row that has no species at all, find it and remove it
apply(spe, 1, sum) #row 8 has 0 species
#Omit row 8
spe <- spe[rowSums(spe) > 0, ]
head(spe, 8)

#Now, do CA
spe_ca <- cca(spe)
spe_ca
summary(spe_ca)
#The output looks similar to that of a PCA. The important things to note are:
#the inertia (unconstrained and total inertia are the same), 
#the Eigenvalues for the unconstrained axes, 
#the Species scores, and 
#the Site scores. 
#Their interpretation is the same as before, but we can reiterate. Let us ...
#calculate the total inertia:
round(sum(spe_ca$CA$eig), 5)

#The inertia for the first axis (CA1) is:
round(spe_ca$CA$eig[1], 5)

#The inertia of CA1 and CA2 is:
round(sum(spe_ca$CA$eig[1:2]), 5)

#The fraction of the variance explained by CA1 and CA2 is:
round(sum(spe_ca$CA$eig[1:2]) / sum(spe_ca$CA$eig) * 100, 2) # result in %

summary(spe_ca)
#Species scores...The most positive and most negative eigenvectors (or loadings) 
#indicate those species that dominate in their influence along particular CA axes

#For example, CA1 will be most heavily loaded by the species Cogo and Satr 
#(eigenvectors of 1.50075 and 1.66167, respectively).
# If there is an environmental gradient, it will be these species that will be 
#most affected.
# At the very least, we can say that the contributions of these species are 
#having an overriding influence on the community differences seen between sites.

#Site scores are also as seen earlier in PCA. The highest positive or negative 
#loadings indicate sites that are dispersed far apart on the biplot 
#(in ordination space). They will have large differences in fish community composition.

# ORDINATION DIAGRAMS -----------------------------------------------------

plot(spe_ca, scaling = 1, main = "CA fish abundances - biplot scaling 1")
plot(spe_ca, scaling = 2, main = "CA fish abundances - biplot scaling 2")

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe, tmp <- ordisurf(spe_ca ~ Satr, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Satr"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Scer, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Scer"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Teso, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Teso"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Cogo, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Cogo"))
abline(h = 0, v = 0, lty = 3)

# a posteriori projection of environmental variables in a CA
env <- read.csv("C:/QE2021/Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsEnv.csv")
env <- dplyr::select(env, -1)

# we removed the 8th row in spe, so do it here too
env <- dplyr::slice(env, -8)

# the last plot produced (CA scaling 2) must be active
# scaling 2 is default
(spe_ca_env <- envfit(spe_ca, env, scaling = 2))
plot(spe_ca_env, col = "blue")
plot(spe_ca_env, p.max = 0.05, col = "red") # plot significant variables with a different colour


# QUESTIONS ---------------------------------------------------------------

1.How would you explain the patterns seen in the four panels of the above figure?

  -The species Teso and Cogo are most abundant at sites with highest
   O2 concentrations so, you are more likely to find them co-existing wherever
   there is O2 is highest.
  -Satr on the other hand has a wider O2 range, it can tolerate O2 conentrations
   from high to medium, hence it is most abundant from the top of the mountain
   to the mid-ridges of the river.
  -Scer is most abundant at the bottom ridges, away from the source, where water
   has slowed down, less pollution so, nitrate is high, the amount of bod is
   increased, lowering the O2 conentrations. So this species can tolerate and 
   thrives better in O2-deprived areas.
   
2.Apply approaches taken from the analysis shown immediately above to these 
datasets:

# Bird communities in Mt. Yushan ------------------------------------------

ybirds.spe <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_spe.txt', row.names = 1)
ybirds.spe[1:7, 1:7]

#Convert to tibble
ybirds.spe <- as_tibble(ybirds.spe)  

#Do the CA
spe_ca <- cca(ybirds.spe)  
spe_ca
summary(spe_ca)

#Ordination diagrams
par(mfrow = c(1, 2))
plot(spe_ca, scaling = 1, main = "CA Bird abundances - biplot scaling 1")
plot(spe_ca, scaling = 2, main = "CA Bird abundances - biplot scaling 2")

INTERPRETATION: The species ALA and WRN are most abundant at sites 46-50, making
these sites very different from the other sites. Species VRF, FLT and JBR 
coexist in the mid-latilutes sites. There is high bird species diversity in the
low to mid latitudes.

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(ybirds.spe, tmp <- ordisurf(spe_ca ~ BGW, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "BGW"))
abline(h = 0, v = 0, lty = 3)
with(ybirds.spe, tmp <- ordisurf(spe_ca ~ WBB, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "WBB"))
abline(h = 0, v = 0, lty = 3)
with(ybirds.spe, tmp <- ordisurf(spe_ca ~ ALA, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "ALA"))
abline(h = 0, v = 0, lty = 3)
with(ybirds.spe, tmp <- ordisurf(spe_ca ~ WRN, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "WRN"))
abline(h = 0, v = 0, lty = 3)

# a posteriori projection of environmental variables in a CA
ybirds.env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_env.txt', row.names = 1)
ybirds.env <- as_tibble(ybirds.env[,-c(1,2)])
# the last plot produced (CA scaling 2) must be active
# scaling 2 is default
(spe_ca_env <- envfit(spe_ca, ybirds.env, scaling = 2))
plot(spe_ca_env, col = "blue")
plot(spe_ca_env, p.max = 0.05, col = "red") # plot significant variables with a different colour

INTERPRETATION:Species ALA and WRN coexist in the sites on the top of the
mountain where elevation,conifer percentage and slpe steepness are higher,
although the niche of WRN is wider.
The species BGW and WBB also co-exist, but their niche is in the mid-latitudes
where the trees are big and tall, with high tree foliage volume, big stems, and
high canopy cover


# Alpine communities ------------------------------------------------------
names(spe) <- paste("sp", 1:82, sep = "")

library(ade4)
spe <- data(aravo)
spe <- (aravo[["spe"]]) 

#Do the PCA
spe_ca <- cca(spe)
spe_ca
summary(spe_ca)

#Ordination plots
par(mfrow = c(1, 2))
plot(spe_ca, scaling = 1, main = "CA alpine abundances - biplot scaling 1")
plot(spe_ca, scaling = 2, main = "CA alpine abundances - biplot scaling 2")

INTERPRETATION: The alpine trees are more or less distributed evenly across the
mountain with a few outliers. The results of CA1 and CA2 have a variance of 25%
so, they cannot be trusted.
require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe, tmp <- ordisurf(spe_ca ~ Poa.supi, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Poa.supi"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Sali.retu, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Sali.retu"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Cirs.acau, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Cirs.acau"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Alch.vulg, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Alch.vulg"))
abline(h = 0, v = 0, lty = 3)

# a posteriori projection of environmental variables in a CA
env <- data(aravo)
env <- as_tibble(aravo[["env"]]) 
env <- env[, -c(3,5)]

# the last plot produced (CA scaling 2) must be active
# scaling 2 is default
(spe_ca_env <- envfit(spe_ca, env, scaling = 2))
plot(spe_ca_env, col = "blue")
plot(spe_ca_env, p.max = 0.05, col = "red") # plot significant variables with a different colour


INTERPRETATION:Species Circ.acau and Alch.vulg coexist and are most abundant
at mid-latitudes where slope is more or less 45 degress and snow cover is medium.
Species Poa.supi is most abundant down the mountain at lower altitudes where
there is less snow, but as you move up the mountain, the abundance decreases
significantly.
Species Sali.retu is most abundant closer to the mountain tops, where physical
disturbance and snow cover are high, but as you move down the mountain the
abundance decreases significantly.