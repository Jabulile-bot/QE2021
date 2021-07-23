#Quantitative Ecology
#Topic 11: non-metric multidimensional scaling
#Author:Esther(Jabulile)Leroko

#Fetch packages
library(tidyverse)
library(vegan)

# The Doubs River Data ----------------------------------------------------
#Fetch the dataset
setwd("C:/QE2021/Quantitative_Ecology-main/Num_Ecol_R_book_ed1")
spe <- read.csv("C:/QE2021/Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsSpe.csv")
spe <- dplyr::select(spe, -1)#remove the repetitive first column
spe <- dplyr::slice(spe, -8)#row 8 has no species present

#Do the nMDS
spe_nmds <- metaMDS(spe, distance = "bray") #Have to tell it what distance matrix
#to use
spe_nmds

#ORDINATION DIAGRAMS
#Same as before, but 3 new concepts: Stress, Shepard plots, goodness of fit

#Stress indicates the scatter of observed dissimilarities against an expected 
#monotone regression

#Shepard diagram plots ordination distances against original dissimilarities, 
#and adds a monotone or linear fit line to highlight this relationship
#The stressplot() function also produces two fit statistics

#The goodness-of-fit of the ordination is measured as the $R^{2}$ of either a 
#linear or a non-linear regression of the nMDS distances on the original ones

par(mfrow = c(2, 2))
stressplot(spe_nmds, main = "Shepard plot")
ordiplot(spe_nmds, type = "t", cex = 1.5, 
         main = paste0("nMDS stress = ", round(spe_nmds$stress, 2)))
gof = goodness(spe_nmds)
plot(spe_nmds, type = "t", main = "Goodness of fit")
points(spe_nmds, display = "sites", cex = gof * 200) # bigger bubbles indicate a worse fit
#Stress value greater than 0.2 is bad

#We can also build ordination plots from scratch to suit specific needs:\
pl <- ordiplot(spe_nmds, type = "none", main = "nMDS fish abundances ")
points(pl, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl, "species", col = "blue4", cex = 0.9)
text(pl, "sites", col = "red4", cex = 0.9)

#Or we can fit response surfaces using ordisurf() and project environmental drivers:

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe, tmp <- ordisurf(spe_nmds ~ Satr, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Satr"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_nmds ~ Scer, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Scer"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_nmds ~ Teso, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Teso"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_nmds ~ Cogo, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Cogo"))
abline(h = 0, v = 0, lty = 3)

env <- read.csv("C:/QE2021/Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsEnv.csv")
env <- dplyr::select(env, -1)
env <- dplyr::slice(env, -8)

(spe_nmds_env <- envfit(spe_nmds, env)) 
plot(spe_nmds_env, col = "blue")
plot(spe_nmds_env, p.max = 0.05, col = "red")


# QUESTIONS ---------------------------------------------------------------

1.Using two unconstrained ordination techniques of your choice, analyse the mite 
data in the vegan package. Provide a brief description and discussion of what you
have found, and produce the R code
2.Using two unconstrained ordination techniques of your choice 
(not already used in 1, above) analyse the dune data in the vegan package. 
Provide a brief description and discussion of what you have found, and produce 
the R code.

# Mites -------------------------------------------------------------------
#We will use PCoA and nMDS for mites

#Fetch data
data("mite")
spe <- mite
spe[1:6, 1:6] #have a glance at the first 6 rows and columns

#Calc a suitable dissimilarity matrix
spe_bray <- vegdist(spe)

#Do PCoA
spe_pcoa <- capscale(spe ~ 1, distance = "bray")
spe_pcoa
summary(spe_pcoa)

#Ordination diagrams
par(mfrow = c(1, 2))
plot(spe_pcoa, scaling = 1, main = "PCoA mites abundances - biplot scaling 1")
plot(spe_pcoa, scaling = 2, main = "PCoA mites abundances - biplot scaling 2")

INTERPRETATION: 
-Most of the mite species co-occur, with the exception of a few species.
Species LCIL is more likely to be abundant at sites 38, 43, 59 
and 67; which are sites with high water content, with less dense sphagnum species
growing horizontally and no shrub species. 
LRUG also tends to be not assocated with any other species and is more abundant
at the more moist sites with less substrate density. \
Then we have the species ONOV, SUCT, TVEL which coexist at the most dense sites,
with high shrub presence, close to the forest.


#We can fit response surfaces using ordisurf():
require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe, tmp <- ordisurf(spe_pcoa ~ LCIL, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "LCIL"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_pcoa ~ LRUG, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "LRUG"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_pcoa ~ ONOV, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "ONOV"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_pcoa ~ Trhypch1, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Trhypch1"))
abline(h = 0, v = 0, lty = 3)
#then fit the env variables
data("mite.env")
env <- mite.env
#make all columns numerical
env$Substrate <- as.numeric(env$Substrate)
env$Shrub <- as.numeric(env$Shrub)
env$Topo <- as.numeric(env$Topo)
env[1:5, ] #it worked

(spe_pcoa_env <- envfit(spe_pcoa, env, scaling = 2)) 
plot(spe_pcoa_env, col = "blue")
plot(spe_pcoa_env, p.max = 0.05, col = "red")

INTERPRETATION:
There is moisture gradient increasing from left to right. Closer to the forest,
there is very low moisture content, there is high abundance of shrubs, the 
sphagnum species are frowing in hummocks to prevent losing too much water, so
these sites are very dense. But as you move towards the lake, moisture content
increases significantly, and so does the the diversity of mites and mosses.
These 4 species in the diagram are amongst the most influential species on the 
commununity of the mites and they are abundant at different sites, influenced by
the different environmental conditions.
LCIL and Trhypch1 have overlapping niches, they are both abundant at the sites
in the middle, with high water content but a little more dense as they still
have a few shrubs present and some sphagnum species growing in hummocks. But
Trhypch1 occupies a wider range.
LRUG is most abundant at the sites with the highest water content, no shrubs
present and all sphagnum species growing horozontally as they do not have to
worry about drying out. While ONOV grows on the other end of the environmental 
spectrum, the driest sites with abundant shrubs and mosses growing in hummocks
instead of horizontally.


#NOW... nMDS

#Do the nMDS
spe_nmds <- metaMDS(spe, distance = "bray") #using bray-curtis for dissimilarities
#because it is an abundance data
spe_nmds

#Ordination plots
par(mfrow = c(2, 2))
stressplot(spe_nmds, main = "Shepard plot")
ordiplot(spe_nmds, type = "t", cex = 1.5, 
         main = paste0("nMDS stress = ", round(spe_nmds$stress, 2)))
gof = goodness(spe_nmds)
plot(spe_nmds, type = "t", main = "Goodness of fit")
points(spe_nmds, display = "sites", cex = gof * 200)

#We can also build ordination plots from scratch to suit specific needs:\
pl <- ordiplot(spe_nmds, type = "none", main = "nMDS mites abundances ")
points(pl, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl, "species", col = "blue4", cex = 0.9)
text(pl, "sites", col = "red4", cex = 0.9)

INTERPRETEATION:
From the nMDS plot we can see that all the beginning sites(1-30) are grouped
together away from the sites and that is because they are similarly predominantly
influenced by the same species, i.e TVEL, RARD, SSTR to mention a few. These
species could be in these sites because their morphology allows them to
withstand the dry conditions, or because they feed mostly on shrubs dead matter.
Similarly, the more moist sites (from 30s) are also clustered together on the
other side opposite the moist sites, because they are also predominantly 
influenced by the same mite species, different from the mite species influencing
the dry sites. 


# Dune meadows ------------------------------------------------------------
#We will use PCA and CA for this data

#Fetch env data for PCA
data("dune.env")
denv <- as_tibble(dune.env)
denv[1:5, ]
#Make all columns numerical
denv$Moisture <- as.numeric(denv$Moisture)
denv$Management <- as.numeric(denv$Management)
denv$Use <- as.numeric(denv$Use)
denv$Manure <- as.numeric(denv$Manure)
denv[1:5, ] #It worked

#Do PCA
env.pca <- rda(denv, scale = TRUE)
env.pca
summary(env.pca)

#Ordination plots
par(mfrow = c(1, 2))
biplot(env.pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(env.pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

#Create barplots using cleanplot function from textbook
source("C:/QE2021/Quantitative_Ecology-main/Num_Ecol_R_book_ed1/cleanplot.pca.R")
cleanplot.pca(env.pca, scaling = 1)
cleanplot.pca(env.pca, scaling = 2)

INTERPRETATION
There are different land-uses and management strategies in these meadows driving
the differences between these sites. Sites 19-20 are coupled together because 
they do not receive any manure so they will be similar. sites 3-4 receive the 
most manure so they are similar as well. Sites 9, 17-18 are used as hayfields so
they are similar. Sites 2, 6, 11 as well are similar because they are the least
moist and have the least thick A1 horizon.
So, different sites will have different vegetation.

#Now we use CA for species data
data("dune")
spp <- dune
spp[1:6, 1:6] #have a glance

#Do the CA
spe_ca <- cca(spp)
spe_ca
summary(spe_ca)

#Ordination plots
par(mfrow = c(1, 2))
plot(spe_ca, scaling = 1, main = "CA dune vegetation - biplot scaling 1")
plot(spe_ca, scaling = 2, main = "CA dune vegetation - biplot scaling 2")

INTERPRETATION
The species close to each other are most likely to coexist. Empenigr, Airaprae,
and Hyporadi are associated with each other and are likely to be found at site19
whereas, the species Callcusp and Comapald coexist at sites 14-15. Then we have
the group of sites grouped around the center also associated with each other.

#Let us look closely at some of the species that have a great impact on the
#community structure
#fit response surfaces using ordisurf():

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spp, tmp <- ordisurf(spe_ca ~ Callcusp, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Callcusp"))
abline(h = 0, v = 0, lty = 3)
with(spp, tmp <- ordisurf(spe_ca ~ Trifprat, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Trifprat"))
abline(h = 0, v = 0, lty = 3)
with(spp, tmp <- ordisurf(spe_ca ~ Empenigr, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Empenigr"))
abline(h = 0, v = 0, lty = 3)
with(spp, tmp <- ordisurf(spe_ca ~ Airaprae, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Airaprae"))
abline(h = 0, v = 0, lty = 3)

#Fit the env variables into the last graph

(spe_ca_env <- envfit(spe_ca, denv, scaling = 2)) 
plot(spe_ca_env, col = "blue")
plot(spe_ca_env, p.max = 0.05, col = "red")

INTERPRETATION:
These 4 species in the diagram are amongst the most influential species on the 
vegetation commununity and they are abundant at different sites, influenced by
the different environmental conditions. 
Callcusp is most abundant at the sites 14-15, these sites have high moisture
content, standard farming occurs there and this species is a pasture crop.
Trifprat is most abundant at sites 5,7,10, these sites are used as hayfields, 
they are less moist and receive high manure so they have high nutrients.
Empenigr and Airaprae coexist at site 19 but Airaprae extends to site 17 as well,
these sites have lower nutrients because very limited manure is aplied there.


