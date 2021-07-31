#Quantitative Ecology
#Intergrative Assignment: Doubs River data
#Author: Esther(Jabulile)Leroko

#Load required packages
library(tidyverse)
library(vegan)
library(cluster)
library(ggcorrplot)
library(factoextra)
library(ggpubr)

#Loading the data
setwd("C:/QE2021/Quantitative_Ecology-main/Num_Ecol_R_book_ed1")
env <- read.csv("C:/QE2021/Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsEnv.csv")
env <- dplyr::select(env, -1)#all columns must be env variables, column 1 is not
env <- dplyr::slice(env, -8)#remove the repetitive first column

#Do the PCA based on a correlation matrix
env.pca <- rda(env, scale = TRUE)
env.pca
summary(env.pca)
#Ordinatio diagrams
biplot(env.pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(env.pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

#Cleanplot
source("C:/QE2021/Quantitative_Ecology-main/Num_Ecol_R_book_ed1/cleanplot.pca.R")
cleanplot.pca(env.pca, scaling = 1)
cleanplot.pca(env.pca, scaling = 2)

# Correspondence analysis for species data --------------------------------
#Loading the data

spe <- read.csv("C:/QE2021/Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsSpe.csv")
spe <- dplyr::select(spe, -1)#remove the repetitive first column
spe <- dplyr::slice(spe, -8)#site8 has no species present so, remove

#Now, do CA
spe_ca <- cca(spe)
spe_ca
summary(spe_ca)

#Ordination plots
plot(spe_ca, scaling = 1, main = "CA fish abundances - biplot scaling 1")
plot(spe_ca, scaling = 2, main = "CA fish abundances - biplot scaling 2")

#Plot plots of 4 species to show their patterns
require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe, tmp <- ordisurf(spe_ca ~ Satr, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Brown trout (Satr)"))
abline(h = 0, v = 0, lty = 3)

with(spe, tmp <- ordisurf(spe_ca ~ Thth, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Grayling (Thth)"))
abline(h = 0, v = 0, lty = 3)

with(spe, tmp <- ordisurf(spe_ca ~ Baba, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Barbel (Baba)"))
abline(h = 0, v = 0, lty = 3)

with(spe, tmp <- ordisurf(spe_ca ~ Abbr, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Common bream (Abbr)"))
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

# Cluster analysis --------------------------------------------------------
# a correalation matrix
spe_std <- decostand(spe, method = "standardize")
spe_euc <- vegdist(spe_std, method = "euclidian")

# Ward Hierarchical Clustering
spe_ward <- hclust(spe_euc, method="ward.D")
plot(spe_ward) # display dendogram
groups <- cutree(spe_ward, k = 4) # cut tree into 4 clusters (k=4)
# draw dendogram with red borders around the 4 clusters
rect.hclust(spe_ward, k = 4, border = "red")

