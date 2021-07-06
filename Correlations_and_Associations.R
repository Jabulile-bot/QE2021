#Quantitative Ecology
#Topic 6: Correlations and Associations
#Author:Esther(Jabulile)Leroko

#Load packages that we will need
library(tidyverse)
library(vegan)
library(Hmisc) # for rcorr()
library(psych)

# CORRELATIONS BETWEEN ENVIRONMENTAL VARIABLES  ---------------------------
setwd("C:/QE2021/Quantitative_Ecology-main/Num_Ecol_R_book_ed1")
env <- read.csv("DoubsEnv.csv")
head(env, 1)

# drop the first column
env <- dplyr::select(env, -1)
head(env)

#Correlations tell us how environmental variables relate to each other across the
#...sample sites
#No need for standardization here, but some transformation might be needed in 
#...some instances

round(cor(env), 2) #Told R to calc the correlations and round them to 2 decimal
#places
#Cor values will be between -1 and 1. -1 = strong negative correlation,
#0 = no correlation, +1 = strong positive correlation

#Or if we want to see the associated p-values to establish a statistical 
#significance:
rcorr(as.matrix(env))

QUESTION (A)
1. Create a plot of pairwise correlations.

#Convert to dataframe first
env_cor <- as.data.frame(as.matrix(round(cor(env), 2)))
env_cor[1:4, 1:4]

#Plot pairwise correlations
pairs.panels(env_cor,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "pearson", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals


2.Name two top positive and two top negative statistically-significant 
correlations.

- top positive = phosphate concentration(Pho) and ammonium concentration(amm), 
distance from source(dfs) and mean minimum discharge(flo)
- top negative = distance from source(dfs) and altitude(alt), 
altitude(alt) and mean minimum discharge(flo)

3. For each, discuss the mechanism behind the relationships. 
Why do these relationships exist?

a. Pho and amm: Phosphate and Ammonium accumulate in rivers due to sewage,
agricultural runoff, industries, etc. These nutrients come from the same
sources, hence the 100% correlation between them.

b. dfs and flo: The further away from the mouth of the source, the faster the
river discharge. Water flow tends to be slow at the mouth of the source due to
turbulence causing water to want to move in opposite directions. But far from
the source, there are less distractions in the flow and direction of water, 
hence the fast flow

c. dfs and alt: The higly negative correlation between altitude and distance 
from the source is because these do not affect each other at all. The source
could be at any altitude, there will not be any effect on the distance and 
vice versa.

d. alt and flo: The highly negative correlation between altitude and mean
minimum flow shows that these two have no relation to each other at all. The
water body could be at any latitude, and the speed of the flow will be affected
by other things but altitude. 
  

# ASSOCIATION BETWEEN SPECIES ---------------------------------------------

spp <- read.csv("DoubsSpe.csv")
spp <- dplyr::select(spp, -1)
head(spp)

#In order to calculate an association matrix for the fish species we first need to 
#...transpose the data: 
spp_t <- t(spp)
spp_t

QUESTIONS (B)
1. Why do we need to transpose the data?
-We transpose the data so that our species are in rows, and that is because we want
to compare species associations and not the sites

2. What are the properties of a transposed species table? 
-The species will be in rows, sites in columns, abundance in the cells

#after transposing, we can calculate the association matrix:
spp_assoc1 <- vegdist(spp_t, method = "jaccard")
as.matrix((spp_assoc1))[1:5, 1:5] # display only a portion of the data

spp_assoc2 <- vegdist(spp_t, method = "jaccard", binary = TRUE)
as.matrix((spp_assoc2))[1:5, 1:5] # display only a portion of the data

QUESTIONS (C)
1.What are the properties of an association matrix? How do these properties 
differ from that of a:
  i) species dissmilarity matriX and
 ii) correlation matrix?
  
-The association matrix shows the species on both rows and columns, in the cells
are the dissimilarity values between the species, these range from 0 to 1. zero
for 100% similarity and 1 for 100% dissimilarity between the species compared.
-The species dissimilarity matrix compares the species composition between
sites
-The correlation matrix compares between envirnmental variables. On the cells
are correlation values that range from -1(Negative correlation) to 
0(no correlaton) to +1(positive correlation)
  
2.What is the difference between spp_assoc1 and spp_assoc2? 
  Is the information contained in each markedly different from the other?
-spp_assoc1 calculates the dissimilarities on a non-binary data, by not putting
binary = TRUE, we are saying it is an abundance data, so dissimilarities will
take into account the abundance numbers
-spp_assoc2 specifies binaty = TRUE which means presence/absence, vegdist
will not take into account the abundance of the species, but is it present or
not
-The information on both of these is not markedly different, just that the
binary ones will be a bit lower but it will still be the same distribution if
plotted.

 
3.Explain the kind of insight we are able to glean from a species association 
matrix.
-We are able to see what species are able to occupy the same niche more than
others, which tells us something about that particular environment where these
species exist.