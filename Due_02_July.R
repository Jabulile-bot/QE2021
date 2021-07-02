#Quantitative Ecology
#Beta diversity
#Author: Esther(Jabulile)Leroko

library(ggplot2)
library(vegan)
library(tidyverse)
library(betapart)

#set working directory and fetch data
setwd("C:/QE2021/Seaweeds")
spp <- read.csv("seaweeds.csv")
spp <- dplyr::select(spp, -1)

# Lets look at the data:
dim(spp)
spp[1:5, 1:5]
spp[(nrow(spp) - 5):nrow(spp), (ncol(spp) - 5):ncol(spp)]

#Calculate true beta diversity = gamma/alpha
true_beta <- data.frame(
  beta = ncol(spp) / specnumber(spp, MARGIN = 1),
  section_no = c(1:58)
)

# plot the true_beta
ggplot(data = true_beta, (aes(x = section_no, y = beta))) +
  geom_line() + xlab("Coastal section, west to east") + 
  ylab("True beta-diversity") +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"))


#True beta-diversity is one way to do it, we can also calculate beta-diversity
#...using species turnover, i.e gamma - alpha

abs_beta <- data.frame(
  beta = ncol(spp) - specnumber(spp, MARGIN = 1),
  section_no = c(1:58)
)
# plot abs_beta
ggplot(data = abs_beta, (aes(x = section_no, y = beta))) +
  geom_line() + xlab("Coastal section, west to east") + 
  ylab("Absolute beta-diversity") +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"))



#Turnover and nestedness-resultant components

# Decompose total Sørensen dissimilarity into turnover and nestedness-resultant
#...componets:
Y.core <- betapart.core(spp)
Y.pair <- beta.pair(Y.core, index.family = "sor")

# Let Y1 be the turnover component (beta-sim):
Y1 <- as.matrix(Y.pair$beta.sim)

# Let Y2 be the nestedness-resultant component (beta-sne):
Y2 <- as.matrix(Y.pair$beta.sne)

round(Y1[1:10, 1:20], 4) #round(x, 4) rounds the values to 4 digits, we want the
#first 10 rows and first 2o columns
round(Y2[1:10, 1:20], 4)

# QUESTIONS DUE 02 June  --------------------------------------------------
1. 

Y1 <- as.data.frame(Y1)
ggplot(data = Y1, (aes(x = 1:58, y = Y1[,1]))) +
  geom_line() + labs(x = "Section number, west to east",
                     y = "Species turnover",
                     title = "Species turnover of the Kelp forests fom the west to the east coast") +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"))

DISCUSS:The plot shows an increasing trend from west to east, showing that we 
get more dissimilarities between sites as we move eastwards. This trend shows us
that there are processes in this region that cause species to be lost and/or 
gained as we move east, without causing any changes in alpha diversity across
sites. We already know there is heterogeneity in these sites, but the plot says
it is not only the temperature gradient that drives heterogeneity among these
sites but there are other ecological processes as well, it could be the
dispersal agents present in the different sites or predators etc. 



2. 
Nestedness-resultant component is the processes that cause species to be gained
or lost without replacement such that the community with the lowest alpha-diversity
is the subset of the richer community. This component becomes useful in explaining
cases like
the kelp community(the plot above) where there is no Temperature gradient in the 
west coast but we still see dissimilarities between sites. These dissimilarities
could be due to a lot of ecological factors. Kelp forests as ecosystems house
different organisms that interact with and affect each other. An example of
such processes would be when west rock lobsters(very important predators in Kelp
forests) moved from their original area to the South. Such a movement leaves
a distraction in the food web and the environment, and can cause an addition or 
subtraction of kelp species in the West and south coast.
Overfishing in one part of the coast(which is also predicted to be a
probable cause of the rock lobster invasion in the South) also can drive these 
random nestedness-resultant component processes. 