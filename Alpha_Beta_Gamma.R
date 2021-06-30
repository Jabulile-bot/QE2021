#Quantitative ecology
#Week:2
#Author: Esther (Jabulile) Leroko

library(tidyverse)
library(betapart)
library(vegan)
library(gridExtra)
library(BiodiversityR)
library(grid)
library(gridBase)
library(tidyr)

#Load data and see its structure
setwd("C:/QE2021/Seaweeds")
spp <- read.csv("seaweeds.csv")
spp <- dplyr::select(spp, -1)

# Lets look at the data:
dim(spp)

#We see that our dataset has 58 rows and 847 columns. What is in the columns 
#...and rows? Start with the first 5 rows and 5 columns:
spp[1:5, 1:5]

#Now the last 5 rows and 5 columns:
spp[(nrow(spp) - 5):nrow(spp), (ncol(spp) - 5):ncol(spp)]
#Frm this we see that each row consists of sites, from site1 to 58
#And each column consists of the spp, arranged alphabetically, indicated by
# a six letter code, 847 spp


# NOW... 1. ALPHA DIVERSITY -----------------------------------------------

#Let's count no. of spp using diversityresult() found in the 
#... BiodiversityR package

spp_richness <- diversityresult(spp, index = 'richness', method = 'each site')
# spp_richness
ggplot(data = spp_richness, (aes(x = 1:58, y = richness))) +
  geom_line() + xlab("Coastal section, west to east") + ylab("Species richness") +
  theme_bw() +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjUst = 0))

#Can also use specnumber() in vegan 
# Use 'MARGIN = 1' to calculate the number of species within each row (site)
specnumber(spp, MARGIN = 1) #margin = 1 says for each site, if it was =2,...
#the calculation would be for all sites down each column

#That was alpha diversity using spp richness
# NOW... uNIVARIATE DIVERSITY INDICES
#can be calculated using shannon (relative abundance) or simpson index (evenness)

#We will use a different data set for this, since the seaweed one is not...
#... abundance data but a presence/absence data

urlfile1 <- ("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/diversity/light_levels.csv")
light <- read.csv(urlfile1)
light

#Calculate spp richness, Shannon, and Simpson's indices, put these in a data frame
light_div <- data.frame(
  site = c("low_light", "mid_light", "high_light"),
  richness = specnumber(light[, 2:7], MARGIN = 1),
  shannon = round(diversity(light[, 2:7], MARGIN = 1, index = "shannon"), 2),
  simpson = round(diversity(light[, 2:7], MARGIN = 1, index = "simpson"), 2)
)
light_div
#richness equal everywhere because all spp were present in all the sites
#mid-light site has the highest diversity

#Back to Seaweed data

#We use Sorenson index only for presence-absence data to calc dissimilarity
sor <- vegdist(spp, binary = TRUE) # binary = TRUE sets to presence/absence data
sor_df <- round(as.matrix(sor), 4)
sor_df[1:20, 1:20] # the first 20 rows and columns


# ANSWERS TO QUESTIONS ---------------------------------------------------------------

1. The matrix compares the dissimilarity between each site and every other sites.
   Therefore, if there are 5 sites, there will be a 5x5 matrix, making it a square.
   So, the number of sites determines the number of rows and columns since the
   comparison is between sites.

2. The diagnonal shows where each site is compared to itself so, there is 100%
   similarity. Hence, there is 0.0000 in the diagonal line

3. The closer to 1 the non-diagonals, the more dissimilar the sites
   being compared.And the closer they are to 0, the more similar the sites
   being compared.
   
4. 
first_row <- data.frame(sor_df[1,])
ggplot(data = first_row, (aes(x = 1:58, y = sor_df[1,]))) +
  geom_line(col = "seagreen") + labs(x = "Coastal section, west to east",
                                     y = "Dissimilarity (Sorensen index)",
                                     title = "How dissimilar in diversity the other sites are from Site 1") +
  theme_bw() +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"))

5. The further you go from site 1, the greater the differences in environmental
   conditions (specifically, temperature), therefore, the higher the 
   dissimilarity.
   Site 1 is Port Nolloth, which experiences only the Benguela Current, so it has
   very cold waters. The temperatures increase as you move from west to East, so
   does the dissimilarity of those sites from site 1. 
   The first peak on the plot is False bay, which is warmer, therefore, has more 
   seaweed species compared to site 1, then temperatures and dissimilarity
   decrease again towards pringlebay. From there temperatures increase because
   these areas receive the agulhas current water,so, the temperatures are 
   different from site 1, so the diversity here is different as well. 
  
   

# END OF ALPHA DIVERSITY --------------------------------------------------


# BEGINNING OF GAMMA DIVERSITY --------------------------------------------

# the number of columns gives the total number of species in this example:
ncol(spp)
#We can also use diversityresult()
diversityresult(spp, index = "richness", method = "pooled")

Question: Why are the 2 above giving us different values? Which one is correct?

Answer: In this example, all columns have uniques species in them, so, the number
of columns is indeed the number of species in the community, therefore, is the 
correct answer for gamma diversity 
diversityresult() suggests that the first column is the sites, so it 
minuses it


# END OF GAMMA DIVERSITY --------------------------------------------------


# BEGINNING OF BETA DIVERSITY ---------------------------------------------
#Calculate true beta diversity = gamma/alpha
true_beta <- data.frame(
   beta = specnumber(spp, MARGIN = 1) / ncol(spp),
   section_no = c(1:58)
)


# plot the true_beta
ggplot(data = true_beta, (aes(x = section_no, y = beta))) +
   geom_line() + xlab("Coastal section, west to east") + 
   ylab("True beta-diversity") +
   theme(axis.title   = element_text(face = "bold"),
         axis.text    = element_text(face = "bold"))

DISCUSS: from the plot, true beta-diversity starts from very low values because 
the number of seaweed spp found are not that many as temperatures do not allow
But as you move east, the temperatures steadily increase and so does the number
of species, so the number of beta-diversity increases. Comparing the first 15
sites to the last 20 sites, there is high heterogeneity between these grouped
sites and that is because of the temperature gradient.

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

DISCUSS: spp turnover tells us how much spp there are at regional compared to
local scale (remember: abs_beta = gamma - alpha)so, the greater the difference
between gamma and alpha, the higher the spp turnover or the more spp there are
at regional than local, hence this plot shows a 
decreasing trend... the difference in the number of spp in particular sites 
compared to the 58 sites combined decreases as you move towards east, i.e
spp turnover/ absolute beta diversity decreases towards east, with increasing
temperatures. In other words, there is higher heterogeneity between the 
west and the east in terms of seaweeds.

