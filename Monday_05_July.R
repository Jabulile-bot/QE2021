#Quantitative Ecology
#Submission: 05 July
#Author: Esther(Jabulile)Leroko

#Load required packages
library(ggplot2) 
library(vegan)
library(tidyverse)

#Read the file from github
url1 <- ("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsSpe.csv")
doubs <- read.csv(url1)

#Explore data
doubs[1:6, 1:6] #checking the 1st 6 rows and columns
#From the values >1 we know it's abundance data
doubs <- dplyr::select(doubs, -1) #Remove the column x so that columns are species
dim(doubs)

QUESTION 1: LOOK AT DATASET AND EXPLAIN ITS STRUCTURE IN WORDS
ANSWER: The dataset has sites in rows, and species in columns. So it has 30 sites,
and 27 species, abundance of the species in each site in the cells. 

QUESTION 2: WOULD WE USE BRAY-CURTIS OR JACCARD DISSIMILARITIES?
ANSWER: We would use Bray curtis because this is an abundance data, not a 
presence/absence data where we would use Jaccard

QUESTION 3: APPLY THE CALCULATION

class(doubs) #checking if it's a dataframe, it is

#So, we calculate the dissimilarities
doubs_bray <- vegdist(doubs, binary = FALSE) # binary = FALSE says it's not a pa data
bray_df <- round(as.matrix(doubs_bray), 4)
bray_df[1:10, 1:10] #Have a look at the first 10 rows and columns
#Now the last 10 rows and 10 columns:
bray_df[(nrow(bray_df) - 10):nrow(bray_df), (ncol(bray_df) - 10):ncol(bray_df)]

QUESTION 4: EXPLAIN THE MEANING OF THE RESULTS IN BROAD TERMS

The zeros that make a diagonal line are where the site is compared
to itself, so the dissimilarity is zero. The values greater than zero show that 
the sites being compared are different in fish spp composition, and the closer
to 1 the value is the more dissimilar they are. There are several 1.000 values,
these say that those sites do not share any species, they are 100% different. 

QUESTION 5: EXAMINE IT MORE CLOSELY: WHAT GENERAL PATTERN COMES OUT?

class(bray_df) #we cannot use a matrix, convert to dataframe
bray_df<- as.data.frame(bray_df)
bray_df[,1]


ANSWER: The fish species composition becomes more different as you move 
downstream
  
  
QUESTION 6: PLOT THIS PATTERN

ggplot(data = bray_df, (aes(x = 1:30, y = bray_df[,1]))) +
  geom_line() + xlab("River section, upstream to downstream") + 
  ylab("Environmental distance") +
  theme_bw() +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"))

QUESTION 7: WHAT EXPLANATION CAN YOU OFFER FOR THIS PATTERN?
  
ANSWER: There is higher composition of species downstream than upstream, caused
by the differences between sites as you move downstream. The few fish species
upstream are adapted to extreme conditions. Upstream sites receive very low
river discharge, so, less nutrients for plants to grow, therefore, less food for
these species. As you move downstream, more nutrients and food make it easy for 
more species to survive, hence more species, some of those species will not exist
in the first site. The sites from 19 to 28 show 100% dissimilarity from site 1
and this means the species in those sites are not found in site 1 at all. 

QUESTION 8: USING THE decostand() FUNCTION, CREATE PRESENCE/ABSENCE DATA, APPLY
THE APPROPRIATE vegdist() FUNCTION TO OBTAIN A SUITABLE DISSIMILARITY MATRIX

#Convert abundance data to p/a data
p_a <- decostand(doubs, method = "pa")
p_a[1:5, 1:5]
#Use vegdist to get dissimilarities
jac_doubs <- round(vegdist(p_a, method = "jaccard", binary = TRUE, 
                           diag = TRUE), 4)
class(jac_doubs) #check if it's the usable dataframe, it's not

#then...convert to dataframe
jac_df <- as.data.frame(as.matrix(jac_doubs))
jac_df[1:7, 1:7] #check first 7 rows and columns

QUESTION 9: CREATE ANOTHER PLOT AND EXPLAIN THE PATTERN

ggplot(data = jac_df, (aes(x = 1:30, y = jac_df[,1]))) +
  geom_line() + xlab("River section, upstream to downstream") + 
  ylab("Dissimilarity") +
  theme_bw() +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"))

EXPLAIN: The further you move from site 1, the more dissimilar the species 
composition compared to site 1 becomes. The first 5 sites are dominated by the 
primarily carnivorous Brown trout but as you move downstream you get more 
species dominating in abundances due to the more favourable ecological 
conditions. 

