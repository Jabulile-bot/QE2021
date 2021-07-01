#Quantitative Ecology
#Topic 4: Environmental Distance
#Week 2
#Author: Esther (Jabulile) Leroko

library(ggplot2)
library(vegan)
library(geodist) #For calculating geographical distances
library(ggpubr) #For multipanel graphs

#Loading the data from github
urlfile1 <- ("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/diversity/Euclidian_distance_demo_data_xyz.csv")
xyz <- read.csv(urlfile1)

#Explore the data
dim(xyz) #That means it has 7 rows, and 4 columns
xyz


# CALCULATING EUCLIDIAN DISTANCES -----------------------------------------


xyz_euc <- round(vegdist(xyz[, 2:4], method = "euclidian", 
                         upper = FALSE, diag = TRUE), 4)
xyz_euc

EXPLAIN: the zeros along the diagonal are the distance between the site and
itself, so it is zero. The other values are the distance values between sites, 
and can be any value greater than 0

#Convert it to a dataframe that can be used
xyz_df <- as.data.frame(as.matrix(xyz_euc), 4)
xyz_df

#Let us imagine xyz were real environmental conditions
urlfile2 <- ("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/diversity/Euclidian_distance_demo_data_env.csv")
env_fict <- read.csv(urlfile2)
env_fict

So that means that the distance values in the matrix would be all these variables
combined into 1 value. The distance value represents
the differences between sites and not distances.The smaller the value, the more
similar the sites are in terms of the measured environmental variables. 


# NOW, SEAWEED ENVIRONMENTAL DATA -----------------------------------------


urlfile3 <- ("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/diversity/SeaweedsEnv.csv")
env <- read.csv(urlfile3)
env


# CALCULATE Z-SCORES ------------------------------------------------------

e1 <- round(decostand(env, method = "standardize"), 4)
e1[1:5, 1:5] #checking the first 5 rows and columns


# CALC THE EUCLIDEAN DISTANCE ---------------------------------------------

e1_euc <- round(vegdist(e1, method = "euclidian", upper = TRUE), 4)
e1_df <- as.data.frame(as.matrix(e1_euc))
e1_df


#Let's plot
ggplot(data = e1_df, (aes(x = 1:58, y = e1_df[,1]))) +
  geom_line() + xlab("Coastal section, west to east") + 
  ylab("Environmental distance") +
  theme_bw() +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"))

DISCUSS: the environmental distance increases from west to east, that means, the
differences in the environmental variables as you move away from site 1
towards site 58 increase. so we would expect the composition of species found in
the Western sites to be different from the composition in the Eastern sites.


#Try another dataset
