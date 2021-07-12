#QUATITATIVE ECOLOGY
#Topic 7: Unconstrained Ordinations- PCA
#Author: Esther(Jabulile)leroko

#Load packages
library(vegan)
library(tidyverse)

#Doubs dataset
setwd("C:/QE2021/Quantitative_Ecology-main/Num_Ecol_R_book_ed1")
env <- as_tibble(read.csv("DoubsEnv.csv"))
env[1:4, ]

#Remove first column
env <- env[, -1]


#Do the PCA based on a correlation matrix
env.pca <- rda(env, scale = TRUE)
env.pca
#total inertia = number of env variables = 11
cor(env) #Shows us the correlation calues

#To extract the first eigenvalue we can do:
round(env.pca$CA$eig[1], 3)
#The total inertia is:
sum(env.pca$CA$eig)
#So the proportion of variation explained by the first PC is:
round(env.pca$CA$eig[1] / sum(env.pca$CA$eig) * 100, 1) # result in %

# . -----------------------------------------------------------------------


QUESTIONS (A)
Why can a PCA, or any ordination for that matter, not explain all of the variation
in a dataset? In other words, why is it best to only use the first few Principal 
Components for insight into the drivers of variability? What is 'explained' 
by the remaining PC axes?

-Ordinations are there to represent or show us the main trends in the data. The
1st PC shows us how much influence does 1 environmental variable have on the 
variation of the data, and that will be the most influential variable, followed
by the 2nd, 3rd... PCs. As you increase the PCs, the less influence they have.
And since the PCA does not give a statistical significance value, we cannot
tell if the last PC really has the least influence on the variability 
in the environment or if the differences in influence of the few least
influential PCs are significant or if they have any real effect at all

# . -----------------------------------------------------------------------
summary(env.pca)
#we have 11 PCs because we had 11 variables, each PC has an eigenvalue
#Proportion explained = eigenvalue/total inertia
#spp scores: These are rotated and scaled eigenvectors. They indicate the strength
#of the contribution of the original env variables to the new variables(PCs).
#Basically, how much do the original env variables contribute to PC1, PC2 etc.
#the very high -ves and +ves under each PC have the highest influence
#on each PC, values close to 0 have minimal influence
#These spp scores will be the arrows(called vectors) on the ordination plot, the
#longer the vector, the stronger the influence

#Site scores: are the scaled and rotated coordinates of the objects(i.e sites/
#samples). They are the dots on the ordination plots. Sites that are spread
#further apart are very different from each other


# GRAPHICAL REPRESENTATIONS OF ORDINATIONS --------------------------------

biplot(env.pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
#SCALING = 1: the distances between points on the plot will retain their
#euclidean distances, which allows better interpretation to how sites relate to
#one another
biplot(env.pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))
#Scaling = 2: preserves more accurately the angles between the variables, such 
#that smaller angles between variable vectors will reflect stronger correlations

#Now we create biplots using the cleanplot.pca() function that comes with 
#Numerical Ecology in R book:

# we need to load the function first from its R file:
source("C:/QE2021/Quantitative_Ecology-main/Num_Ecol_R_book_ed1/cleanplot.pca.R")
cleanplot.pca(env.pca, scaling = 1)
cleanplot.pca(env.pca, scaling = 2)

THE CIRCLE EXPLANATION:
-The circle on scaling 1 is called "a circle of equilibrium contribution". The 
radius of the circle represents the length of the vector representing a variable
that would contribute equally to all the dimensions of the PCA space.
-Therefore, for any given pair of axes, the variables that have vectors longer
than this radius make a higher contribution than average and can be interpreted
with confidence. 

PLOT INTERPRETATION
-The scaling 1 biplot shows a gradient from left to right, starting with a group
formed by sites 1–10 which display the highest values of altitude (alt) and slope
(slo), and the lowest values in river discharge (flo) and distance from the source
(dfs) and hardness (har). 
-The second group of sites (11–16), has the highest values in oxygen
content (oxy) and the lowest in nitrate concentration (nit). 
-A third group of very similar sites (17–22) shows intermediate values in almost 
all the measured variables; they are not spread out by the variables contributing 
to axes 1 and 2. 
-Phosphate (pho) and ammonium (amm) concentrations, as well as biological oxygen 
demand (bod) show their maximum values around sites 23–25; the values decrease 
afterwards.
-Overall, the progression from oligotrophic, oxygen-rich to eutrophic, 
oxygen-deprived water is clear.



The scaling 2 biplot shows that the variables are organized in groups. 
-altitude and slope are very highly, positively correlated,
and these two variables are very highly, negatively correlated with
another group comprising distance from the source, river discharge and calcium
concentration. 
-Oxygen content is positively correlated with slope (slo) and altitude,
but very negatively with phosphate and ammonium concentration and, of
course, with biological oxygen demand. 
The right part of the diagram shows the variables associated with the lower 
section of the river, i.e. the group discharge(flo) and hardness (har), highly 
correlated with the distance from the source, and
the group of variables linked to eutrophication, i.e. phosphate and ammonium 
concentration and biological oxygen demand. 
Positively correlated with these two groups is nitrate concentration (nit). 
-Nitrate and pH have nearly orthogonal arrows, indicating a correlation close to
0. pH displays a shorter arrow, showing its lesser importance for the ordination
of the sites in the ordination plane. A plot of axes 1 and 3 would emphasize its
contribution to axis 3.

#We can plot the underlying environmental gradients using the ordisurf() function
#in vegan. We plot the response surfaces for altitude and biological oxygen demand:
biplot(env.pca, type = c("text", "points"), col = c("black", "black"))
ordisurf(env.pca ~ bod, env, add = TRUE, col = "turquoise", knots = 1)
ordisurf(env.pca ~ alt, env, add = TRUE, col = "salmon", knots = 1)

# . -----------------------------------------------------------------------

QUESTIONS (B)


1.Replicate the analysis shown above on the environmental data
 A. Bird communities along elevation gradient in Yushan Mountain, Taiwan

#Fetch the file
ybirds.env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_env.txt', row.names = 1)
ybirds.env[1:7, 1:7]
summary(ybirds.env)
#Convert to tibble and remove coulmns 1 and 2 since they are not numeric
ybirds.env <- as_tibble(ybirds.env[,-c(1,2)])

#Do the PCA
env.pca <- rda(ybirds.env, scale = TRUE)
env.pca
summary(env.pca)

#Plots
biplot(env.pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(env.pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

#Now we create biplots using the cleanplot.pca() function that comes with 
#Numerical Ecology in R book:

# we need to load the function first from its R file:
source("C:/QE2021/Quantitative_Ecology-main/Num_Ecol_R_book_ed1/cleanplot.pca.R")
cleanplot.pca(env.pca, scaling = 1)
cleanplot.pca(env.pca, scaling = 2)

2.Discuss the patterns observed:
A. explain the ordination diagram with particular reference to the major 
patterns shown;
   
   -This biplot shows an elevation gradient from left to right, with the group of
   sites 1-15 being at the bottom of the mountain, very cold environment, and
   exposed to the valley. These sites are an evergreen forest with high tree 
   diversity(TD) and tree species diversity(TSD), high secondary tree cover
   (T2C), and high herb cover(HC).
   And then there is the group of sites 17-21 and 24-29 with highest tree DBH(SDDB)
   and basal area(TBA), high foliage height diversity(FHD), high canopy cover(T1C) and 
   height(CH), and high total foliage volume(TFV), but with the lowest
   shrub cover(SC) and ground cover(GC).
   Then we have the group of sites 34-44 at high elevation(ELE), exposed to
   upper and steep slopes(EXP and SLP), with highest conifer percentage(CP).

B. provide a mechanistic explanation for the existence of the patterns seen 
with respect to elevation/altitude; and
   
   -At low elevation (bottom of the mountain), the soil is moist all year because
   of the valley water, it is cold, and these conditions allow for an evergreen 
   forest, hence the high tree diversity, secondary tree and herb cover.
   But as you move up the mountain, in the middle, you get a more of a deciduos 
   forest, with intermediate environmental conditions, there is more competition
   for light, water etc, this is the transition area. There is high canopy cover 
   and height, trees have huge stems, and they have a lot of leaves, and this is
   because they are competing for resources and only the taller and bigger
   can survive.
   As you move to the top of the mountain, the environment gets very dry, hence
   the domination of conifers and very low tree diversity and herb cover.
   
c. if there are significant positive or negative correlations between the 
environmental variables, provide mechanistic reasons for how they came about.

cor(ybirds.env)   
 -There is high +ve correlation between the standard deviation of tree DBH(SDDB) 
  and tree basal area(TBA). TBA is the cross-sectional area of a tree at breast
  height, and to find it you need to calculate the stem diameter at breast
  height(DBH), therefore, DBH and TBA are directly proportional.These two are
  highly negatively correlated with shrub cover because shrubs are very short 
  and at breast height is just leaves and not stems.
 -Elevation(ELE) and conifer%(CP) are highly positively correlated because it is
  very cold and dry up the mountain and those are areas where conifers strive. 
  These two are highly negatively correlated to tree diversity(TSD and TD),
  and herb cover(HC) because these 2 groups are in direct opposite ends of the
  environmental conditions.


# Bird communities ends here ----------------------------------------------


# Alpine communities starts here ------------------------------------------
1.Replicate the analysis shown above on the environmental data

B. Alpine plant communities in Aravo, France
   
#Fetch the file
install.packages("ade4")
library(ade4)
env <- data(aravo)
env <- as_tibble(aravo[["env"]]) 

#the columns form and ZoogD are factors, since the columns are not that many,
#I will change these to numeric
env$Form <- as.numeric(as.character(env$Form))
env$ZoogD <- as.numeric(env$ZoogD)
summary(env)
#Do PCA
env.pca <- rda(env, scale = TRUE)
env.pca
summary(env.pca)

#Plots
biplot(env.pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(env.pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

#Now we create biplots using the cleanplot.pca() function that comes with 
#Numerical Ecology in R book:

# we need to load the function first from its R file:
source("C:/QE2021/Quantitative_Ecology-main/Num_Ecol_R_book_ed1/cleanplot.pca.R")
cleanplot.pca(env.pca, scaling = 1)
cleanplot.pca(env.pca, scaling = 2)


2.Discuss the patterns observed:
  A. explain the ordination diagram with particular reference to the major 
     patterns shown;
  
  -The mesotopography of the area causes a strong gradient of decreasing
   snow-cover from left to right, with the group of sites 50-63, which are
   at the lower slopes having the most snow-melting days (least snow cover).
   The group of sites on the top left panel, which are on middle slopes have
   the highest physical disturbance, in convex form, early snow-melting days.
   The sites with steeper slopes are highly disturbed by marmots, and have less
   snow-melting days so they are always covered by snow.
   
   
B. provide a mechanistic explanation for the existence of the patterns seen 
     with respect to elevation/altitude; and

  -At low elevation, there is high snow-melting because of high temperatures
   compared to the sites at the top of the mountain where it is colder.
   The middle slope sites receive intermediate snow-melting days as their 
   temperatures are intermediate, but experience high physical disturbance
   because the area is concave that allows solifluction especially.
   Then at the upper slopes there is a lot of snow cover, more zoological
   disturbance by marmots which prefer these cold conditions.
   
   
c. if there are significant positive or negative correlations between the 
environmental variables, provide mechanistic reasons for how they came about.

     
cor(env)
  -There is high negative correlation between physical disturbance and form,
   and that is because the sites there are very convex, middle sloped, making
   these areas easily erodible.
  -The high negative correlation between snow-melting days and slope is because
   the steeper sites are higher up the mountain where it is cold and there is 
   less melting, so the more steeper, the less melting.
  -The positive correlation between zoological disturbance and slope+aspect is
   because these sites are at the top of the mountain, it is cold there and the
   marmots love it there more than the other warm sites which are at the bottom
   so, the steeper, the more zoological disturbance.
   