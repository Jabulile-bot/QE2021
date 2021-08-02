# load libraries
library(tidyverse)
library(vegan)
library(missMDA) #for missing values
library(ggcorrplot) #for correlation
library(ggrepel)
library(cluster)
library(factoextra)
library(ggpubr)

# load data
# Domestic general government health expenditure as a percentage of general # government expenditure
SDG1.a <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG1.a_domestic_health_expenditure.csv") %>%
  filter(Period == 2016) %>% 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG1.a") 

# Maternal mortality ratio (per 100 000 live births)
SDG3.1_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.1_maternal_mort.csv") %>%
  filter(Period == 2016,
         Indicator == "Maternal mortality ratio (per 100 000 live births)") %>% # leave only this indicators in the rows
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>% #select only these required columns
  mutate(SDG = "SDG3.1_1") #create a new columns called SDG with the observations being SDG 3.1_1

# SDG 3.1 Births attended by skilled health personnel (%))
SDG3.1_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.1_skilled_births.csv") %>%
  filter(Period == 2016) %>% 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.1_2")

# SDG 3.2 Number of neonatal deaths (Child mortality)
SDG3.2_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.2_neonatal_deaths.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_1")

# SDG 3.2 Number of under-five deaths (Child mortality)
SDG3.2_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.2_under_5_deaths.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_2")

# SDG 3.2 Number of infant deaths (Child mortality)
SDG3.2_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.2_infant_deaths.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_3")

# SDG 3.3 New HIV infections (per 1000 uninfected population))
SDG3.3_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_new_HIV_infections.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_1")

# SDG 3.3 Incidence of tuberculosis (per 100 000 population per year))
SDG3.3_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_TB.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_2")

# SDG 3.3 Malaria incidence (per 1 000 population at risk))
SDG3.3_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_malaria.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_3")

# SDG 3.3 Hepatitis B surface antigen (HBsAg) prevalence among children under 5 years-prevalence-among-children-under-5-years)
SDG3.3_4 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_hepatitis_B.csv") %>%
  filter(Period == 2015) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_4")

# SDG 3.3 Reported number of people requiring interventions against NTDs
SDG3.3_5 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_NCD_interventions.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_5")

# SDG 3.4 Adult mortality rate (probability of dying between 15 and 60 years per 1000 population))
SDG3.4_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_adult_death_prob.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_1")

# SDG 3.4 Number of deaths attributed to non-communicable diseases, by type of disease and sex
SDG3.4_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes",
         Dim2 == "Diabetes mellitus") %>%
  mutate(Indicator = Dim2) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_2")

SDG3.4_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes",
         Dim2 == "Cardiovascular diseases") %>%
  mutate(Indicator = Dim2) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_3")

SDG3.4_4 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes",
         Dim2 == "Respiratory diseases") %>%
  mutate(Indicator = Dim2) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_4")

# SDG 3.4 Crude suicide rates (per 100 000 population) (SDG 3.4.2))
SDG3.4_5 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_suicides.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_5")

# SDG3.4 Total NCD Deaths (in thousands)
SDG3.4_6 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_NCD_data_total.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_6")

# SDG 3.5 Alcohol, total per capita (15+) consumption (in litres of pure alcohol) (SDG Indicator 3.5.2)-alcohol-per-capita-(15-)-consumption)
SDG3.5 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.5_alcohol_consumption.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.5")

#SDG 3.6 Estimated road traffic death rate (per 100 000 population))
SDG3.6 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.6_traffic_deaths_prop.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.6")

#SDG 3.7 Adolescent birth rate (per 1000 women aged 15-19 years))
SDG3.7 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.7_adolescent_births.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.7")

# SDG 3.8 UHC Index of service coverage (SCI)
SDG3.8_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.8_UHC_data_availability.csv") %>%
  filter(Period == "2013-2017") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.8_1")

# SDG 3.8 Data availability for UHC index of essential service coverage (%))
SDG3.8_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.8_UHC_data_availability.csv") %>%
  filter(Period == 2017) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.8_2")

#SDG 3.9 Poison control and unintentional poisoning
SDG3.9_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.9_unintentional_poisoning_prop.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.9_1")

# SDG 3.9 Mortality rate attributed to exposure to unsafe WASH services (per 100 000 population) (SDG 3.9.2)-(sdg-3-9-2))
SDG3.9_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.9_WASH_mortalities.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.9_3")

# SDG 16.1 Estimates of rate of homicides (per 100 000 population)
SDG16.1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG16.1_homicides.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG16.1")

# SDG 3.a Prevalence of current tobacco use among persons aged 15 years and older (age-standardized rate)
SDG3.a <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.a_tobacco_control.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.a")

# SDG 3.b Total net official development assistance to medical research and basic health sectors per capita (US$), by recipient country
SDG3.b_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_dev_assistence_for_med_research.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_1")

# SDG 3.b Measles-containing-vaccine second-dose (MCV2) immunization coverage by the nationally recommended age (%)-immunization-coverage-by-the-nationally-recommended-age-(-))
SDG3.b_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_measles_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_2")

#SDG 3.b Diphtheria tetanus toxoid and pertussis (DTP3) immunization coverage among 1-year-olds (%)-immunization-coverage-among-1-year-olds-(-))
SDG3.b_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_diphtheria_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_3")

#SDG 3.b Pneumococcal conjugate vaccines (PCV3) immunization coverage among 1-year-olds (%)-immunization-coverage-among-1-year-olds-(-))
SDG3.b_4 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_pneumococcal_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_4")

# SDG 3.b Girls aged 15 years old that received the recommended doses of HPV vaccine
SDG3.b_5 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_HPV_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_5")

# SDG 3.c SDG Target 3.c | Health workforce: Substantially increase health financing and the recruitment, development, training and retention of the health workforce in developing countries, especially in least developed countries and small island developing States
SDG3.c_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Medical doctors (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_1")

SDG3.c_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Nursing and midwifery personnel (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_2")

SDG3.c_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Dentists (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_3")

SDG3.c_4 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Pharmacists  (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_4")

# SDG 3.d Average of 13 International Health Regulations core capacity scores, SPAR version
SDG3.d_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.d_health_risks.csv")  %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.d_1")

# Other Life expectancy at birth (years))
other_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_Other_life_expectancy.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes",
         Indicator == "Life expectancy at birth (years)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "other_1")

#Other Life expectancy at age 60 (years))
other_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_Other_life_expectancy.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes",
         Indicator == "Life expectancy at age 60 (years)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "other_2")
#The above codes were reading the different csv files, the focus is on the year 2015 data, hence we filtered the period 2015. The select() function chooses the columns we want in our new data frame. And using mutate() we add another column called SDG and in the rows, we put whatever number that SDG IS

# Now we combine all the dataframes created above
health <- do.call("rbind", lapply(ls(),get)) #rbind combines the dataframes by the rows. lapply() makes sure the new object keeps the length of the original. ls() lists all the names that were in the original
head(health) #Have a glance at the data

# Create a list of all the SDGs used using unique()
unique(health[, c(5, 1)]) # the list must show columns 5 and 1 from the health dataframe

# Make the health dataframe wider
health_wide <- health %>%
  arrange(Location) %>% #arrange all the rows of other columns by the location column, from location starting with a... to z
  select(-Indicator) %>% #remove the column indicator
  pivot_wider(names_from = SDG, values_from = FactValueNumeric) %>% #have new columns names_from with its rows taken from the SDG column; and values_from with rows having values from the factValueNumeric column
  as_tibble() #save this in a tibble form

# Add world population data by reading the csv from github
popl <- read_csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_population.csv") %>%
  filter(Year == 2016) %>% #only the year 2016 data 
  rename(popl_size = `Population (in thousands) total`,
         Location = Country) %>% #rename the 2 columns into popl_size and Location
  select(Location, popl_size) %>% #Select only these columns
  mutate(popl_size = as.numeric(gsub("[[:space:]]", "", popl_size)) * 1000) #multiply the popl_size by 1000s and make them numeric

# Now join the population and health data using left_join
health_wide <- health_wide %>%
  left_join(popl)


# Express some variables to unit of population size using mutate
health_wide <- health_wide %>%
  mutate(SDG3.4_4 = SDG3.4_4 / popl_size * 100000,
         SDG3.4_3 = SDG3.4_3 / popl_size * 100000,
         SDG3.4_2 = SDG3.4_2 / popl_size * 100000,
         SDG3.4_6 = SDG3.4_6 / 100,
         SDG3.2_2 = SDG3.2_2 / popl_size * 100000,
         SDG3.2_3 = SDG3.2_3 / popl_size * 100000,
         SDG3.2_1 = SDG3.2_1 / popl_size * 100000)

# calculate histograms
health_wide$na_count <- apply(health_wide[, 3:(ncol(health_wide) - 1)], 1, function(x) sum(is.na(x)))
hist(health_wide$na_count, breaks = 14, plot = TRUE)


# remove rows where there are more than 10 NAs
health_wide <- health_wide %>%
  filter(na_count <= 10) %>%
  select(-na_count)


# calculate pairwise correlations
corr <- round(cor(health_wide[, 3:(ncol(health_wide) - 1)]), 1)


# visualization of the correlation matrix
ggcorrplot(corr, type = 'upper', outline.col = "grey60",
           colors = c("#1679a1", "white", "#f8766d"),
           lab = TRUE) #using the correlation values to plot, we want the upper part only instead of it being a square matrix,

INTERPRETATION:
This data shows very high positive correlation between Child mortalities 
(neonatal, infants, under-5yrs), martenal mortalities and mortalities due to
unsafe WASH services. Pregnant women who live in very poor countries are exposed
very poor and unsafe water, sanitation and hygiene conditions; whatever the 
pregnant woman drinks or eats gets to the child and most of these children and
the woman do not survive.
And then we see highly negative correlation between life expectancy and martenal
mortality, child mortality, adult mortality rate, to mention a few... this is
because where life expectancy is high, all the mentioned variables/factors will
be very low due to the countries economic status. Wealthy countries have safe
WASH services, they eat healthy food, have the best medical services so,
their populations live longer, there is less child, martenal and adult
mortalities. 

# Impute remaining NAs
health_wide_complete <- imputePCA(health_wide[, 3:(ncol(health_wide) - 1)])$completeObs


# Do the PCA, start by standardizing the data so that all variables are on same scale. then calculate the PCA
health_wide_complete_std <- decostand(health_wide_complete, method = "standardize")
health_pca <- rda(health_wide_complete_std)
health_pca

# More informative output
summary(health_pca)

# Graphical representations
biplot(health_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(health_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

# Make the plots look better
par(mfrow = c(1, 2))
pl1 <- ordiplot(health_pca, type = "none", scaling = 1, main = "PCA WHO/SDG")
points(pl1, "sites", pch = 21, cex = 1.0, col = "grey20", bg = "grey80")
points(pl1, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl1, "species", col = "blue4", cex = 0.9)


pl2 <- ordiplot(health_pca, type = "none", scaling = 2, main = "PCA WHO/SDG")
points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl2, "species", col = "blue4", cex = 0.9)
text(pl2, "sites", col = "red4", cex = 0.9)

INTERPRETATION:
Here we have the group of countries that are wealthy /with high GDP per capita on
the right and the poor countries or low GDP on the left. We have the group of 
countries that are on the bottom right; these are some of the richest countries 
with high life expectancy, both at birth and at 60 years; they have very strong 
healthcare facilities and high medical research assistance; most European 
countries would fall here. These countries are also the countries with the 
fewest new HIV infections and TB incidences, very few unintentional poisoning 
and low adult mortality rates, which would be high for poor but developing countries like those
in the top left which are the direct opposite. And then we have the countries on
our top right, rich countries as well, with highest births that are attended by 
skilled healthcare personnel, high number of cardiovascular and respiratory 
diseases and high prevalence of tobacco use. USA would be a great example, 
a country with easily accessible resources to most, so urbanised that people 
get sick because of their dietary lifestyle, not because of poor healthcare
infrastructure or lack of healthcare personnel. 
And then we get the countries on our bottom left with highest child, martenal 
and adult mortality rates, malaria and hepatitis B incidences. I would expect A 
lot of African and some Asian countries to be be in this group. What is different
between the bottom left and the top left countries is that the top countries are
more urban, they have a good chance of jumping onto the top right side. The top
left countries have fairly good healthcare infrastructures and WASH services 
compared to the bottom left, this has prevented a lot of child and martenal 
mortalities, the struggle is now on educating the people from a young age about
caring about what they feed their body, diseases out there and increasing the
job opportunities for young people to prevent them from crime, suicide, and unsafe sexual
activities.


Most countries are clustered together, with a few outliers. Site92 and site32 
are examples of those outliers. Sites 54 and 86 are similar to each other, 
same as sites 105, 132, 147.
  
Neonatal and infants mortality are the leading variables that cause variation
between perfomance of various countries. More babies dying in a country tells
you just how much inaccessible things like healthcare, sanitation and food are 
to the mothers and the entire population. The richer the country the more
accessible these are to the public, the healthier the population and the less
child mortality rates. This is why we find high child mortality countries 
directly opposite those with highest births attended by health personnel.
Most mothers in extremely poor countries live far from clinics and hospital
that babies are born at home in not so clean environments, that even the
smallest implications a baby is born with cost their life or that of the mother.
Suicide rates and assistance for medical research as well are amongst the 
leading drivers of the variation in the perfomance of the countries, where the 
countries with the least assistance for medical research having the highest 
suicide rate per 100 000 population. 



# Other ways to create plots using ggplot
site_scores <- tibble(ParentLocation = health_wide$ParentLocation,
                      Location = health_wide$Location)
site_scores <- tibble(cbind(site_scores, scores(health_pca, display = "sites", choices = c(1:7))))
species_scores <- data.frame(scores(health_pca, display = "species", choices = c(1:7)))
species_scores$species <- rownames(species_scores)
species_scores <- tibble(species_scores)


ggplot(data = site_scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col = ParentLocation)) +
  geom_segment(data = species_scores, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"), 
               color = "lightseagreen", alpha = 1, size = 0.3) +
  geom_text(data = species_scores, 
            aes(x = PC1, y = PC2, label = species),
            color = "black") +
  xlab("PC1") + ylab("PC2") + 
  ggtitle("WHO SDGs, Scaling 2")

INTERPRETATION:
Most african countries are on the left, where the poorest countries are;
Most of them have high rates of child, maternal and adult mortalities due to the
poor healthcare facilities, few healthcare practitioners, very poor water and
sanitation.
Western pacific countries are not clustred together, they are a very diverse
group in terms of GDP, race, and many other factors. The Eastern
Mediterranean group is different from the Western pacific group in that the
countries are either very poor(mostly those in the African part) or 
very rich (those in the european and asian part).
European countries are obviously going to cluster on the healthier side of the
world because of their high GDP per capita and great healthcare system.



# South Africa vs. other countries ----------------------------------------

site_scores <- tibble(ParentLocation = health_wide$ParentLocation,
                      Location = health_wide$Location)
site_scores <- tibble(cbind(site_scores, scores(health_pca, display = "sites", choices = c(1:7))))
species_scores <- data.frame(scores(health_pca, display = "species", choices = c(1:7)))
species_scores$species <- rownames(species_scores)
species_scores <- tibble(species_scores)


ggplot(data = site_scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col = ParentLocation)) +
  geom_segment(data = species_scores, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"), 
               color = "lightseagreen", alpha = 1, size = 0.3) +
  geom_text(data = site_scores, 
            aes(x = PC1, y = PC2, label = Location),
            color = "black") +
  theme(legend.position = "none") +
  xlab("PC1") + ylab("PC2") + 
  ggtitle("WHO SDGs, Scaling 2")

# . -----------------------------------------------------------------------
site_scores <- tibble(ParentLocation = health_wide$ParentLocation,
                      Location = health_wide$Location)
site_scores <- site_scores %>% 
  mutate(label = if_else(Location %in% c("South Africa", "Pakistan", "Iceland", "Nigeria",
                                         "Finland", "Egypt", "Rwanda", "Japan", "Israel"), "yes", "no"))
site_scores <- tibble(cbind(site_scores, scores(health_pca, display = "sites", choices = c(1:7))))
species_scores <- data.frame(scores(health_pca, display = "species", choices = c(1:7)))
species_scores$species <- rownames(species_scores)
species_scores <- tibble(species_scores)


ggplot(data = site_scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col = ParentLocation)) +
  geom_segment(data = species_scores, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"), 
               color = "lightseagreen", alpha = 1, size = 0.3) +
  geom_text_repel(aes(label = if_else(label == "yes",Location,"")), col = "black", size = 3) +
  theme(legend.position = "none") +
  xlab("PC1") + ylab("PC2") + 
  ggtitle("WHO SDGs, Scaling 2")



# . -----------------------------------------------------------------------


INTERPRETATION:
South Africa, compared to the whole world and Africa, is not a poor country. This
makes it easy for the country to achieve this SGD goal. But because it is one of the most
urbanised countries in Africa, it has high mortalities of non-communicable
diseases like cardiovascular and respiratory diseases due to unhealthy eating, smoking 
and polluted air. More new HIV and TB infections than other African countries and the world.
it has very low child and martenal mortalities, low mortalities caused by WASH 
services; this is because the population has better access to healthcare systems. 
South Africa is doing well with SDG3, it has already achieved more than half 
of the mini goals falling under SGD3, the country is slowly crossing over to the
top right countries. South Africa is investing more in medical research and that
is probably why they are doing better than other african countries. They need to
continue educating the people from a young age about the dangers of the food 
they feed to their bodies, more sex education is needed and more youth employment
because the more young people on the streets, the more crime, more suicides due to 
mental illness, more unsafe sexual activities.
South Africa stands a good chance to achieve this goal.


It is sad that Africa is barely making it. But it is expected because decade after
decade the world proves just how little african lives matter to the whole world. 
As long as they are benefitting from the natural resources of Africa and that is all
that matters, more people dying in Africa is actually benefitting the world. 
The world needs to start investing in Africa. Africa is a very wealthy continent,
most of these other continents are getting richer at the expense of Africa, that
is wrong and it needs to change.



# Cluster analysis --------------------------------------------------------


#Loading the SDG data with NAs imputed
SDGs <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/SDG_complete.csv")
SDGs[1:5, 1:8]

#What are the parent location?
unique(SDGs$ParentLocation)

#How many countries do we have
length(SDGs$Location)

# a correalation matrix
corr <- round(cor(SDGs[3:ncol(SDGs)]), 1)
ggcorrplot(corr, type = 'upper', outline.col = "white", 
           colors = c("navy", "white", "#FC4E07"), 
           lab = TRUE)

#sTANDARDIZE to account for the different measurement scales
SDGs_std <- decostand(SDGs[3:ncol(SDGs)], method = "standardize")
SDGs_euc <- vegdist(SDGs_std, method = "euclidian")
rownames(SDGs_std) <- SDGs$Location # carry location names into output

#To find out how many clusters to use we can use fviz_nbclust
# using silhouette analysis
plt1 <- fviz_nbclust(SDGs_std, cluster::pam, method = "silhouette") + theme_grey()

# total within cluster sum of square / elbow analysis
plt2 <- fviz_nbclust(SDGs_std, cluster::pam, method = "wss") + theme_grey()

# gap statistics
plt3 <- fviz_nbclust(SDGs_std, cluster::pam, method = "gap_stat") + theme_grey()

ggarrange(plt1, plt2, plt3, nrow = 3)

#Choose 3 cluster
SDGs_pam <- pam(SDGs_std, metric = "euclidean", k = 3)

fviz_cluster(SDGs_pam, geom = "point", ellipse.type = "convex", palette = c("#FC4E07", "violetred3", "deepskyblue3"), ellipse.alpha = 0.05) +
  geom_text(aes(label = SDGs$Location), size = 2.5)

#Make the plot clearer
# scale SA bigger for plotting
SDGs <- SDGs %>%  
  mutate(col_vec = ifelse(Location == "South Africa", "black", "grey50"),
         scale_vec = ifelse(Location == "South Africa", 3.5, 2.5))

fviz_cluster(SDGs_pam, geom = "point", ellipse.type = "convex",
             palette = c("#FC4E07", "violetred3", "deepskyblue3"),
             ellipse.alpha = 0.05, pointsize = 2.0) +
  geom_text(aes(label = SDGs$Location), size = SDGs$scale_vec, col = SDGs$col_vec)

#Another way
fviz_cluster(SDGs_pam, palette = c("#FC4E07", "violetred3", "deepskyblue3"), ellipse.type = "euclid", 
             star.plot = TRUE, repel = TRUE, pointsize = SDGs$scale_vec * 0.8) + # SA, no 147, plotted slightly bigger
  theme_grey()

#Do a silhouette analysis to check cluster fidelity:
fviz_silhouette(SDGs_pam, palette = c("#FC4E07", "violetred3", "deepskyblue3"), ggtheme = theme_grey())

#Once happy with the number of clusters, find the median value for each cluster:
SDGs_centroids <- SDGs %>%  
  mutate(cluster = SDGs_pam$clustering) %>% 
  group_by(cluster) %>%  
  summarise_at(vars(other_1:SDG3.b_5), median, na.rm = TRUE)
SDGs_centroids

#Or you can use pam() to find the median values using example countries for each cluster
SDGs_pam$medoids

#We can do a coloured pairwise scatterplot to check data details
pairs(SDGs[, 3:10], col = c("#FC4E07", "violetred3", "deepskyblue3")[SDGs_pam$clustering])


# ANSWERS TO QUESTIONS ----------------------------------------------------

1. pam() plots a scatter plot to an 'ordination diagram' instead of a dendrogram
like in hierarchical or agglomerative clustering

2. In this example, the optimal would be 3 clusters because looking at the plots
above made using fviz_nbclust(), the difference between k =3 and k =4 is too small
that if we were to check the cluster fidelity of 4 clusters, the 4th one would be 
negative. So, 3 clusters is the optimal.

3. 

sdg_clust <- hclust(SDGs_euc, method = "ward.D")

par(mfrow =c(1, 1))
plot(sdg_clust) # display dendogram

#Cut the tree into 3 clusters, Customized color for groups
fviz_dend(sdg_clust, k = 3,
                   k_colors = c("#FC4E07", "violetred3", "deepskyblue3"))

The results are exactly the same. I would prefer to continue with the pam() method. It is easier to see and interpret.
Sometimes the dendrogram becomes too clustered at the end nodes and you cannot
see the words clearly.

4. 
#Using optimal number of clusters, k = 3
SDGs_pam <- pam(SDGs_std, metric = "euclidean", k = 3)

fviz_cluster(SDGs_pam, geom = "point", ellipse.type = "convex", palette = c("#FC4E07", "green", "deepskyblue3"), ellipse.alpha = 0.05) +
  geom_text(aes(label = SDGs$Location), size = 2.5)

#Make the plot clearer
# scale SA bigger for plotting
SDGs <- SDGs %>%  
  mutate(col_vec = ifelse(Location == "South Africa", "black", "grey50"),
         scale_vec = ifelse(Location == "South Africa", 3.5, 2.5))

#cluster plot
fviz_cluster(SDGs_pam, palette = c("#FC4E07","green", "deepskyblue3"), ellipse.type = "euclid", 
             star.plot = TRUE, repel = TRUE, pointsize = SDGs$scale_vec * 0.8) + # SA, no 147, plotted slightly bigger
  theme_grey()

#Do a silhouette analysis to check cluster fidelity:
fviz_silhouette(SDGs_pam, palette = c("#FC4E07", "green", "deepskyblue3"), ggtheme = theme_grey())

4.
Cluster group 1 consists of the countries that are really struggling to meet this
UN goal. These countries have the most cases of child and martenal cases due to
the poor and unsafe WASH services, and the lack of good healthcare system. Education
in these countries is still not available to everyone, there is very little employment,
and the few that get education leave these countries the first chance they get. These
are most african countries.
Cluster 2 is the developing countries, more urbanised than the 1st cluster, so
they no longer struggle with child and martenal mortalities due to poor healthcare
system and WASH services. These countries have high new HIV cases because it is in
the urban areas and lots of parties and unsafe sex, more sex work as well. More
TB incidences because of industries, polluted air, and more cases of cardivascular 
diseases and deaths beacuse people eat unhealthy food. These are a few african
countries and asian countries and South American.
Cluster 3 is what everyone wishes for. They have reached the goal. These are
the developed countries. These are Oceania and Europe.

Africa as a continent is really struggling to reach this goal but a lot has
improved. The mortalities, life expectancy and the number of poeple dying from HIV
have decreased dramatically over the years. Most African countries are underdeveloped,
there are not enough resources for everyone, the WASH services are poor and unsafe,
hence most African countries fall in the group of countries with the most child and
martenal deaths in the world. 
The UN knows that Africa alone, on its own, cannot reach half these goals. The
world needs to help out. But we have seen over the years how Africans have not
been top priority.

5.
South Africa is the most multiracial country in Africa, that attracts international
investors. That means the country will have money for research and can provide
basic needs to the people. There is free healthcare system for everyone, therefore,
there are very few cases of child and martenal deaths. There is adequate assistance
for medical research and research in general, there have been medical breakthroughs
from South Africa. So, there is knowledge on a lot of diseases and more people 
are educated on these.

But South Africa is still in Africa, crippled by and still suffers the effects of
the history of the continent (colonisation, apartheid...). The unemployment rate
is very high, with increasing cases of mental illnesses amongst the youth. This 
increases suicide rates, the crime rate, and reckless sexual behaviour, increasing
the new HIV infections. We still have low life expectancy just like all african
countries and we still have a lot of teenage pregnancy and all these prevent us 
from going all the way to the other side.
