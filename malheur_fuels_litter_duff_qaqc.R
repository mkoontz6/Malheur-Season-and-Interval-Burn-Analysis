#############################################################################################
# Fuels litter duff QA/QC Script
# Author: Maggie Koontz
# Purpose: Review Malheur fuels litter duff for errors
# Date: June 1, 2026
#############################################################################################

#Load libraries
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)#readxl()
library(tidyverse)

#---------------------------------------------------------------------------------------------
# 1. Load data
#---------------------------------------------------------------------------------------------

#Data
# Map usernames to file paths
user_paths_data <- c(
  Emily   = "emsanderss",
  mak600     = "C:\\Users\\mak600\\Documents\\Malheur Downloaded Data\\",
  jcronan = "C:/Users/jcronan/Box/FERA-UW/Research/AustinWater_FuelsAssessment/8_Data/Field_Data/05_Data_csv/"
)

#Lookup Tables
# Map usernames to file paths

#no lookup tables yet


# Detect current user
current_user <- Sys.info()[["user"]]

# Check if user exists in mapping for data files
if (!current_user %in% names(user_paths_data)) {
  stop("No file path configured for this user: ", current_user)
}


# Check if user exists in mapping for lut files
#if (!current_user %in% names(user_paths_lut)) {

#na
  
# Load data

dat <- read_excel(paste(user_paths_data[current_user], 
                        "SIB_fuels_litter_duff.xlsx",
                        sep = ""))
#reshape data so that each depth measurement has a unique row
#in other words there will be two rows per transect

library(dplyr)
library(ggplot2)

depth_long <- dat %>%
  pivot_longer(
    cols = c(`6m litter/duff type`, `6m depth`,
             `12m litter/duff type`, `12m depth`),
    names_to = c("distance", ".value"),
    names_pattern = "(6m|12m) (.*)"
  )

head(depth_long)

#---------------------------------------------------------------------------------------------
# 2. Numeric Check
#---------------------------------------------------------------------------------------------
plot_1 <- ggplot(dat, aes(x = `6m depth`)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of 6m fuels depth", x = "Litter/Duff Depth (mm)", y = "Frequency at 6m on Transect") +
  theme_minimal()


depth_outliers_6m <- dat %>%
  filter(`6m depth` > 100)
#most depths fall between 0 and 100 mm, 5 measurements over 100 mm depth 

plot_2 <- ggplot(dat, aes(x = `12m depth`)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of 12m fuels depth", x = "Litter/Duff Depth (mm)", y = "Frequency at 12m on Transect") +
  theme_minimal()
#very similar distribution to 6m depths, skewed right, with 8 measurements over 100mm depths

depth_outliers_12m <- dat %>%
  filter(`12m depth` > 100)

intersect(depth_outliers_6m,depth_outliers_12m)
#one transect with both 12m and 6m measurements over 100mm

#not familiar enough with the methods to know if it suspicious if depth measurements
#vary greatly between 12 and 6 m along a transect. I would assume it is not suspicious.

#Chart depths by litter/duff type
unique(depth_long$`litter/duff type`)
#24 unique litter/duff types across both 12m and 6m samples

#should bare soil, mineral soil and bare be consolidated?
#should pipo bark, bark decay, bark flakes, bark plate pipo, bark pipo, bark flakes (pipo) be consolidated?
#should carex, grass thatch, rice, sedge and grass be consolidated?
#overall, seems like varying degrees of specificity is creating false diversity

#are CEVE a ceanothus? what is CELE?

#create a preliminary lookup table to reclassify duff and litter types
#note- I need to double check with Jim/Nate that these reclassifications are accurate

# Define vectors
old_name <- c(
  #soils   
   "BARE SOIL", "MINERAL SOIL", "BARE",#should rock be added to this category?
  #BARK, assuming PIPO bark should NOT get it's own category 
    "BARK FLAKES","BARK DECAY","PIPO BARK", "BARK PIPO", "BARK FLAKES (PIPO)", "BARK PLATE PIPO",
  #LIVE GRASS
    "CAREX","GRASS","RICE","SEDGE") #again, not confident if this is accurate categorization

new_name <- c(
  #SOILS
  "BARE", "BARE", "BARE",
  #BARKS
  "BARK", "BARK", "BARK","BARK","BARK", "BARK",
  #LIVE GRASS
  "LIVE GRASS", "LIVE GRASS","LIVE GRASS","LIVE GRASS")


# Combine into a data frame
fuels_category_lut <- data.frame(old_name = old_name, new_name = new_name)

#use lut to create new recategorized data
dat_2 <- depth_long %>%
  left_join(
    fuels_category_lut,
    by = c("litter/duff type" = "old_name")
  ) %>%
  mutate(
    fuel_type_updated = coalesce(new_name, `litter/duff type`)
  ) %>%
  select(-new_name,-`litter/duff type`)

unique(dat_2$fuel_type_updated)

#ARTR
ggplot(filter(dat_2, fuel_type_updated == "ARTR"), aes(x = depth)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of ARTR Depth", x = "depth (mm)", y = "Count") +
  theme_minimal()

#PIPO
ggplot(filter(dat_2, fuel_type_updated == "PIPO"), aes(x = depth)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of PIPO Depth", x = "depth (mm)", y = "Count") +
  theme_minimal()

#BARE
ggplot(filter(dat_2, fuel_type_updated == "BARE"), aes(x = depth)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Bare Depth", x = "depth (mm)", y = "Count") +
  theme_minimal()

#two samples over of bare soil over 0
#this may be an issue depending on methods, especiall for 40 mm sample
dat_2 %>%
  filter(fuel_type_updated == "BARE" & depth >0)

#GRASS THATCH
ggplot(filter(dat_2, fuel_type_updated == "GRASS THATCH"), aes(x = depth)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Grass Thatch Depth", x = "depth (mm)", y = "Count") +
  theme_minimal()

dat_2 %>%
  filter(fuel_type_updated == "GRASS THATCH" & depth > 40)

#BARK
ggplot(filter(dat_2, fuel_type_updated == "BARK"), aes(x = depth)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Bark Depth", x = "depth (mm)", y = "Count") +
  theme_minimal()

dat_2 %>%
  filter(fuel_type_updated == "BARK" & depth > 100)

#LIVE GRASS
ggplot(filter(dat_2, fuel_type_updated == "LIVE GRASS"), aes(x = depth)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Live Grass", x = "depth (mm)", y = "Count") +
  theme_minimal()

#CEVE
ggplot(filter(dat_2, fuel_type_updated == "CEVE"), aes(x = depth)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of CEVE", x = "depth (mm)", y = "Count") +
  theme_minimal()

ggplot(filter(dat_2, fuel_type_updated == "ROCK"), aes(x = depth)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of CEVE", x = "depth (mm)", y = "Count") +
  theme_minimal()


#JUOC, MOSS, CELE, PIPO/WOOD ROT each have only one sample
#WOOD ROT has two samples

#ROCK has a negative sample

#on that note, check for negative depths
#use unedited data frame "dat" instead of dat_2 or depth_long
dat %>% filter(`6m depth`<0)
dat %>% filter(`12m depth`<0)

zero_depths_6m <- (dat %>% filter(`6m depth`== 0))
unique(zero_depths_6m$`6m litter/duff type`)

zero_depths_12m <- dat %>% filter(`12m depth`== 0)
unique(zero_depths_12m$`12m litter/duff type`)

zero_depth_both_samples<-intersect(zero_depths_6m,zero_depths_12m)
#4 transects had zero depths for both samples



#---------------------------------------------------------------------------------------------
# 2. Categorical Checks
#---------------------------------------------------------------------------------------------
unique(dat$Year)
unique(dat$Stand)
unique(dat$Treatment)
unique(dat$Plot)



#---------------------------------------------------------------------------------------------
# 3. Conditional Checks
#---------------------------------------------------------------------------------------------

#check there are no directions over 360 or under 0
dat %>%
  filter(Direction > 360 | Direction < 0) #OK

#check there are no duplicate transect directions per plot
dat %>%
  group_by(Stand, Treatment, Plot, Direction) %>%
  filter(n() > 1) #ok

#check there are no more than two transect total per plot
dat %>%
  group_by(Stand, Treatment, Plot) %>%
  filter(n() > 2 | n() < 2)  #ok
#there are two transects that have unique combinations of year, stand, treatment and plot
#so they are missing a plot transect with a different direction
#the transect IDs are identical except one is stand Driveway 14 and one is Driveway 15
#investigate is this is a typo 

#------ MK 6/1/26 left off here

#does not seem like the same treatment and plots were sampled in 2025 and 2026, but check this