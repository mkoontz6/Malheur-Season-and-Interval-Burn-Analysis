#############################################################################################
# SIB 1000 hr fuels QA/QC Script
# Author: Maggie Koontz
# Purpose: Review SIB 1000 hr fuels entered from spring 2026 data collection for errors
# Date: June 5, 2026
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

# Load 1000 hr data

dat <- read_excel(paste(user_paths_data[current_user], 
                        "SIB_fuels_1000_hr.xlsx",
                        sep = ""))
dat_2026 <- dat %>%
  filter(!Year=="2025")

# Load litter duff data to cross reference direction

litter_duff <- read_excel(paste(user_paths_data[current_user], 
                        "SIB_fuels_litter_duff.xlsx",
                        sep = ""))

# Load 1, 10, 100 hr data to cross reference direction

other_fuels <- read_excel(paste(user_paths_data[current_user], 
                        "SIB_fuels_1_10_100_hr.xlsx",
                        sep = ""))

#---------------------------------------------------------------------------------------------
# 2. Numeric Check
#---------------------------------------------------------------------------------------------
#check all directions are between 0 and 360
dat_2026 %>%
  filter(Direction > 360 | Direction < 0)

#check for negatives
dat_2026 %>%
  filter(Diameter < 1)
  
dat_2026 %>%
  filter(`Decay class` < 1)

ggplot(dat_2026, aes(x = Diameter)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of 1000 Hr Fuels Diameter", x = "diameter (cm)", y = "Count") +
  theme_minimal()

#check all 1000 fuels are greater than the cutoff diameter
dat_2026 %>%
  filter(Diameter < 7.6)
#three samples that should be changed to 100-hr fuels

#check outlier samples that are highlighted by the histogram
dat_2026 %>%
  filter(Diameter > 40)
#not any other data like length to verify high diameters

#check decay class frequencies
ggplot(dat_2026, aes(x = `Decay class`)) +
  geom_bar(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Decay Classes", x = "Decay Class", y = "Count") +
  theme_minimal()
#ok


#---------------------------------------------------------------------------------------------
# 3. Categorical Check
#---------------------------------------------------------------------------------------------

unique(dat_2026$Year)
unique(dat_2026$Stand)
unique(dat_2026$Treatment)
unique(dat_2026$Species)
unique(dat_2026$Elevated)

#---------------------------------------------------------------------------------------------
# 4. Conditional Check
#---------------------------------------------------------------------------------------------

#check any transects without 1000 fuels do not have data in other columns
dat_2026 %>%
  filter(Species == "NONE") %>%
  filter(!is.na(Elevated)| !is.na(Diameter) | !is.na(`Decay class`))

#check that transect directions match at least one of the transect directions in litter duff
#for the same stand, treatment, plots
missing_dirs_1 <- dat_2026 %>%
  distinct(Stand, Treatment, Plot, Direction) %>%
  anti_join(
    litter_duff %>%
      distinct(Stand, Treatment, Plot, Direction),
    by = c("Stand", "Treatment", "Plot", "Direction")
  )

missing_dirs_1

#four directions in 1000 hr fuels that do not match litter duff data
#compare these with missing_dir with the rows they SHOULD match with
litter_duff %>%
  filter(Stand=="Driveway 17") %>%
  filter(Treatment == "Control") %>%
  filter(Plot == "20.0")
#in litter_duff data set, driveway 17 control plot 20 has directions 93 and 273
#but in 1000hr fuels, driveway 17 control plot 20 has directions 20 and 200

litter_duff %>%
  filter(Stand=="Driveway 17") %>%
  filter(Treatment=="Fall 15") %>%
  filter(Plot == "17B")
#in litter_duff dataset, there is no driveway 17 fall 15, plot 17B
#but in 1000hr fuels, driveway 17 fall 15 plot 17B exists with directions 274 and 94



#do the same check but comparing 1000 hr fuels to 1, 10, 100 hr fuels dataset
#check that transect directions match at least one of the transect directions
#for the same stand, treatment, plots
missing_dirs_2 <- dat_2026 %>%
  distinct(Stand, Treatment, Plot, Direction) %>%
  anti_join(
    other_fuels %>%
      distinct(Stand, Treatment, Plot, Direction),
    by = c("Stand", "Treatment", "Plot", "Direction")
  )

#driveway 17 control plot 20 direction 20 and 200 come up in both missing dir 1 and 2
other_fuels %>%
  filter(Stand=="Driveway 17") %>%
  filter(Treatment == "Control") %>%
  filter(Plot == "20.0")

other_fuels %>%
  filter(Stand=="Driveway 17") %>%
  filter(Treatment=="Fall 5") %>%
  filter(Plot == "15A")
#this combination exists in 1000 hour fuels but not 1 10 100 hr fuels

other_fuels %>%
  filter(Stand=="Driveway 17") %>%
  filter(Treatment=="Fall 5") %>%
  filter(Plot == "15.0")
#this combination exists in 1000 hr fuesl but not 1 10 100 hr fuels

#CONCLUSIONS:

#fairly confident that in 1000 hr fuels data driveway 17 control plot 20 directions should
#be changed from 20 and 200 to 93 and 273 to match litter and duff data and 1 10 100 hr data

#Driveway 17 Fall 5 15A directions 189 and 9,
#Driveway 17 Fall 5 15.0 directions 136 and 316,
#Driveway 17 Fall 15 17B directions 274 and 94
#are combinations that exists in 1000 hr fuels data but not in litter duff data or 1 10 100 hr data

