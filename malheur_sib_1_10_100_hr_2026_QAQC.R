#############################################################################################
# SIB 1 10 100 hr fuels QA/QC Script
# Author: Maggie Koontz
# Purpose: Review SIB 1 10 100 hr fuels entered from spring 2026 data collection for errors
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
                        "SIB_fuels_1_10_100_hr.xlsx",
                        sep = ""))
dat_2026 <- dat %>%
  filter(!Year=="2025")

# Load litter duff data to cross reference direction

litter_duff <- read_excel(paste(user_paths_data[current_user], 
                                "SIB_fuels_litter_duff.xlsx",
                                sep = ""))

# Load 1, 10, 100 hr data to cross reference direction

thousand_hr_fuels <- read_excel(paste(user_paths_data[current_user], 
                                "SIB_fuels_1000_hr.xlsx",
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
  filter(`1-hour` < 0)

dat_2026 %>%
  filter(`10-hour` < 0)

dat_2026 %>%
  filter(`100-hour` < 0)

ggplot(dat_2026, aes(x = `1-hour`)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of 1 Hr Fuels Counts", x = "Number of Fuels", y = "Count") +
  theme_minimal()

ggplot(dat_2026, aes(x = `10-hour`)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of 10 Hr Fuels Counts", x = "Number of Fuels", y = "Count") +
  theme_minimal()

#ok, one transect had 14 counts

ggplot(dat_2026, aes(x = `100-hour`)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of 100 Hr Fuels Counts", x = "Number of Fuels", y = "Count") +
  theme_minimal()

#ok, a few had 10 counts


#---------------------------------------------------------------------------------------------
# 3. Categorical Check
#---------------------------------------------------------------------------------------------

unique(dat_2026$Year)
unique(dat_2026$Stand)
unique(dat_2026$Treatment)


#---------------------------------------------------------------------------------------------
# 4. Conditional Check
#---------------------------------------------------------------------------------------------

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


#six directions in 1 10 100 hr fuels that do not match litter duff data

#1. Directions 274 and 92

#compare these with missing_dir with the rows they SHOULD match with
litter_duff %>%
  filter(Stand=="Driveway 17") %>%
  filter(Treatment == "Fall 15") %>%
  filter(Plot == "17B")
#in litter_duff data set, there is no driveway 17 fall 15 17B
#I believe these data were misrecorded as Driveway 17 and should be Driveway 26
#check:
litter_duff %>%
  filter(Stand=="Driveway 26") %>%
  filter(Treatment == "Fall 15") %>%
  filter(Plot == "17B")
#the only plot 17B when looking at the original scanned datasheets is in driveway 26
#and the same note "no ends found, random azimuth chosen" is written for both driveway 17 plot 
#17B sample in 1 10 100 hr fuels and driveway 26 plot 17B in litter duff and physical data scan


#2. directions 9 and 189

litter_duff %>%
  filter(Stand=="Driveway 17") %>%
  filter(Treatment=="Fall 15") %>%
  filter(Plot == "15A")
#in litter_duff dataset, there is no driveway 17 fall 15, plot 15A, but there is
#driveway 15 fall 5 plot 15A

#check this matches direction:
litter_duff %>%
  filter(Stand=="Driveway 17") %>%
  filter(Treatment=="Fall 5") %>%
  filter(Plot == "15A")
#yes, so fall 15 shoudl be changed to fall 5


#3. directions 136 and 316
litter_duff %>%
  filter(Stand=="Driveway 17") %>%
  filter(Treatment=="Fall 15") %>%
  filter(Plot == "15.0")
#in litter_duff dataset, there is no driveway 17 fall 15, plot 15
#I believe these in 1 10 100 hr dataset Driveway 17 Fall 15  17B should be Fall 5
#because this makes the directions match litter_duff data
#check:
litter_duff %>%
  filter(Stand=="Driveway 17") %>%
  filter(Treatment=="Fall 5") %>%
  filter(Plot == "15.0")
#yes



#check that transect directions match at least one of the transect directions in 1000 hr fuels
#for the same stand, treatment, plots
missing_dirs_2 <- dat_2026 %>%
  distinct(Stand, Treatment, Plot, Direction) %>%
  anti_join(
    thousand_hr_fuels %>%
      distinct(Stand, Treatment, Plot, Direction),
    by = c("Stand", "Treatment", "Plot", "Direction")
  )

#there are 20 instances of directions not matching 


