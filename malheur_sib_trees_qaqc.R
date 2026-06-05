#############################################################################################
# SIB Trees QA/QC Script
# Author: Maggie Koontz
# Purpose: Review Malheur SIB Trees data for errors
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

# Load data

dat <- read_excel(paste(user_paths_data[current_user], 
                        "SIB_trees.xlsx",
                        sep = ""))


#---------------------------------------------------------------------------------------------
# 2. Numeric Check
#---------------------------------------------------------------------------------------------

#check for negatives
dat %>%
  filter(Plot < 1) %>%
  filter(`Tree number` < 1)

#use histograms to check there are no plot numbers with a count of 1
ggplot(dat, aes(x = Plot)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Plot Numbers", x = "Plot No.", y = "Count") +
  theme_minimal()
#visually appears there are no plot no counts with only a few samples, which is good

#check same thing using group_by()
dat %>%
  group_by(Plot) %>%
  filter(n()<10)
#one new tree tally row with no plot or treatment info

#check there are no stands with a count of one
ggplot(dat, aes(x = Stand)) +
  geom_bar(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Stand", x = "Stand", y = "Count") +
  theme_minimal()
#ok, besides NA row

#check there are no treatment types with a count of one
ggplot(dat, aes(x = Treatment)) +
  geom_bar(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Treatment", x = "Treatment", y = "Count") +
  theme_minimal()
#ok, but there are over 500 samples with treatment type labeled as NA

#---------------------------------------------------------------------------------------------
# 3. Categorical Check
#---------------------------------------------------------------------------------------------

unique(dat$Year)
unique(dat$`Condition (L/D)`)
unique(dat$`Standing (Y/N)`)

#---------------------------------------------------------------------------------------------
# 4. Conditional Check
#---------------------------------------------------------------------------------------------

#check there are not trees of the same number within the same plot
dat %>%
  group_by(Year, Stand, Treatment, Plot, `Tree number`) %>%
  filter(n() > 1) 

dat %>%
  group_by(Year, Stand, Treatment, Plot, `Tree number`) %>%
  filter(n() > 2)
#32 duplicate rows, all with n = 2. Probably due to the fact that when I entered data I recorded everything as 
#2025, not sure if this should be changed or not

#find instances of live trees that are not standing
live_standing <- dat %>%
  filter(`Condition (L/D)`=="L" & `Standing (Y/N)` == "N")
#One sample of a live downed tree (indicated by notes, so this is not a recording typo).
#not sure if this an acceptable catagorization or not

#check plot numbers are unique to their treatments and stands
dat %>%
  group_by(Plot) %>%
  summarise(
    ok = n_distinct(Treatment) == 1 & #n_distinct is a more concise nrow(unique(dat()))
      n_distinct(Stand) == 1,
    .groups = "drop"
  ) %>%
  filter(!ok)
#ok, every plot number has unique treatment and stand combination

#check tree numbers are unique to treatment, stand, and plot
duplicate_tree_nos <- dat %>%
  group_by(`Tree number`) %>%
  summarise(
    ok = n_distinct(Treatment) == 1 &
      n_distinct(Stand) == 1 &
      n_distinct(Plot) == 1,
    .groups = "drop"
  ) %>%
  filter(!ok)
#for example, there are two rows with tree number 4208 that have different 
#plot and treatment categorizations

#are these an issue?

