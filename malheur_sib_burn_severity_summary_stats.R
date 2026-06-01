#############################################################################################
# Fire severity Summary Statistics
# Author: Jim Cronan
# Purpose: Collate and fire severity data and generate summary statistics.
#Date started: 8-April-2026
#############################################################################################

#Load libraries
library(dplyr)#graphics
library(ggplot2)#graphics
library(readr)#??? - read.csv() {utils}.
library(gridExtra) #to display multiple histograms at once -- grid.arrange()
library(tidyverse) #needed for pipe function (%>%)

#---------------------------------------------------------------------------------------------
# 1. Load data
#---------------------------------------------------------------------------------------------

#Clean Data - canopy
# Map usernames to file paths
user_paths_canopy <- c(
  Nat   = "",
  Becky = "",
  jcronan = "C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/02_Clean_Data/Severity_indices/Canopy/",
  esande02 = "",
  margaretkoontz = ""
)

#Clean Data - ground
# Map usernames to file paths
user_paths_ground <- c(
  Nat   = "",
  Becky = "",
  jcronan = "C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/02_Clean_Data/Severity_indices/Ground_cover/",
  esande02 = "",
  margaretkoontz = ""
)


# Outgoing (saved) data


# Detect current user
current_user <- Sys.info()[["user"]]

# Check if user exists in mapping for data files
if (!current_user %in% names(user_paths_burn)) {
  stop("No file path configured for this user: ", current_user)
}

# Check if user exists in mapping for lut files
if (!current_user %in% names(user_paths_lut)) {
  stop("No file path configured for this user: ", current_user)
}


#Load canopy data
c1 <- read.csv(paste(user_paths_canopy[current_user], 
                     "20260406_burn_severity_canopy_clean.csv", sep = ""))

#Load ground data
g1 <- read.csv(paste(user_paths_ground[current_user], 
                     "20260407_burn_severity_ground_clean.csv", sep = ""))


#Since ground cover and canopy cover data are sampled at different points in the plot average them
#before you start collating them together,

#Calculate plot average for canopy data
# Step 1: Calculate average measurement per plot
c2 <- c1 %>%
  group_by(Plot, year) %>%
  summarise(
    canopy_cover = mean(percent_canopy_cover, na.rm = TRUE),
    .groups = "drop"
  )

#Calculate plot average for ground cover data
g2 <- g1 %>%
  group_by(ground_type, Plot, year) %>%
  summarise(
    ground_cover = mean(cover_percent, na.rm = TRUE),
    .groups = "drop"
  )
