#############################################################################################
# Fire severity Matrix
# Author: Jim Cronan, Emily Sanders, Maggie Koontz
# Purpose: Collate and format raw data containing independent variables on burn severity
#into a single matrix with variables arranged in columns and sites arranged in rows.
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

#Data
# Map usernames to file paths
user_paths_burn <- c(
  Nat   = "",
  Becky = "",
  jcronan = "C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/Severity_indices/Fire_severity/",
  esande02 = "C:/Users/esande02/Downloads/FERA/Malheur/burn_severity/",
  mak600 = "C:\\Users\\mak600\\Documents\\Malheur\\Malheur Data\\"
)

#Lookup Tables
# Map usernames to file paths
user_paths_lut <- c(
  Nat   = "",
  Becky     = "",
  jcronan = "C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/Severity_indices/Fire_severity/",
  esande02 = "C:/Users/esande02/Downloads/FERA/Malheur/burn_severity/lut_burn_severity_file_names.csv",
  mak600 = "C:\\Users\\mak600\\Documents\\Malheur\\Malheur Data\\lut_burn_severity_file_names.csv")



# Outgoing (saved) data
user_paths_saved_data <- c(
  Nate   = "",
  Becky = "",
  jcronan = "C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/02_Clean_Data/Severity_indices/Fire_severity/",
  esande02 = "",
  mak600 = ""
)


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


#Load data
#Load lut
burn_year <- c(2003, 2008, 2012)

#Burn severity data
for (i in 1:length(burn_year))
{
  object_name <- paste("b", burn_year[i], sep = "")
  
  temp <- read.csv(paste(user_paths_burn[current_user],
                         "Plot_severity_",
                         burn_year[i],
                         ".csv",
                         sep = ""))
  
  assign(object_name, temp)
}

#---------------------------------------------------------------------------------------------
# 2. Data structure
#---------------------------------------------------------------------------------------------

str(b2003)
str(b2008)
str(b2012)

b2003$Year <- 2003
b2008$Year <- 2008
b2012$Year <- 2012

#---------------------------------------------------------------------------------------------
# 3. Merge datasets
#---------------------------------------------------------------------------------------------
fsc_1 <- bind_rows(b2003, b2008, b2012)


#Remove exclosure plots
fsc_2 <- 
  fsc_1 %>% 
  filter(
    !grepl("^98\\d{2}$", as.character(Plot)),
  )



#-------------------------------------------------------------------------------------
# 4. Save corrected data
#---------------------------------------------------------------------------------------------
#This is corrected data.
#No data has been removed.

#Set working directory to clean data folder.
setwd(paste(user_paths_saved_data[current_user], sep = ""))

#Set date and time.
dt <- Sys.Date()
clean_dt <- gsub("-", "", dt, fixed = T)

write_csv(fsc_2, paste(clean_dt, "_burn_severity_fire_clean.csv", sep = ""))

#---------------------------------------------------------------------------------------------
# End
#---------------------------------------------------------------------------------------------



