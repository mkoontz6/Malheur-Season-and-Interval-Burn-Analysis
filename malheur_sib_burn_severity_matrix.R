#############################################################################################
# Independent Variables Matrix
# Author: Jim Cronan, Emily Sanders, Maggie Koontz
# Purpose: Collate and format raw data containing independent variables on burn severity
#into a single matrix with variables arranged in columns and sites arranged in rows.
#############################################################################################

#Load libraries
library(dplyr)#graphics
library(ggplot2)#graphics
library(readr)#read.csv() {utils}.

#---------------------------------------------------------------------------------------------
# 1. Load data
#---------------------------------------------------------------------------------------------

#Data
# Map usernames to file paths
user_paths_burn <- c(
  Nat   = "",
  Becky = "",
  jcronan = "",
  esande02 = "C:/Users/esande02/Downloads/FERA/Malheur/burn_severity/",
  mak600 = "C:\\Users\\mak600\\Documents\\Malheur\\Malheur Data\\"
)

#Lookup Tables
# Map usernames to file paths
user_paths_lut <- c(
  Nat   = "",
  Becky     = "",
  jcronan = "",
  esande02 = "C:/Users/esande02/Downloads/FERA/Malheur/burn_severity/lut_burn_severity_file_names.csv",
  mak600 = "C:\\Users\\mak600\\Documents\\Malheur\\Malheur Data\\lut_burn_severity_file_names.csv")


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
burn_year <- read.csv(paste(user_paths_lut[current_user]))

#Burn severity data
for (i in seq_along(burn_year$file_name_year))
{
  object_name <- paste("b", burn_year$file_name_year[i], sep = "")
  
  temp <- read.csv(paste(user_paths_burn[current_user],
                         "Plot_severity_",
                         burn_year$file_name_year[i],
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

test_mat <- bind_rows(b2003, b2008, b2012)
View(test_mat)


