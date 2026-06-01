#############################################################################################
# Forest Floor Matrix
# Author: Jim Cronan
# Purpose: Collate and format raw data containing independent variables into a single
#matrix with variables arranged in columns and sites arranged in rows.
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
user_paths_ground <- c(
  Nat   = "",
  Becky     = "",
  jcronan = "C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/Severity_indices/Ground_cover/"
)

#Lookup Tables
# Map usernames to file paths
user_paths_lut <- c(
  Nate   = "C:/Users/NathanWade/Box/SIB/Cronan/Wade/3_Data/01_Raw_Data/Severity_indices/Ground_cover/",
  Becky     = "",
  jcronan = "C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/Severity_indices/Ground_cover/",
  esande02 = "C:/Users/esande02/Downloads/FERA/Malheur/burn_severity/lut_burn_severity_file_names.csv",
  mak600 = "C://Users//mak600//Documents//Malheur//Canopy Data//canopy_file_lut.csv")

# Outgoing (saved) data
user_paths_saved_data <- c(
  Nate   = "C:/Users/NathanWade/Box/SIB/Cronan/Wade/3_Data/02_Clean_Data/Severity_indices/Ground_cover/",
  Becky = "",
  jcronan = "C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/02_Clean_Data/Severity_indices/Ground_cover",
  esande02 = "",
  mak600 = ""
)

# Detect current user
current_user <- Sys.info()[["user"]]

# Check if user exists in mapping for data files
if (!current_user %in% names(user_paths_ground)) {
  stop("No file path configured for this user: ", current_user)
}


# Check if user exists in mapping for lut files
if (!current_user %in% names(user_paths_canopy)) {
  stop("No file path configured for this user: ", current_user)
}

#Ground lookup tables
#Plot list
plot_lut <- read.csv(paste(user_paths_ground[current_user], 
                           "lut_plots.csv", sep = ""))
#File list
ground_year <- read.csv(paste(user_paths_ground[current_user], 
                              "lut_ground_file_names.csv", sep = ""))
#Cover type crosswalk table
ctct <- read.csv(paste(user_paths_ground[current_user], 
                       "data_dictionary.csv", sep = ""))

#Ground cover data.
for (i in 1:length(ground_year$file_name_year))
{
  object_name <- paste("g", ground_year$file_name_year[i], sep = "")
  temp <- read.csv(paste(user_paths_ground[current_user], 
                         "Ground_", ground_year$file_name_year[i], 
                         ".csv", sep = ""))
  assign(object_name, temp)
  rm(object_name, temp)
}

#---------------------------------------------------------------------------------------------
# 2. Create uniform cover data categories
#---------------------------------------------------------------------------------------------

#List of new column headings for cover types.
replacements <- sort(unique(ctct$new_name))

#Object to hold outgoing object names - you will need this for next action.
names_v1 <- vector()
names_check <- list()

for(a in 1:length(ground_year$file_name_year))
{
  obj <- get(ground_year$file_name[a])
  cn <-  paste("cats_", ground_year$file_name_year[a], sep = "") 
  for(i in 1:length(replacements))
  {
    old_names <- ctct$Column_heading[which(ctct[,cn] == "Y" & 
                                             ctct$new_name == replacements[i])]
    new_names <- rep(replacements[i],length(old_names))
    match_names_1 <- new_names[match(names(obj), old_names)]
    match_names_2 <- match_names_1[!is.na(match_names_1)]
    names(obj)[names(obj) %in% old_names] <- match_names_2
    rm(old_names, new_names, match_names_1, match_names_2)
  }
  names_v1[a] <- paste("g", ground_year$file_name_year[a], "_1", sep = "") 
  assign(names_v1[a], obj)
  names_check[[a]] <- unique(colnames(obj))
  rm(obj, cn)
}

#---------------------------------------------------------------------------------------------
# 4. Sum cover data for each year
#---------------------------------------------------------------------------------------------

#Object to hold outgoing object names - you will need this for next action.
names_v2 <- vector()

for(a in 1:length(ground_year$file_name_year))
{
  obj <- get(names_v1[a])
  out_file <- data.frame(
    litter = rep(0,length(obj[,1])), 
    mineral = rep(0,length(obj[,1])), 
    moss = rep(0,length(obj[,1])), 
    other = rep(0,length(obj[,1]))
  )
  for(i in 1:length(replacements))
  {
    obj_cvr <- obj[,colnames(obj) == replacements[i]]
    obj_cvr <- as.matrix(obj_cvr)
    out_file[,i] <- apply(obj_cvr,1,sum, na.rm = T)
    rm(obj_cvr)
  }
  out_file_append <- data.frame(
    Plot = obj$Plot, 
    Quad = obj$Quad, 
    out_file)
  names_v2[a] <- paste("g", ground_year$file_name_year[a], "_2", sep = "") 
  assign(names_v2[a], out_file_append)
  rm(obj, out_file, out_file_append)
}

#---------------------------------------------------------------------------------------------
# 5. Standardizing each annual dataset
#---------------------------------------------------------------------------------------------

#2002
#convert to long format
g2002_3 <- g2002_2 %>%
  pivot_longer(
    cols = -c(Plot, Quad), 
    names_to = "ground_type", 
    values_to  = "cover_percent") %>%
  mutate(year = "2002")
g2002_df <- as.data.frame(g2002_3)

#2003
#convert to long format
g2003_3 <- g2003_2 %>%
  pivot_longer(
    cols = -c(Plot, Quad), 
    names_to = "ground_type", 
    values_to  = "cover_percent") %>%
  mutate(year = "2003")
g2003_df <- as.data.frame(g2003_3)

#2004
#convert to long format
g2004_3 <- g2004_2 %>%
  pivot_longer(
    cols = -c(Plot, Quad), 
    names_to = "ground_type", 
    values_to  = "cover_percent") %>%
  mutate(year = "2004")
g2004_df <- as.data.frame(g2004_3)

#2007
#convert to long format
g2007_3 <- g2007_2 %>%
  pivot_longer(
    cols = -c(Plot, Quad), 
    names_to = "ground_type", 
    values_to  = "cover_percent") %>%
  mutate(year = "2007")
g2007_df <- as.data.frame(g2007_3)

#2009
#convert to long format
g2009_3 <- g2009_2 %>%
  pivot_longer(
    cols = -c(Plot, Quad), 
    names_to = "ground_type", 
    values_to  = "cover_percent") %>%
  mutate(year = "2009")
g2009_df <- as.data.frame(g2009_3)

#2012
#convert to long format
g2012_3 <- g2012_2 %>%
  pivot_longer(
    cols = -c(Plot, Quad), 
    names_to = "ground_type", 
    values_to  = "cover_percent") %>%
  mutate(year = "2012")
g2012_df <- as.data.frame(g2012_3)

#2015
#convert to long format
g2015_3 <- g2015_2 %>%
  pivot_longer(
    cols = -c(Plot, Quad), 
    names_to = "ground_type", 
    values_to  = "cover_percent") %>%
  mutate(year = "2015")
g2015_df <- as.data.frame(g2015_3)

#2025
#convert to long format
g2025_3 <- g2025_2 %>%
  pivot_longer(
    cols = -c(Plot, Quad), 
    names_to = "ground_type", 
    values_to  = "cover_percent") %>%
  mutate(year = "2025")
g2025_df <- as.data.frame(g2025_3)

#---------------------------------------------------------------------------------------------
# 6. Aggregate into a single, vertical, cleaned dataset
#---------------------------------------------------------------------------------------------

#Combine all canopy files into a single table.
#Merge comments and notes,
#Remove data columns that are not meaningful
ground_combined_1 <-
  list(
    g2002_df,
    g2003_df,
    g2004_df,
    g2007_df,
    g2009_df,
    g2012_df,
    g2015_df,
    g2025_df
  ) %>% 
  bind_rows() 

#Remove exclosure plots
#Remove all plots that start with 98 and are at least four characters/digits long.
#Can't just remove plots with "98" because plot 98 is not an exclosure plot.
ground_combined_2 <- 
  ground_combined_1 %>% 
  filter(
    !grepl("^98\\d{2}$", as.character(Plot)),
  )

#---------------------------------------------------------------------------------------------
# 7. Perform some basic QAQC checks
#---------------------------------------------------------------------------------------------

#write a function to visualize plot, position, and percent cover
#frequencies that can be applied to any year and be used to check
#for outliers and typos

ground_hist <- function(data, year_input) {
  
  # Filter the data for the specified year
  df_year <- data %>% filter(year == year_input)
  
  # Count occurrences of each category
  count_data <- as.data.frame(table(df_year$Quad))
  colnames(count_data) <- c("Quad", "Count")
  
  p1 <- ggplot(count_data, aes(x = Quad, y = Count, fill = Quad)) +
    geom_bar(stat = "identity", width = 0.6, fill = "steelblue") +  # stat="identity" uses precomputed counts
    geom_text(aes(label = Count), vjust = 1.5, size = 5) + 
    theme_minimal() +
    labs(
      title = paste("Quad Frequencies -", year_input),
      x = "Quad",
      y = "Count"
    )
  
  # Histogram of Plot
  p2 <- ggplot(df_year, aes(x = factor(Plot))) +  # Treat numeric Plot as categorical
    geom_bar(fill = "darkgreen") +
    theme_minimal() + 
    theme(
      axis.text.x = element_text(
        angle = 45,        # Rotation angle in degrees
        hjust = 1,         # Horizontal justification (1 = right aligned)
        vjust = 1,         # Vertical justification
        size = 6)) +     # X-axis label font size)
    labs(
      title = paste("Plot Frequencies -", year_input),
      x = "Plot",
      y = "Count"
    )
  
  # Histogram of Percent Litter Cover
  p3 <- ggplot(df_year[df_year$ground_type == "litter",], aes(x = cover_percent)) +
    geom_histogram(binwidth = 10, fill = "orange", color = "black") +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    theme_minimal() +
    labs(
      title = paste("Percent Litter Cover -", year_input),
      x = "Percent Cover",
      y = "Count"
    )

  # Histogram of Percent Mineral Soil Cover
  p4 <- ggplot(df_year[df_year$ground_type == "mineral",], 
               aes(x = cover_percent)) +
    geom_histogram(binwidth = 10, fill = "orange", color = "black") +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    theme_minimal() +
    labs(
      title = paste("Percent Mineral Soil Cover -", year_input),
      x = "Percent Cover",
      y = "Count"
    )

  # Histogram of Percent Canopy Cover
  p5 <- ggplot(df_year[df_year$ground_type == "moss",], aes(x = cover_percent)) +
    geom_histogram(binwidth = 10, fill = "orange", color = "black") +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    theme_minimal() +
    labs(
      title = paste("Percent Moss Cover -", year_input),
      x = "Percent Cover",
      y = "Count"
    )
  
  # Histogram of Percent Canopy Cover
  p6 <- ggplot(df_year[df_year$ground_type == "other",], aes(x = cover_percent)) +
    geom_histogram(binwidth = 10, fill = "orange", color = "black") +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    theme_minimal() +
    labs(
      title = paste("Percent Misc. Cover -", year_input),
      x = "Percent Cover",
      y = "Count"
    )
  
  # Return a named list of plots
  return(list(
    Quad = p1,
    Plot = p2,
    Percent_Litter_Cover = p3,
    Percent_Mineral_Soil_Cover = p4,
    Percent_Moss_Cover = p5,
    Percent_Misc_Cover = p6
    ))
}


#---------------------------------------------------------------------------------
#2002 check

qaqc_g2002 <- ground_hist(ground_combined_2,2002)

grid.arrange(
  qaqc_g2002$Quad, 
  qaqc_g2002$Plot, 
  qaqc_g2002$Percent_Litter_Cover, 
  qaqc_g2002$Percent_Mineral_Soil_Cover, 
  qaqc_g2002$Percent_Moss_Cover, 
  qaqc_g2002$Percent_Misc_Cover, 
  ncol = 2
)

#Cronan:
#No errors.

#---------------------------------------------------------------------------------
#2003 check

qaqc_g2003 <- ground_hist(ground_combined_2,2003)

grid.arrange(
  qaqc_g2003$Quad, 
  qaqc_g2003$Plot, 
  qaqc_g2003$Percent_Litter_Cover, 
  qaqc_g2003$Percent_Mineral_Soil_Cover, 
  qaqc_g2003$Percent_Moss_Cover, 
  qaqc_g2003$Percent_Misc_Cover, 
  ncol = 2
)

#Cronan:
#There is a very high value for litter cover
ground_combined_1[ground_combined_2$year == 2003 & 
                    ground_combined_2$ground_type == "litter" & 
                    ground_combined_2$cover_percent > 100,]
#Two values should be corrected.
#1 ----------------------------------------------------
ground_combined_1[ground_combined_2$year == 2003 & 
                    ground_combined_2$Plot == 32 & 
                    ground_combined_2$Quad == "EQ1",]
#Correct Plot 32 EQ1 from 199 to 99.5

#2 ----------------------------------------------------
ground_combined_1[ground_combined_2$year == 2003 & 
                    ground_combined_2$Plot == 69 & 
                    ground_combined_2$Quad == "SQ1",]
#Correct Plot 32 EQ1 from 966 to 96

#---------------------------------------------------------------------------------
#2004 check

qaqc_g2004 <- ground_hist(ground_combined_2,2004)

grid.arrange(
  qaqc_g2004$Quad, 
  qaqc_g2004$Plot, 
  qaqc_g2004$Percent_Litter_Cover, 
  qaqc_g2004$Percent_Mineral_Soil_Cover, 
  qaqc_g2004$Percent_Moss_Cover, 
  qaqc_g2004$Percent_Misc_Cover, 
  ncol = 2
)

#Cronan:
#No errors.

#---------------------------------------------------------------------------------
#2007 check

qaqc_g2007 <- ground_hist(ground_combined_2,2007)

grid.arrange(
  qaqc_g2007$Quad, 
  qaqc_g2007$Plot, 
  qaqc_g2007$Percent_Litter_Cover, 
  qaqc_g2007$Percent_Mineral_Soil_Cover, 
  qaqc_g2007$Percent_Moss_Cover, 
  qaqc_g2007$Percent_Misc_Cover, 
  ncol = 2
)

#Cronan:
#No errors.

#---------------------------------------------------------------------------------
#2009 check

qaqc_g2009 <- ground_hist(ground_combined_2,2009)

grid.arrange(
  qaqc_g2009$Quad, 
  qaqc_g2009$Plot, 
  qaqc_g2009$Percent_Litter_Cover, 
  qaqc_g2009$Percent_Mineral_Soil_Cover, 
  qaqc_g2009$Percent_Moss_Cover, 
  qaqc_g2009$Percent_Misc_Cover, 
  ncol = 2
)

#Cronan:
#No errors.

#---------------------------------------------------------------------------------
#2012 check

qaqc_g2012 <- ground_hist(ground_combined_2,2012)

grid.arrange(
  qaqc_g2012$Quad, 
  qaqc_g2012$Plot, 
  qaqc_g2012$Percent_Litter_Cover, 
  qaqc_g2012$Percent_Mineral_Soil_Cover, 
  qaqc_g2012$Percent_Moss_Cover, 
  qaqc_g2012$Percent_Misc_Cover, 
  ncol = 2
)

#Cronan:
#WQ1 is missing 4 entries.
#Plot 40 or 41 are missing entries.

length(ground_combined_2$Plot[ground_combined_2$year == 2012 & 
                    ground_combined_2$Plot == 40])
#Has the correct number of readings.
length(ground_combined_2$Plot[ground_combined_2$year == 2012 & 
                    ground_combined_2$Plot == 41])
#Plot 41 is missing readings for WQ1

#---------------------------------------------------------------------------------
#2015 check

qaqc_g2015 <- ground_hist(ground_combined_2,2015)

grid.arrange(
  qaqc_g2015$Quad, 
  qaqc_g2015$Plot, 
  qaqc_g2015$Percent_Litter_Cover, 
  qaqc_g2015$Percent_Mineral_Soil_Cover, 
  qaqc_g2015$Percent_Moss_Cover, 
  qaqc_g2015$Percent_Misc_Cover, 
  ncol = 2
)

#Cronan:
#There are readings with very low values for litter cover
ground_combined_2[ground_combined_2$year == 2015 & 
                    ground_combined_2$ground_type == "litter" & 
                    ground_combined_2$cover_percent < 0,]
#Four values with -999. They are either NA or incorrect values.

#1 ----------------------------------------------------
ground_combined_2[ground_combined_2$year == 2015 & 
                    ground_combined_2$Plot == 33 & 
                    ground_combined_2$Quad == "EQ2",]
#Correct Plot 33 EQ2 from -999 to 98.5

#2 ----------------------------------------------------
ground_combined_2[ground_combined_2$year == 2015 & 
                    ground_combined_2$Plot == 10 & 
                    ground_combined_2$Quad == "EQ2",]
#Correct Plot 10 EQ2 from -999 to 93

#3 ----------------------------------------------------
ground_combined_2[ground_combined_2$year == 2015 & 
                    ground_combined_2$Plot == 75 & 
                    ground_combined_2$Quad == "WQ2",]
#Correct Plot 75 WQ2 from -999 to 96

#4 ----------------------------------------------------
ground_combined_2[ground_combined_2$year == 2015 & 
                    ground_combined_2$Plot == 102 & 
                    ground_combined_2$Quad == "WQ2",]
#Not possible to correct, multiple negative values.
#Change to NA


#There is a very low values for mineral soil cover
ground_combined_1[ground_combined_2$year == 2015 & 
                    ground_combined_2$ground_type == "mineral" & 
                    ground_combined_2$cover_percent < 0,]
#Single value should be corrected.

#1 ----------------------------------------------------
ground_combined_2[ground_combined_2$year == 2015 & 
                    ground_combined_2$Plot == 102 & 
                    ground_combined_2$Quad == "WQ2",]
#Not possible to correct, multiple negative values.
#Change to NA

#---------------------------------------------------------------------------------
#2025 check

qaqc_g2025 <- ground_hist(ground_combined_2,2025)

grid.arrange(
  qaqc_g2025$Quad, 
  qaqc_g2025$Plot, 
  qaqc_g2025$Percent_Litter_Cover, 
  qaqc_g2025$Percent_Mineral_Soil_Cover, 
  qaqc_g2025$Percent_Moss_Cover, 
  qaqc_g2025$Percent_Misc_Cover, 
  ncol = 2
)

#Cronan:
#No errors.

##################################################################################
##################################################################################
##################################################################################
#                                    CORRECTIONS

##################################################################################
#1
#New data frame with corrections
gc3 <- ground_combined_2

##################################################################################
#2
#Correct Plot 32 EQ1 litter cover from 199 to 99.5
gc3$cover_percent[gc3$year == 2003 & 
                    gc3$Plot == 32 & 
                    gc3$Quad == "EQ1" & 
                    gc3$ground_type == "litter"] <- 99.5

#Correct Plot 69 SQ1 litter cover from 966 to 96
gc3$cover_percent[gc3$year == 2003 & 
                    gc3$Plot == 69 & 
                    gc3$Quad == "SQ1" & 
                    gc3$ground_type == "litter"] <- 96.0

##################################################################################
#3
#Add rows for missing data in plot 41, WQ1 in 2012.
add_1 <- data.frame(Plot = as.integer(rep(41, 4)),
                    Quad = rep("WQ1", 4), 
                    ground_type = c("litter", "mineral", "moss", "other"), 
                    cover_percent = rep(NA, 4), 
                    year = rep("2012", 4))
gc4 <- rbind(gc3, add_1)

##################################################################################
#4

#Correct Plot 33 EQ2 from -999 to 98.5
gc4$cover_percent[gc4$year == 2015 & 
                    gc4$Plot == 33 & 
                    gc4$Quad == "EQ2" & 
                    gc4$ground_type == "litter"] <- 98.5

#Correct Plot 10 EQ2 from -999 to 98.5
gc4$cover_percent[gc4$year == 2015 & 
                    gc4$Plot == 10 & 
                    gc4$Quad == "EQ2" & 
                    gc4$ground_type == "litter"] <- 93.0

#Correct Plot 75 EQ2 from -999 to 93
gc4$cover_percent[gc4$year == 2015 & 
                    gc4$Plot == 75 & 
                    gc4$Quad == "WQ2" & 
                    gc4$ground_type == "litter"] <- 96.0

#Change to NA
gc4$cover_percent[gc4$year == 2015 & 
                    gc4$Plot == 102 & 
                    gc4$Quad == "WQ2" & 
                    gc4$ground_type == "litter"] <- NA

#Change to NA
gc4$cover_percent[gc4$year == 2015 & 
                    gc4$Plot == 102 & 
                    gc4$Quad == "WQ2" & 
                    gc4$ground_type == "mineral"] <- NA

#-------------------------------------------------------------------------------------
# 6. Save corrected data
#---------------------------------------------------------------------------------------------
#This is corrected data.
#No data has been removed.

#Set working directory to clean data folder.
setwd(paste(user_paths_saved_data[current_user], sep = ""))

#Set date and time.
dt <- Sys.Date()
clean_dt <- gsub("-", "", dt, fixed = T)

write_csv(gc4, paste(clean_dt, "_burn_severity_ground_clean.csv", sep = ""))

#---------------------------------------------------------------------------------------------
# End
#---------------------------------------------------------------------------------------------
