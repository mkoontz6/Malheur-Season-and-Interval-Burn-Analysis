#############################################################################################
# Canopy Cover Matrix
# Author: Jim Cronan, Emily Sanders, Maggie Koontz
# Purpose: Collate and format raw canopy cover data containing independent variables 
# on burn severity into a single matrix with variables arranged in columns and sites
# arranged in rows.
#############################################################################################

library(dplyr)

library(gridExtra)
install.packages("gridExtra") #to display multiple histograms at once

#---------------------------------------------------------------------------------------------
# 1. Load data
#---------------------------------------------------------------------------------------------

#Data
# Map usernames to file paths
user_paths_canopy <- c(
  Nat   = "",
  Becky = "",
  jcronan = "",
  esande02 = "",
  mak600 = "C:\\Users\\mak600\\Documents\\Malheur\\Canopy Data"
)

#Lookup Tables
# Map usernames to file paths
user_paths_lut <- c(
  Nat   = "",
  Becky     = "",
  jcronan = "",
  esande02 = "C:/Users/esande02/Downloads/FERA/Malheur/burn_severity/lut_burn_severity_file_names.csv",
  mak600 = "C:\\Users\\mak600\\Documents\\Malheur\\Canopy Data\\canopy_file_lut.csv")


# Detect current user
current_user <- Sys.info()[["user"]]

# Check if user exists in mapping for data files
if (!current_user %in% names(user_paths_canopy)) {
  stop("No file path configured for t
       his user: ", current_user)
}

# Check if user exists in mapping for lut files
if (!current_user %in% names(user_paths_lut)) {
  stop("No file path configured for this user: ", current_user)
}


#Load data
#Load lut
canopy_lut <- read.csv(paste(user_paths_lut[current_user]))

#Load canopy data

canopy_list <- list()

for (i in seq_along(canopy_lut$file_name_year)) {
  
  year <- canopy_lut$file_name_year[i]
  
  file_path <- file.path(
    user_paths_canopy[current_user],
    paste0("Canopy_", year, ".csv")
  )
  
  canopy_list[[paste0("c", year)]] <- read.csv(file_path)
}


#---------------------------------------------------------------------------------------------
# 2. Exploring data structure variation
#---------------------------------------------------------------------------------------------

view(canopy_list$c2002) #percent CC present, sampled at center, 5 and 15 m
view(canopy_list$c2003) #percent CC present, sampled at center, 5 and 15 m
view(canopy_list$c2004) #has prop CC- need to multiply by 100, sampled at center, 5 and 15 m
view(canopy_list$c2007) #wide format, where each row is a plot which is sampled at center, 5 and 15 m,
                        #need to covert to verticle format and multiply values by 4 to include percent cc
view(canopy_list$c2009) #same as 2007
view(canopy_list$c2012) #wide format same as 2007, but measured at 5, 10, and 15 m
                        #will transfer this over to full matrix
view(canopy_list$c2015) #same as 2012 but with 8 cardinal directions
view(canopy_list$c2025) #wide format, but slightly different than other wide formats

#all include enclosure plots. In 2002-2004 data this is notated by an exclosure
#column scored as a binary variable as wel as the plot number starting with 98
#2007 and on data  enclosure plots are not marked with a column and start with 98.

#---------------------------------------------------------------------------------------------
# 3. Standardizing each yearly dataset
#---------------------------------------------------------------------------------------------
#2002
#add year column and rename PerCC to percent canopy cover
c2002_standard <- canopy_list$c2002 %>%
  rename(percent_canopy_cover = PerCC) %>%
  mutate(year = "2002")

#2003
#add year column and rename PerCC to percent canopy cover
c2003_standard <- canopy_list$c2003 %>%
  rename(percent_canopy_cover = PerCC) %>%
  mutate(year = "2003")

#2004
#add a Percent Canopy Cover column (PerCC) by multiplying preexisting propCC column by 100
c2004_standard <- canopy_list$c2004 %>%
  mutate("percent_canopy_cover" = propCC * 100) %>%
  mutate(year = "2004")

#2007
#convert to long format and calculate percent canopy cover
c2007_standard <- canopy_list$c2007 %>%
  pivot_longer(
    cols = -c(Plot, Comment), #copies over comments for each sample per plot
    names_to = "sample", 
    values_to  = "canopy_cover")%>%
  mutate("percent_canopy_cover" = canopy_cover *4) %>%
  mutate(year = "2007")
  
#2009
#convert to long format and calculate percent canopy cover
c2009_standard <- canopy_list$c2009 %>%
  pivot_longer(
    cols = -c(Plot, Comment), #copies over comments for each sample per plot
    names_to = "sample", 
    values_to  = "canopy_cover")%>%
  mutate("percent_canopy_cover" = canopy_cover *4)%>%
  mutate(year = "2009")

#2012
#convert to long format and calculate percent canopy cover
c2012_standard <- canopy_list$c2012 %>%
  pivot_longer(
    cols = -c(Plot, Comment, Date), #copies over comments and dates for each sample per plot
    names_to = "sample", 
    values_to  = "canopy_cover")%>%
  mutate("percent_canopy_cover" = canopy_cover *4)%>%
  mutate(year = "2012") %>%
  select(-"Date")


#2015
#convert to long format and calculate percent canopy cover
c2015_standard <- canopy_list$c2015 %>%
  pivot_longer(
    cols = -c(Plot, Date, Initials), #copies over comments, dates, and initials for each sample per plot
    names_to = "sample", 
    values_to  = "canopy_cover")%>%
  mutate("percent_canopy_cover" = canopy_cover *4) %>%
  mutate(year = "2015") %>%
  select(-"Date")

#2025
#convert to long format and calculate percent canopy cover
c2025_standard <- canopy_list$c2025 %>%
  pivot_longer(
    cols = -c(Plot, Year, Stand, Treatment, Notes), #copies over comments, dates, and initials for each sample per plot
    names_to = "sample", 
    values_to  = "canopy_cover")%>%
  mutate("percent_canopy_cover" = canopy_cover *4) %>%
  select(-c("Year", "Stand", "Treatment"))%>% #this information is not needed for combined matrix
  mutate(year = "2025")

#---------------------------------------------------------------------------------------------
# 4. Aggregate into a single, vertical, cleaned dataset
#---------------------------------------------------------------------------------------------

canopy_combined <-
  list(
      c2002_standard,
      c2003_standard,
      c2004_standard,
      c2007_standard,
      c2009_standard,
      c2012_standard,
      c2015_standard,
      c2025_standard
    ) %>%
  bind_rows() %>%
  unite(
    "comments/notes", Comment, Notes) %>% #merge comments and notes
  filter(                  #exclude exclosure plots
    !grepl("^98", Plot),   # exclude Plot starting with 98
    Exc != 1               # exclude rows where exc == 1
  ) %>%
  select(                  #Remove columns that are not meaningful
    -c("propCC",
       "sample",
       "canopy_cover",
       "Initials"))

#---------------------------------------------------------------------------------------------
# 5. Perform some basic QAQC checks
#---------------------------------------------------------------------------------------------

#write a function to visualize plot, position, and percent cover
#frequencies that can be applied to any year and be used to check
#for outliers and typos

canopy_hist <- function(data, year_input) {
  
  # Filter the data for the specified year
  df_year <- data %>% filter(year == year_input)
  
  # Histogram of Position
  p1 <- ggplot(df_year, aes(x = Position)) +
    geom_bar(fill = "steelblue") +
    theme_minimal() +
    labs(
      title = paste("Position Frequencies -", year_input),
      x = "Position",
      y = "Count"
    )
  
  # Histogram of Plot
  p2 <- ggplot(df_year, aes(x = factor(Plot))) +  # Treat numeric Plot as categorical
    geom_bar(fill = "darkgreen") +
    theme_minimal() +
    labs(
      title = paste("Plot Frequencies -", year_input),
      x = "Plot",
      y = "Count"
    )
  
  # Histogram of Percent Canopy Cover
  p3 <- ggplot(df_year, aes(x = percent_canopy_cover)) +
    geom_histogram(binwidth = 10, fill = "orange", color = "black") +
    theme_minimal() +
    labs(
      title = paste("Percent Canopy Cover -", year_input),
      x = "Percent Canopy Cover",
      y = "Count"
    )
  
  # Return a named list of plots
  return(list(
    Position = p1,
    Plot = p2,
    Percent_Canopy_Cover = p3
  ))
}

#2002 check

qaqc_c2002 <- canopy_hist(canopy_combined,2002)

grid.arrange(
  qaqc_c2002$Position, #same number of counts across all positions- this is good
                       #might want a different way to check because count is so
                       #so high a variance may not be visible by just scanning the histogram
  qaqc_c2002$Plot, #almost all plots had 9 samples (at 5 and 15m points for each
                   #cardinal direction, plus one at center)
                   #although, there are two plots that have a different count which
                   #should be checked for notes
  qaqc_c2002$Percent_Canopy_Cover, #all data between 0-100. bimodal frequency at 
                                    #0 and 100, makes sense that a disproportional
                                    #amount of plots would be counted as having no 
                                    #or all quadrants with data in moosehorn due to
                                    #human tendency 
  ncol = 1
)

#--------------MK ended here 3/18-----------#
#next continue on qaqc and talk with Jim to see if data is formatted correctly
canopy_combined %>%
  filter(year=2002)%>%
  filter()

