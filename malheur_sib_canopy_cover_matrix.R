#############################################################################################
# Canopy Cover Matrix
# Author: Jim Cronan, Emily Sanders, Maggie Koontz
# Purpose: Collate and format raw canopy cover data containing independent variables 
# on burn severity into a single matrix with variables arranged in columns and sites
# arranged in rows.
#############################################################################################

library(dplyr)
library(gridExtra) #to display multiple histograms at once
library(tidyverse) #needed for pipe function (%>%)
#---------------------------------------------------------------------------------------------
# 1. Load data
#---------------------------------------------------------------------------------------------

#Incoming data
# Map usernames to file paths
user_paths_canopy <- c(
  Nate   = "C:/Users/NathanWade/Box/SIB/Cronan/Wade/3_Data/01_Raw_Data/Severity_indices/Canopy",
  Becky = "",
  jcronan = "C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/Severity_indices/Canopy",
  esande02 = "",
  mak600 = "C://Users//mak600//Documents//Malheur//Canopy Data"
)

#Lookup Tables
# Map usernames to file paths
user_paths_lut <- c(
  Nate   = "C:/Users/NathanWade/Box/SIB/Cronan/Wade/3_Data/01_Raw_Data/Severity_indices/Canopy/",
  Becky     = "",
  jcronan = "C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/Severity_indices/Canopy/",
  esande02 = "C:/Users/esande02/Downloads/FERA/Malheur/burn_severity/lut_burn_severity_file_names.csv",
  mak600 = "C://Users//mak600//Documents//Malheur//Canopy Data//canopy_file_lut.csv")

# Outgoing (saved) data
user_paths_saved_data <- c(
  Nate   = "C:/Users/NathanWade/Box/SIB/Cronan/Wade/3_Data/02_Clean_Data/Severity_indices/Canopy/",
  Becky = "",
  jcronan = "C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/02_Clean_Data/Severity_indices/Canopy",
  esande02 = "",
  mak600 = ""
)

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

# Check if user exists in mapping for saved files
if (!current_user %in% names(user_paths_saved_data)) {
  stop("No file path configured for this user: ", current_user)
}

#Load data
#Load lut
canopy_lut <- read.csv(paste(user_paths_lut[current_user], "canopy_file_lut.csv", sep = ""))

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

View(canopy_list$c2002) #percent CC present, sampled at center, 5 and 15 m
View(canopy_list$c2003) #percent CC present, sampled at center, 5 and 15 m
View(canopy_list$c2004) #has prop CC- need to multiply by 100, sampled at center, 5 and 15 m
View(canopy_list$c2007) #wide format, where each row is a plot which is sampled at center, 5 and 15 m,
                        #need to covert to verticle format and multiply values by 4 to include percent cc
View(canopy_list$c2009) #same as 2007
View(canopy_list$c2012) #wide format same as 2007, but measured at 5, 10, and 15 m
                        #will transfer this over to full matrix
View(canopy_list$c2015) #same as 2012 but with 8 cardinal directions
View(canopy_list$c2025) #wide format, but slightly different than other wide formats

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
    names_to = "Position", 
    values_to  = "canopy_cover")%>%
  mutate("percent_canopy_cover" = canopy_cover *4) %>%
  mutate(year = "2007")
c2007_standard_df <- as.data.frame(c2007_standard)  

#2009
#convert to long format and calculate percent canopy cover
c2009_standard <- canopy_list$c2009 %>%
  pivot_longer(
    cols = -c(Plot, Comment), #copies over comments for each sample per plot
    names_to = "Position", 
    values_to  = "canopy_cover")%>%
  mutate("percent_canopy_cover" = canopy_cover *4)%>%
  mutate(year = "2009")
c2009_standard_df <- as.data.frame(c2009_standard)  

#2012
#convert to long format and calculate percent canopy cover
c2012_standard <- canopy_list$c2012 %>%
  pivot_longer(
    cols = -c(Plot, Comment, Date), #copies over comments and dates for each sample per plot
    names_to = "Position", 
    values_to  = "canopy_cover")%>%
  mutate(Comment = as.character(Comment)) %>% 
  mutate("percent_canopy_cover" = canopy_cover *4)%>%
  mutate(year = "2012") %>%
  select(-"Date")
c2012_standard_df <- as.data.frame(c2012_standard)  
c2012_standard_df$Comment[is.na(c2012_standard_df$Comment)] <- ""

#2015
#convert to long format and calculate percent canopy cover
c2015_standard <- canopy_list$c2015 %>%
  pivot_longer(
    cols = -c(Plot, Date, Comment, Initials), #copies over comments, dates, and initials for each sample per plot
    names_to = "Position", 
    values_to  = "canopy_cover")%>%
  mutate("percent_canopy_cover" = canopy_cover *4) %>%
  mutate(year = "2015") %>%
  select(-"Date")
c2015_standard_df <- as.data.frame(c2015_standard)  
c2015_standard_df$Comment[is.na(c2015_standard_df$Comment)] <- ""

#2025
#convert to long format and calculate percent canopy cover
c2025_standard <- canopy_list$c2025 %>%
  pivot_longer(
    cols = -c(Plot, Year, Stand, Treatment, Notes), #copies over comments, dates, and initials for each sample per plot
    names_to = "Position", 
    values_to  = "canopy_cover")%>%
  mutate("percent_canopy_cover" = canopy_cover *4) %>%
  select(-c("Year", "Stand", "Treatment"))%>% #this information is not needed for combined matrix
  mutate(year = "2025")
c2025_standard_df <- as.data.frame(c2025_standard)  

#---------------------------------------------------------------------------------------------
# 4. Aggregate into a single, vertical, cleaned dataset
#---------------------------------------------------------------------------------------------

#Combine all canopy files into a single table.
#Merge comments and notes,
#Remove data columns that are not meaningful
canopy_combined_1 <-
  list(
      c2002_standard,
      c2003_standard,
      c2004_standard,
      c2007_standard_df,
      c2009_standard_df,
      c2012_standard_df,
      c2015_standard_df,
      c2025_standard_df
    ) %>%
  bind_rows() %>%
  unite("comments/notes", Comment, Notes) %>%
  select(                  
    -c("Exc", 
       "C_cover",
       "propCC",
       "canopy_cover",
       "Initials"))

#Remove "NA_NA", "_NA", and "NA_" assignments in notes for blank entries.
canopy_combined_1$`comments/notes`[canopy_combined_1$`comments/notes` == "NA_NA"] <- ""
canopy_combined_1$`comments/notes`[canopy_combined_1$`comments/notes` == "_NA"] <- ""
canopy_combined_1$`comments/notes`[canopy_combined_1$`comments/notes` == "NA_"] <- ""

#Remove exclosure plots
#Remove all plots that start with 98 and are at least four characters/digits long.
#Can't just remove plots with "98" because plot 98 is not an exclosure plot.
canopy_combined_2 <- 
  canopy_combined_1 %>% 
  filter(
  !grepl("^98\\d{2}$", as.character(Plot)),
)

#---------------------------------------------------------------------------------------------
# 5. Perform some basic QAQC checks
#---------------------------------------------------------------------------------------------

#write a function to visualize plot, position, and percent cover
#frequencies that can be applied to any year and be used to check
#for outliers and typos

canopy_hist <- function(data, year_input) {
  
  # Filter the data for the specified year
  df_year <- data %>% filter(year == year_input)
  
  # Count occurrences of each category
  count_data <- as.data.frame(table(df_year$Position))
  colnames(count_data) <- c("Position", "Count")
  
  p1 <- ggplot(count_data, aes(x = Position, y = Count, fill = Position)) +
    geom_bar(stat = "identity", width = 0.6, fill = "steelblue") +  # stat="identity" uses precomputed counts
    geom_text(aes(label = Count), vjust = 1.5, size = 5) + 
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


#---------------------------------------------------------------------------------
#2002 check

qaqc_c2002 <- canopy_hist(canopy_combined_2,2002)

grid.arrange(
  qaqc_c2002$Position, 
  qaqc_c2002$Plot, 
  qaqc_c2002$Percent_Canopy_Cover,  
  ncol = 1
)

#Koontz:
#same number of counts across all positions- this is good

#almost all plots had 9 samples (at 5 and 15m points for each
#cardinal direction, plus one at center)
#although, there are two plots that have a different count which
#should be checked for notes

#all data between 0-100. bimodal frequency at 
#0 and 100, makes sense that a disproportional
#amount of plots would be counted as having no 
#or all quadrants with data in moosehorn due to
#human tendency

#Cronan - I think Maggie is correct about samples/plot. Seems that some 107 readings
#were accidentally labelled as plot 106. Emailed Nat to ask on 3/27/2026.
#Nat agreed, the extra points in plot 106 should be re-labeled as plot 107.
canopy_combined_2[canopy_combined_2$year == 2002 & canopy_combined_2$Plot == 106,]
canopy_combined_2[canopy_combined_2$year == 2002 & canopy_combined_2$Plot == 107,]

#---------------------------------------------------------------------------------
#2003 check

qaqc_c2003 <- canopy_hist(canopy_combined_2,2003)

grid.arrange(
  qaqc_c2003$Position,
  qaqc_c2003$Plot,
  qaqc_c2003$Percent_Canopy_Cover,
  ncol = 1
)
#Cronan - no errors.

#---------------------------------------------------------------------------------
#2004 check

qaqc_c2004 <- canopy_hist(canopy_combined_2,2004)

grid.arrange(
  qaqc_c2004$Position,
  qaqc_c2004$Plot,
  qaqc_c2004$Percent_Canopy_Cover,
  ncol = 1
)

#Cronan - no errors.

#---------------------------------------------------------------------------------
#2007 check

qaqc_c2007 <- canopy_hist(canopy_combined_2,2007)

grid.arrange(
  qaqc_c2007$Position,
  qaqc_c2007$Plot,
  qaqc_c2007$Percent_Canopy_Cover,
  ncol = 1
)

#Cronan - no errors.

#---------------------------------------------------------------------------------
#2009 check

qaqc_c2009 <- canopy_hist(canopy_combined_2,2009)

grid.arrange(
  qaqc_c2009$Position,
  qaqc_c2009$Plot,
  qaqc_c2009$Percent_Canopy_Cover,
  ncol = 1
)

#Cronan - no errors.

#---------------------------------------------------------------------------------
#2012 check

qaqc_c2012 <- canopy_hist(canopy_combined_2,2012)

grid.arrange(
  qaqc_c2012$Position,
  qaqc_c2012$Plot,
  qaqc_c2012$Percent_Canopy_Cover,
  ncol = 1
)

#Cronan - no errors.

#---------------------------------------------------------------------------------
#2015 check

qaqc_c2015 <- canopy_hist(canopy_combined_2,2015)

grid.arrange(
  qaqc_c2015$Position,
  qaqc_c2015$Plot,
  qaqc_c2015$Percent_Canopy_Cover,
  ncol = 1
)

#Cronan - one of the canopy cover values is -4000
canopy_combined_2[which(canopy_combined_2$percent_canopy_cover < 0),]
#Plot: 42
#Position: C
#Year: 2015
#Percent canopy cover: -3996

#---------------------------------------------------------------------------------
#2025 check

qaqc_c2025 <- canopy_hist(canopy_combined_2,2025)

grid.arrange(
  qaqc_c2025$Position,
  qaqc_c2025$Plot,
  qaqc_c2025$Percent_Canopy_Cover,
  ncol = 1
)
#Cronan - no errors.

##################################################################################
##################################################################################
##################################################################################
#                                    CORRECTIONS

##################################################################################
#1
#New data frame with corrections
cc3 <- canopy_combined_2

##################################################################################
#2
#Assign mislabeled canopy readings in plot 106 to plot 107
cc3$Plot[478] <- 107 #formerly plot 106 second reading with "C" position
cc3$Plot[479] <- 107 #formerly plot 106 second reading with "N_5" position

#Assign NA value to canopy cover reading of -3996 in plot 42 | position C | 2015
cc3$percent_canopy_cover[5983] <- NA

##################################################################################
#3
#Round percent canopy cover readings to the nearest whole number
cc3$percent_canopy_cover <- round(cc3$percent_canopy_cover,0)

##################################################################################
#4
#A number of rows
cc3$percent_canopy_cover <- round(cc3$percent_canopy_cover,0)

##################################################################################
#5
#Not all positions are labelled consistently
sort(unique(cc3$Position))

#Change 'Central' to 'C'
cc3$Position[cc3$Position == "Center"] <- "C"

#Change 'E15' to 'E_15'
cc3$Position[cc3$Position == "E15"] <- "E_15"

#Change 'E5' to 'E_5'
cc3$Position[cc3$Position == "E5"] <- "E_5"

#Change 'N15' to 'N_15'
cc3$Position[cc3$Position == "N15"] <- "N_15"

#Change 'N5' to 'N_5'
cc3$Position[cc3$Position == "N5"] <- "N_5"

#Change 'S15' to 'S_15'
cc3$Position[cc3$Position == "S15"] <- "S_15"

#Change 'S5' to 'S_5'
cc3$Position[cc3$Position == "S5"] <- "S_5"

#Change 'W15' to 'W_15'
cc3$Position[cc3$Position == "W15"] <- "W_15"

#Change 'W5' to 'W_5'
cc3$Position[cc3$Position == "W5"] <- "W_5"

#Recheck positions
sort(unique(cc3$Position))


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

write_csv(cc3, paste(clean_dt, "_burn_severity_canopy_clean.csv", sep = ""))

#---------------------------------------------------------------------------------------------
# End
#---------------------------------------------------------------------------------------------

