#This script takes the cleaned output for the dependent variables (fuel loading, vegetation cover from quadrats, shrub cover from plots, and the O-horizon depths) and runs summary statistics on them to make sure nothing is out of place.

#March 12, 2026
#Nathan Wade

library(tidyverse)

#importing data
input <- "C:/Users/NathanWade/Box/SIB/Cronan Wade/3_Data/02_Clean_Data/"

#importing data
fuelsQuad <- read.csv(paste0(input, "Fuels/Fuels_direction.csv"))
odepthQuad <- read.csv(paste0(input, "O_horizon_depth/ODepth_quad.csv"))
vegQuad <- read.csv(paste0(input, "Vegetation/Vegetation_quad.csv"))
shrubPlot <- read.csv(paste0(input, "Shrubs/Shrub_plot.csv"))

#defining column types
fuelsQuad <- fuelsQuad %>% mutate(Year = as.factor(Year), 
                                  Plot = as.factor(Plot),
                                  Stand = as.factor(Stand),
                                  Direction = as.factor(Direction),
                                  Treatment = as.factor(Treatment))

odepthQuad <- odepthQuad %>% mutate(Year = as.factor(Year), 
                                    Plot = as.factor(Plot),
                                    Stand = as.factor(Stand),
                                    Quad = as.factor(Quad),
                                    Treatment = as.factor(Treatment))

vegQuad <- vegQuad %>% mutate(Yr = as.factor(Yr), 
                              Plot = as.factor(Plot),
                              Stand = as.factor(Stand),
                              Quad = as.factor(Quad),
                              Treatment = as.factor(Treatment))

shrubPlot <- shrubPlot %>% mutate(Year = as.factor(Year), 
                                  Plot = as.factor(Plot),
                                  Stand = as.factor(Stand),
                                  Treatment = as.factor(Treatment))


#running summary statistics####
#fuels data####
###one-hour fuels
fuelsQuad %>% group_by(Year) %>%
  summarise(meanHrone = mean(hrone, na.rm = TRUE),
            medianHrone = median(hrone, na.rm = TRUE),
            minHrone = min(hrone, na.rm = TRUE),
            maxHrone = max(hrone, na.rm = TRUE),
            rangeHrone = max(hrone, na.rm = TRUE) - min(hrone, na.rm = TRUE),
            sdHrone = sd(hrone, na.rm = TRUE),
            n = n())

###ten-hour fuels
fuelsQuad %>% group_by(Year) %>%
  summarise(meanHrten = mean(hrten, na.rm = TRUE),
            medianHrten = median(hrten, na.rm = TRUE),
            minHrten = min(hrten, na.rm = TRUE),
            maxHrten = max(hrten, na.rm = TRUE),
            rangeHrten = max(hrten, na.rm = TRUE) - min(hrten, na.rm = TRUE),
            sdHrten = sd(hrten, na.rm = TRUE),
            n = n())

###hundred-hour fuels
fuelsQuad %>% group_by(Year) %>%
  summarise(meanHrhun = mean(hrhun, na.rm = TRUE),
            medianHrhun = median(hrhun, na.rm = TRUE),
            minHrhun = min(hrhun, na.rm = TRUE),
            maxHrhun = max(hrhun, na.rm = TRUE),
            rangeHrhun = max(hrhun, na.rm = TRUE) - min(hrhun, na.rm = TRUE),
            sdHrhun = sd(hrhun, na.rm = TRUE),
            n = n())

###thousand-hour fuels
fuelsQuad %>% group_by(Year) %>%
  summarise(meanHrthou = mean(hrthou, na.rm = TRUE),
            medianHrthou = median(hrthou, na.rm = TRUE),
            minHrthou = min(hrthou, na.rm = TRUE),
            maxHrthou = max(hrthou, na.rm = TRUE),
            rangeHrthou = max(hrthou, na.rm = TRUE) - min(hrthou, na.rm = TRUE),
            sdHrthou = sd(hrthou, na.rm = TRUE),
            n = n())

##o-horizon depth (Brown's transect)
fuelsQuad %>% group_by(Year) %>%
  summarise(meanLandd = mean(landd, na.rm = TRUE),
            medianLandd = median(landd, na.rm = TRUE),
            minLandd = min(landd, na.rm = TRUE),
            maxLandd = max(landd, na.rm = TRUE),
            rangeLandd = max(landd, na.rm = TRUE) - min(landd, na.rm = TRUE),
            sdLandd = sd(landd, na.rm = TRUE),
            n = n())


##histograms
###one-hour fuels
(ggplot(fuelsQuad, aes(x = hrone)) +
    geom_histogram(bins = 30) +
    facet_wrap(~Year) +
    theme_bw())

###ten-hour fuels
(ggplot(fuelsQuad, aes(x = hrten)) +
    geom_histogram(bins = 30) +
    facet_wrap(~Year) +
    theme_bw())

###hundred-hour fuels
(ggplot(fuelsQuad, aes(x = hrhun)) +
    geom_histogram(bins = 30) +
    facet_wrap(~Year) +
    theme_bw())

###thousand-hour fuels
(ggplot(fuelsQuad, aes(x = hrthou)) +
    geom_histogram(bins = 30) +
    facet_wrap(~Year) +
    theme_bw())

###litter and duff depths
(ggplot(fuelsQuad, aes(x = landd)) +
    geom_histogram(bins = 30) +
    facet_wrap(~Year) +
    theme_bw())


#quadrat o-horizon depth####
odepthQuad %>% group_by(Year) %>%
  summarise(meanO_Depth = mean(O_Depth, na.rm = TRUE),
            medianO_Depth = median(O_Depth, na.rm = TRUE),
            minO_Depth = min(O_Depth, na.rm = TRUE),
            maxO_Depth = max(O_Depth, na.rm = TRUE),
            rangeO_Depth = max(O_Depth, na.rm = TRUE) - min(O_Depth, na.rm = TRUE),
            sdO_Depth = sd(O_Depth, na.rm = TRUE),
            n = n())

#o-horizon histogram
(ggplot(odepthQuad, aes(x = O_Depth)) +
    geom_histogram(bins = 30) +
    facet_wrap(~Year) +
    theme_bw())


#quadrat vegetation cover####
##cheatgrass
vegQuad %>% filter(Species == "BRTE") %>% group_by(Yr) %>%
  summarise(meanCover = mean(Cover, na.rm = TRUE),
            medianCover = median(Cover, na.rm = TRUE),
            minCover = min(Cover, na.rm = TRUE),
            maxCover = max(Cover, na.rm = TRUE),
            rangeCover = max(Cover, na.rm = TRUE) - min(Cover, na.rm = TRUE),
            sdCover = sd(Cover, na.rm = TRUE),
            n = n())

##snowbrush
vegQuad %>% filter(Species == "CEVE") %>% group_by(Yr) %>%
  summarise(meanCover = mean(Cover, na.rm = TRUE),
            medianCover = median(Cover, na.rm = TRUE),
            minCover = min(Cover, na.rm = TRUE),
            maxCover = max(Cover, na.rm = TRUE),
            rangeCover = max(Cover, na.rm = TRUE) - min(Cover, na.rm = TRUE),
            sdCover = sd(Cover, na.rm = TRUE),
            n = n())

##rabbitbrush
vegQuad %>% filter(Species == "RABBIT") %>% group_by(Yr) %>%
  summarise(meanCover = mean(Cover, na.rm = TRUE),
            medianCover = median(Cover, na.rm = TRUE),
            minCover = min(Cover, na.rm = TRUE),
            maxCover = max(Cover, na.rm = TRUE),
            rangeCover = max(Cover, na.rm = TRUE) - min(Cover, na.rm = TRUE),
            sdCover = sd(Cover, na.rm = TRUE),
            n = n())


#histograms
##cheatgrass
(ggplot(vegQuad %>% filter(Species == "BRTE"), aes(x = Cover)) +
    geom_histogram(bins = 30) +
    facet_wrap(~Yr) +
    theme_bw())

##snowbrush
(ggplot(vegQuad %>% filter(Species == "CEVE"), aes(x = Cover)) +
    geom_histogram(bins = 30) +
    facet_wrap(~Yr) +
    theme_bw())

##rabbitbrush
(ggplot(vegQuad %>% filter(Species == "RABBIT"), aes(x = Cover)) +
    geom_histogram(bins = 30) +
    facet_wrap(~Yr) +
    theme_bw())


#plot shrub cover####
##snowbrush
shrubPlot %>% filter(Code == "CEVE") %>% group_by(Year) %>%
  summarise(meanCover = mean(Cover, na.rm = TRUE),
            medianCover = median(Cover, na.rm = TRUE),
            minCover = min(Cover, na.rm = TRUE),
            maxCover = max(Cover, na.rm = TRUE),
            rangeCover = max(Cover, na.rm = TRUE) - min(Cover, na.rm = TRUE),
            sdCover = sd(Cover, na.rm = TRUE),
            n = n())

##rabbitbrush
shrubPlot %>% filter(Code == "RABBIT") %>% group_by(Year) %>%
  summarise(meanCover = mean(Cover, na.rm = TRUE),
            medianCover = median(Cover, na.rm = TRUE),
            minCover = min(Cover, na.rm = TRUE),
            maxCover = max(Cover, na.rm = TRUE),
            rangeCover = max(Cover, na.rm = TRUE) - min(Cover, na.rm = TRUE),
            sdCover = sd(Cover, na.rm = TRUE),
            n = n())


#histograms
##snowbrush
(ggplot(shrubPlot %>% filter(Code == "CEVE"), aes(x = Cover)) +
    geom_histogram(bins = 30) +
    facet_wrap(~Year) +
    theme_bw())

##rabbitbrush
(ggplot(shrubPlot %>% filter(Code == "RABBIT"), aes(x = Cover)) +
    geom_histogram(bins = 30) +
    facet_wrap(~Year) +
    theme_bw())


###############################
#tests####
##vegetation quads (3 species per quad, 8 quads per plot, 3 plots per treatment (6 for control), and 14 per stand (13 for Trout))
###checking that every quadrat has 3 species
vegSpCheck <- vegQuad %>% group_by(Yr, Stand, Treatment, Plot, Quad) %>%
  summarise(n_Species = n_distinct(Species), .groups = 'drop')

###checking that every year has 89 plots, 5 stands, and 5 treatments
vegYrCheck <- vegQuad %>% group_by(Yr) %>%
  summarise(n_Plots = n_distinct(Plot),
            n_Treatment = n_distinct(Treatment),
            n_Stand = n_distinct(Stand), .groups = 'drop')

vegQuadCheck <- vegQuad %>% group_by(Yr, Plot) %>%
  summarise(n_Quad = n_distinct(Quad), .groups = 'drop')

vegStandCheck <- vegQuad %>% group_by(Yr) %>%
  summarise(n_Stand = n_distinct(Stand), .groups = 'drop')

vegTreatmentCheck <- vegQuad %>% group_by(Yr) %>%
  summarise(n_Treat = n_distinct(Treatment), .groups = 'drop')

###making sure every plot has 8 quadrats
vegQuadCheck <- vegQuad %>% group_by(Yr, Stand, Treatment, Plot) %>%
  summarise(n_Quads = n_distinct(Quad), .groups = 'drop')

###making sure the unique combination of year, stand, treatment, plot, quadrat, and species exists

