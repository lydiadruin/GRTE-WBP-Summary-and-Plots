### Numeric Summaries ###
## Lydia M. Druin ##
# GRTE WBP Summary and Plots #

# Stored as "/scripts/Step3_summary_numeric.R" #
# Updated 05/08/25 #

### Purpose ###
# This code is for generating numeric summary review data of monitored
# whitebark pine stands in Grand Tetons National Park #

### Note to User ###
# You MUST run "Step1_data_joining.R" and complete processing steps BEFORE
# running this code and "Step2_data_processing.R"

# Set-up ####

### Clean Up Workspace ###
rm(list = ls()) # empty environment/loaded items in workspace
gc() # release memory

### Load Libraries ###

install.packages(c("tidyverse", "cowplot")) # only need to run this the first time
# Installing packages every session won't hurt, however!

library(tidyverse)
library(cowplot)

options(scipen = 999) # trick to force R to stay in numeric notation
options(dplyr.width = Inf, dplyr.print_min = 100) # viewer settings

# Step 1. Load Source File ####
sourcefile <- "grte_wbp_processed_liveTRUSTpostdeadrecords_LMDMay2025.csv"
# Note to user: If you are incorporating post-2024 data, this is a point where
# you need to change the above file name to *your* new file name! Make sure it is
# saved as a .csv file, and copy+paste the file name into the above line while
# retaining the quotation marks around the file name + ".csv" at the end!

path <- "data/processed_data"

sourcepath <- paste(path, sourcefile, sep = "/")

grte <- read.csv(sourcepath, header = TRUE)

glimpse(grte) # glimpse at the data--make sure it is what you expect!

# Step 2. Create Numeric Summaries ####

## Part A. Blister Rust Infection ####

# Infections by Year #
with(grte, table(SurveyYear, RustInfection, useNA = "always"))
# count of trees observed w/ infections each year; note that NA values are dead trees (dead trees won't have active rust infections)

# Infections by Stand #
with(grte, table(Stand_ID, RustInfection, useNA = "always"))
# count of observations of infections in each stand; note that NA values are dead trees (dead trees won't have active rust infections)

# Export these data:
rust_table <- with(grte, table(SurveyYear, Stand_ID, RustInfection, Interp_DBH))
write.csv(rust_table, "output/results/BlisterRustInfections_Summary_LMDMay2025.csv", row.names = FALSE)

## Part B. Cone Presence ####

# Cone Detections by Year #
with(grte, table(Interp_DBH_Cat, ConesPresent, SurveyYear))
# count of trees observed with cones each year

# Cone Detections by Stand #
with(grte, table(Interp_DBH_Cat, ConesPresent, Stand_ID))
# count of *observations* of cones in each stand (not # of trees!)

# Export these data:
cone_table <- with(grte, table(SurveyYear, Stand_ID, ConesPresent, Interp_DBH))
write.csv(cone_table, "output/results/ConePresence_Summary_LMDMay2025.csv", row.names = FALSE)

## Part C. Beetle Infestation ####

# Beetle Infestations by Year #
with(grte, table(Interp_DBH_Cat, MtnPineBeetle, SurveyYear))
# count of trees observed w/ beetles each year

# Beetle Infestations by Stand #
with(grte, table(Interp_DBH_Cat, MtnPineBeetle, Stand_ID))
# count of *observations* of beetles in each stand

# Export these data:
beetle_table <- with(grte, table(SurveyYear, Stand_ID, MtnPineBeetle, Interp_DBH))
write.csv(beetle_table, "output/results/BeetlePresence_Summary_LMDMay2025.csv", row.names = FALSE)

## Part D. Trees Entering and Exiting Monitoring ####

grte$Tree_unq = paste(grte$Stand_ID, grte$Tree_ID, grte$ClumpMembership, sep = ".")
unique_trees <- grte[!duplicated(grte$Tree_unq), ] # Specify one record per individual

# Trees Entering Monitoring by Year #
with(unique_trees, table(TreeAddYear, useNA = "always"))

# Trees Entering Monitoring by Stand and Year #
with(unique_trees, table(TreeAddYear, Stand_ID, useNA = "always"))
# Note: this is a count of the number of trees permanently tagged and entering
# into monitoring (either at study start or once reaching 140cm height) at
# each stand throughout the years. Yes--25 Short only has one tree in monitoring!
# (because all other tagged trees were already dead when tagged; LMD 2025)

# Trees Exiting Monitoring by Year #
with(unique_trees, table(YearFirstDied, useNA = "always"))

# Trees Exiting Monitoring by Stand and Year #
with(unique_trees, table(YearFirstDied, Stand_ID, useNA = "always"))
# Note: likewise--of those trees in monitoring, this is a count of how many
# have died! The NA values are the number of trees per site still alive.

# Export these data:
enterexit_table <- with(unique_trees, table(Stand_ID, TreeAddYear, YearFirstDied, Interp_DBH, useNA = "always"))
write.csv(enterexit_table, "output/results/EnterExitingMonitoring_Summary_LMDMay2025.csv", row.names = FALSE)

# Step 4. Understory Data Summary ####

## Part A. Prepare Understory Data ####

understory <- c(
  "SamplePanel", "Stand_ID", "SurveyYear", "SS_noR_germ",
  "SS_BR_germ", "SS_noR_40", "SS_BR_40", "SS_noR_100", "SS_BR_100",
  "SS_noR_139", "SS_BR_139", "SeedSap_BR_Absent", "SeedSap_BR_PresentUnknown",
  "elev_meters", "slope_percent", "aspect_degrees", "GrizPCA", "TsectAzimuth",
  "VegType_GRTE", "HabitatType_Steele", "CoverType_Despain", "CanopyCov_Site",
  "CanopyCov_PIAL", "CanopyCov_ABLA", "CanopyCov_PIEN", "CanopyCov_PICO",
  "Midden_Undis", "Midden_Excav", "SeedSource_Dist", "SeedSource_Descr",
  "Disturb_Comment", "Understory_Tshrub", "Height_TShrub", "Cover_Tshrub",
  "Understory_Sshrub", "Height_SShrub", "Cover_SShrub", "Understory_Grama",
  "Height_Grama", "Cover_Grama", "Understory_Forb", "Height_Forb", "Cover_Forb",
  "Understory_Nonvasc", "Height_Nonvasc", "Cover_Nonvasc", "Object_Rocks",
  "Object_DownLog", "Object_Trunk", "Object_Grass", "Object_Forb"
)
understory <- grte[, understory]

# Understory data is at the stand-level, so we will reduce to just one record per stand each year!
understory$Stand_Year <- with(understory, paste(Stand_ID, SurveyYear, sep = "."))
understory <- understory[!duplicated(understory$Stand_Year), ]
understory <- understory %>% select(-Stand_Year) # Remove "Stand_Year" now that we don't need it

## Part B. Density of Understory Trees ####

understory$SeedSap_Total <- understory$SeedSap_BR_Absent + understory$SeedSap_BR_PresentUnknown

understory$SeedSap_Tot_Density <- understory$SeedSap_Total / 0.05
understory$SeedSap_BRPresent_Density <- understory$SeedSap_BR_PresentUnknown / 0.05
understory$SeedSap_BRAbsent_Density <- understory$SeedSap_BR_Absent / 0.05

## Part C. Summary Data ####

# Counts of Observed Seedling/Saplings by Year and Stand
understory_counts <- understory %>%
  group_by(SurveyYear, Stand_ID) %>%
  select(SeedSap_BR_Absent, SeedSap_BR_PresentUnknown, SeedSap_Total)
View(understory_counts)
# Export:
write.csv(understory_counts, "output/results/ObservedCounts_SeedSap_byYearbyStand_LMDMay2025.csv", row.names = FALSE)

# Densities (SEEDLINGS PER HECTARE!) by Year #
understory_density_byYear <- understory %>%
  group_by(SurveyYear) %>%
  summarize(
    Min_Tot_Density = min(SeedSap_Tot_Density),
    Mean_Tot_Density = mean(SeedSap_Tot_Density),
    Max_Tot_Density = max(SeedSap_Tot_Density),
    Min_BR_Density = min(SeedSap_BRPresent_Density),
    Mean_BR_Density = mean(SeedSap_BRPresent_Density),
    Max_BR_Density = max(SeedSap_BRPresent_Density),
    Min_Absent_Density = min(SeedSap_BRAbsent_Density),
    Mean_Absent_Density = mean(SeedSap_BRAbsent_Density),
    Max_Absent_Density = max(SeedSap_BRAbsent_Density)
  )
View(understory_density_byYear)
# Export:
write.csv(understory_density_byYear, "output/results/Density_perHectare_SeedSap_byYear_LMDMay2025.csv", row.names = FALSE)

# Densities (SEEDLINGS PER HECTARE!) by Stand #
understory_density_byStand <- understory %>%
  group_by(Stand_ID) %>%
  summarize(
    Min_Tot_Density = min(SeedSap_Tot_Density),
    Mean_Tot_Density = mean(SeedSap_Tot_Density),
    Max_Tot_Density = max(SeedSap_Tot_Density),
    Min_BR_Density = min(SeedSap_BRPresent_Density),
    Mean_BR_Density = mean(SeedSap_BRPresent_Density),
    Max_BR_Density = max(SeedSap_BRPresent_Density),
    Min_Absent_Density = min(SeedSap_BRAbsent_Density),
    Mean_Absent_Density = mean(SeedSap_BRAbsent_Density),
    Max_Absent_Density = max(SeedSap_BRAbsent_Density)
  )
View(understory_density_byStand)
# Export:
write.csv(understory_density_byStand, "output/results/Density_perHectare_SeedSap_byStand_LMDMay2025.csv", row.names = FALSE)

## Part D. Midden Counts ####

# Observations of Undisturbed Middens by Year
with(understory, table(SurveyYear, Midden_Undis, useNA = "always"))

# Observations of Undisturbed Middens by Stand
with(understory, table(Stand_ID, Midden_Undis, useNA = "always"))

# Observations of Excavated Middens by Year
with(understory, table(SurveyYear, Midden_Excav, useNA = "always"))

# Observations of Excavated Middens by Stand
with(understory, table(Stand_ID, Midden_Excav, useNA = "always"))

## Part E. Export Data ####

View(understory)

write.csv(understory, "data/processed_data/Understory_SiteBasedData_LMDMay2025.csv", row.names = FALSE)
# This data contains all data collected at the stand-level: veg/cover type, canopy cover,
# understory tree/seedling data, and midden data. Includes understory veg info
# and object counts when recorded! Review the above exported csv file to 
# explore further!

# END ----