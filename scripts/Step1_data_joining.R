### Data Joining ###
## Lydia M. Druin ##
# GRTE WBP Summary and Plots #

# Stored as "/scripts/Step1_data_joining.R" #
# Updated 05/08/25 #

### Purpose ###
# This code combines site-based data with the reviewed/QAQC'ed field data

### Note to User ###
# Review the notes at the end of this code #
# This code is the same as in the R project "GRTE WBP Ratio Estimator and Bayes Blister Rust" #

# Set-up ####

### Clean Up Workspace ###
rm(list = ls()) # empty environment/loaded items in workspace
gc() # release memory

### Load Libraries ###

install.packages(c("tidyverse")) # only need to run this the first time
# Installing packages every session won't hurt, however!

library(tidyverse)

options(scipen = 999) # trick to force R to stay in numeric notation
options(dplyr.width = Inf, dplyr.print_min = 100) # viewer settings

# Step 1. Load Source Files ####
## Part A. QA/QC'ed Data File ####
sourcefile <- "GRTE_WBP_CollatedData_QAQC_LMD_May2025.csv"
# Note to user: If you are incorporating post-2024 data, this is a point where
# you need to change the above file name to *your* new file name! Make sure it is
# saved as a .csv file, and copy+paste the file name into the above line while
# retaining the quotation marks around the file name + ".csv" at the end!

path <- "data/raw_data"
sourcepath <- paste(path, sourcefile, sep = "/")

grte <- read.csv(sourcepath, header = TRUE, na.strings = c("-999", "na", "NA", "unk"))

glimpse(grte) # glimpse at the data--make sure it is what you expect!

## Part B. Site-Based Data File ####
sourcefile <- "GRTE_WBP_SiteData_LMDMay2025.csv"
# SITE DATA FILE DOES NOT NEED UPDATED UNLESS SITE DATA CHANGES!
sourcepath <- paste(path, sourcefile, sep = "/")

site <- read.csv(sourcepath, header = TRUE, na.strings = c("-999", "na", "NA", "unk"))

glimpse(site) # glimpse at the data--make sure it is what you expect!

# Step 2. Join Data ####

joindat <- left_join(site, grte, by = "Stand_ID")

# Step 3. Fill Missing Data Columns ####

## Part A. DBH Category ####

joindat$DBH_Cat <- ifelse(joindat$DBH_cm <= 2.5, "<=2.5cm",
  ifelse(joindat$DBH_cm <= 10, ">2.5 to <=10cm",
    ifelse(joindat$DBH_cm <= 30, ">10 to <=30cm",
      ifelse(joindat$DBH_cm > 30, ">30cm", "e. no data")
    )
  )
)
with(joindat, table(DBH_Cat, DBH_cm))

## Part B. Tree Add Date ####

# First, create unique identifier for Trees within Stands:
# We do this because some trees within one stand have the same tag as trees in
# another stand

joindat$Tree_unq <- with(joindat, paste(Stand_ID, Tree_ID, ClumpMembership, sep = "."))

# Next, identify the year each tree entered into monitoring:
grte_add <- joindat %>%
  group_by(Tree_unq) %>%
  summarize(TreeAddYear = min(SurveyYear)) %>%
  ungroup()

# Finally, combine "TreeAddYear" to the main dataframe:
grte <- left_join(joindat, grte_add, by = "Tree_unq")

## Part C. YearFirstDied ####

# First, subset data to only trees with dead/recently dead status:
joindat_died <- subset(joindat, TreeStatus == "D" | TreeStatus == "RD")

# Next, identify the earliest year they were recorded dead:
grte_died <- joindat_died %>%
  group_by(Tree_unq) %>%
  summarize(YearFirstDied = min(SurveyYear)) %>%
  ungroup()

# Combine "YearFirstDied" to the main dataframe:
grte <- left_join(grte, grte_died, by = "Tree_unq")

# Finally, remove "Tree_unq":
grte <- select(grte, -Tree_unq)

# Step 4. Export Data ####
write.csv(grte, "data/raw_data/GRTE_WBP_CombinedReviewedData_Rexport_LMDMay2025.csv", row.names = FALSE)

# FINAL NOTE ####

# The exported data csv file was then formatted so columns are in the same order
# as the data dictionary. See "GRTE_WBP_ReviewedDataSet_QAQC_AllSurveyData_LMDNov2024.xlsx".
# Data was also done where NAs in YearFirstDied were removed.
# Then, the data was copy+pasted into the above xlsx file on the ReviewedData+SiteData
# sheet.
# Finally, the data csv file with the correct formatting was saved as
# "GRTE_WBP_CompleteReviewedData_QAQC_LMDMay2025.csv"

# END ----