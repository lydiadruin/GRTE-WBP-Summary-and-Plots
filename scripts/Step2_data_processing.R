### Data Processing ###
## Lydia M. Druin ##
# GRTE WBP Summary and Plots #

# Stored as "/scripts/Step2_data_processing.R" #
# Updated 05/08/25 #

### Purpose ###
# This code is for processing data #

### Note to User ###
# You MUST run "Step1_data_joining.R" and complete processing steps BEFORE
# running this code!
# This code is the same as in the R project "GRTE WBP Ratio Estimator and Bayes Blister Rust" #

# Set-up ####

### Clean Up Workspace ###
rm(list = ls()) # empty environment/loaded items in workspace
gc() # release memory

### Load Libraries ###

install.packages(c("tidyverse", "santoku")) # only need to run this the first time
# Installing packages every session won't hurt, however!

library(tidyverse)

options(scipen = 999) # trick to force R to stay in numeric notation
options(dplyr.width = Inf, dplyr.print_min = 100) # viewer settings

# Step 1. Load Source File ####

sourcefile <- "GRTE_WBP_CompleteReviewedData_QAQC_LMDMay2025.csv"
# Note to user: If you are incorporating post-2024 data, this is a point where
# you need to change the above file name to *your* new file name! Make sure it is
# saved as a .csv file, and copy+paste the file name into the above line while
# retaining the quotation marks around the file name + ".csv" at the end!

# IMPORTANT NOTE: Complete "Step1_data_joining.R" and all processing steps
# to generate the sourcefile for this code!

path <- "data/raw_data"

sourcepath <- paste(path, sourcefile, sep = "/")
grte <- read.csv(sourcepath,
  header = TRUE, na.strings = c("-999", "na", "NA", "")
)

glimpse(grte) # glimpse at the data--make sure it is what you expect!

# Step 2. Filter Data ####

## Part A. Create Unique Identifier for Stands and Trees ####

grte$ClumpMembership[is.na(grte$ClumpMembership)] <- ""

grte$Tree_unq <- with(grte, paste(Stand_ID, Tree_ID, sep = "."))
grte$Tree_unq <- with(grte, paste(Tree_unq, ClumpMembership, sep = ""))
# We incorporate clump here because some trees "share" a tag number.

## Part B. Exclude Trees Dead at Tagging ####

idx <- grte[which(grte$YearFirstDied == grte$TreeAddYear), ]$Tree_unq
idx <- grte[which(grte$Tree_unq %in% idx), ]

length(unique(idx$Tree_unq))
# 57 trees were already dead when they were tagged. (552 total surveys)
# Because we don't know when they died, we will be excluding these data.

# First, save these records if interested:
write.csv(idx, "data/review/DeadTreesTagged_atMonitoringStart_LMD_May2025.csv", row.names = FALSE)

# Exclude:
grte_live <- grte[!(grte$Tree_unq %in% idx$Tree_unq), ]
# Resulting dataframe is of trees that were ALIVE when tagged!

## Part C. Exclude Untrustworthy Data Records ####

# These are records that need further review; see the xlsx file for further info!

with(grte_live, table(AlertStatus)) # Review alert statuses for all data records

grte_liveTRUST <- subset(grte_live, AlertStatus == "caution" | is.na(grte_live$AlertStatus))
# Resulting dataframe is of trees that do NOT have an alert! And were alive when tagged.

with(grte_liveTRUST, table(AlertStatus, useNA = "always"))

# To review the records with alert statuses, check the xlsx file! Many trees
# only have a couple records with alerts--every record for a given tree may
# not always have an alert. Viewing the data in whole in the xlsx file is recommended.

## Part D. Data Checks ####

# Rule 1: Dead trees cannot have green needles
with(grte_liveTRUST, table(TreeStatus, NeedleColor, useNA = "always"))

# Rule 2: Dead trees cannot bear cones
with(grte_liveTRUST, table(TreeStatus, ConesPresent, useNA = "always"))

## Part E. Identify and Remove Exception Trees ####

with(grte_liveTRUST, table(TreeException, useNA = "always"))

idx <- grte_liveTRUST[!is.na(grte_liveTRUST$TreeException), ]$Tree_unq
idx <- grte_liveTRUST[grte_liveTRUST$Tree_unq %in% idx, ]

# Write these trees and the exception cases to a csv file if interested:
write.csv(idx, "data/review/ExceptionTrees_LMDMay2025.csv", row.names = FALSE)

# Exception trees are not excluded from the overall data--this is because they
# are monitored before and/or after exception (meaning, unable to be found during
# a given monitoring visit, but are found and surveyed during other visits).
# When generating data summaries, however, we will treat these tree exception
# surveys as "no data" and exclude them from our summaries

grte_liveTRUST <- subset(grte_liveTRUST, is.na(grte_liveTRUST$TreeException))

# Step 3. Interpolate DBH ####

# DBH was only recorded in 2007 and 2011; many records have missing DBH data!
# So, for trees with a known DBH, we will update missing data to the last-known
# DBH values

## Part A. Create Dataframe of DBH Data ####

# Identify the Earliest Year+Value of DBH:
grte_earliest <- grte_liveTRUST %>%
  group_by(Tree_unq) %>%
  filter(DBH_cm > 0) %>%
  summarize(
    meas1 = min(SurveyYear),
    dbh1 = min(DBH_cm)
  ) %>%
  ungroup()
# Add this to the main dataframe:
grte_liveTRUST <- left_join(grte_liveTRUST, grte_earliest)

# Identify the Newest Year+Value of DBH:
grte_latest <- grte_liveTRUST %>%
  group_by(Tree_unq) %>%
  filter(DBH_cm > 0) %>%
  summarize(
    meas2 = max(SurveyYear),
    dbh2 = max(DBH_cm)
  ) %>%
  ungroup()
# Add this to the main dataframe:
grte_liveTRUST <- left_join(grte_liveTRUST, grte_latest)

# Create DBH-specific dataframe:
tree_dbh_data <- grte_liveTRUST %>%
  group_by(SurveyYear, Tree_unq) %>%
  summarize(
    dbh1 = first(dbh1),
    dbh2 = first(dbh2),
    meas1 = first(meas1),
    meas2 = first(meas2)
  )
grte_liveTRUST <- grte_liveTRUST[, 1:129]

## Part B. Determine the difference between measurement years ####

tree_dbh_data <- tree_dbh_data %>%
  mutate(
    dif1 = abs(SurveyYear - meas1),
    dif2 = abs(SurveyYear - meas2)
  )
# Create values for the difference (dif1/2) between year surveyed and the earliest
# (meas1) or latest (meas2) year of DBH measurement

## Part C. Assign DBH value (either earliest or latestDBH) ####

tree_dbh_data$Interp_DBH <- ifelse(tree_dbh_data$dif1 < tree_dbh_data$dif2,
  tree_dbh_data$dbh1, tree_dbh_data$dbh2
)
# Voo Doo R-stats! Explanation below if interested:
# When the difference between the survey year and latest year of DBH measurement is
# larger than the difference between the survey year and earliest year of DBH measurement,
# take the value from dbh1 (earliest DBH); but, when that is smaller, take the
# value from dbh2 (latest DBH)--put the selected value into Interp_DBH.
# The goal is to represent Interp_DBH as the most likely measurement of DBH for a given
# survey year. Recall that DBH is not measured every survey visit--trees are
# supposed to be re-measured 10-15 years. This means if the earliest DBH was
# measured 16 years (say, 2004) from a given survey's year (say, 2020), but the
# latest DBH was measured 4 years (say, 2016) from a given survey's year, we will
# assign "Interp_DBH" as equal to the DBH measurement from 2016, because that is
# more recent! The code repeats for every year a tree was surveyed.

## Part D. Bin Interp_DBH into the DBH categories ####
santoku::tab(tree_dbh_data$Interp_DBH, c(2.5, 10, 30)) # Show the number of cases of each DBH category
tree_dbh_data$Interp_DBH_Cat <- santoku::chop(tree_dbh_data$Interp_DBH, c(2.5, 10, 30))

## Part E. Add Interpolated DBH to main dataframe ####

tree_dbh_data <- select(tree_dbh_data, -dbh1, -dbh2, -meas1, -meas2, -dif1, -dif2) %>% ungroup()

grte_liveTRUST <- left_join(grte_liveTRUST, tree_dbh_data)

glimpse(grte_liveTRUST) # Check out the new interpolated DBH data!

with(grte_liveTRUST, table(Interp_DBH_Cat, useNA = "always")) # Review the distribution of DBH

# How many individuals are lacking DBH measurements?
grte_sub <- subset(grte_liveTRUST, is.na(grte_liveTRUST$Interp_DBH_Cat))
length(unique(grte_sub$Tree_unq))
# 24 trees in the processed data do not have DBH (and, by extension, height) data!

# Step 4. Process Data ####

## Part A. Blister Rust Infection ####

# First: How do we define blister rust infection?
# If InfectionPresent = TRUE then at least one of two things is true:
# 3 of 5 indicators were observed (flagging, rodent chewing, etc)
# AeciaPresent = TRUE
# However, prior to 2024, GRTE did not record Aecia presence.
# This means that InfectionPresent = TRUE only happens when indicators were recorded!
# Therefore, many trees will have recorded cankers *but* InfectionPresent = FALSE
# because indicators were not recorded.

# Create CankerInfection column for blister rust infection detection #
grte_liveTRUST$CankerInfection <- ifelse(grte_liveTRUST$BranchInfectionAny == "Y", "Y",
  ifelse(grte_liveTRUST$BoleInfectionAny == "Y", "Y", "N")
)

# Create RustInfection column for blister rust infection detection #
grte_liveTRUST$RustInfection <- ifelse(grte_liveTRUST$CankerInfection == "Y", "Y",
  ifelse(grte_liveTRUST$InfectionPresent == "N" | is.na(grte_liveTRUST$InfectionPresent), "N", "Y")
)
# If a tree has observed branch cankers, bole cankers, or min. 3 indicators, it will = Y for RustInfection

# Trees with indicator-based infections but no observed cankers:
idx <- grte_liveTRUST[which(grte_liveTRUST$RustInfection == "Y" & grte_liveTRUST$CankerInfection == "N"), ]$Tree_unq
idx <- grte_liveTRUST[which(grte_liveTRUST$Tree_unq %in% idx), ]
# Export a csv file of these trees that have a survey record where indicators
# show a rust infection but no cankers were recorded:
write.csv(idx, "data/review/BlisterRustOverall_NonspecificInfections_LMDMay2025.csv", row.names = FALSE)

length(unique(idx$Tree_unq)) # 19 trees had recorded an indicator-based infection but no cankers

# How likely is it that the cases where trees had no recorded cankers but
# did have observed indicators actually had blister rust? LMD 2024/2025: untrustworthy.
# So, we will coerce cases where trees have min. 3 indicators but no cankers to
# "RustInfection" = N
grte_liveTRUST$RustInfection <- ifelse(grte_liveTRUST$CankerInfection == "N" &
  grte_liveTRUST$RustInfection == "Y", "N", grte_liveTRUST$RustInfection)

## Part B. Cone Presence and Mountain Pine Beetle ####

# Cone Presence
with(grte_liveTRUST, table(ConesPresent))

# Correct cone presence to be only Y/N:
grte_liveTRUST$ConesPresent <- ifelse(grte_liveTRUST$ConesPresent == "Y", "Y", "N")

# Beetle Presence
with(grte_liveTRUST, table(MtnPineBeetle))

# Correct beetle presence to be only Y/N:
grte_liveTRUST$MtnPineBeetle <- ifelse(grte_liveTRUST$MtnPineBeetle == "Y", "Y", "N")

## Part C. Identify and Remove Post-Dead Survey Visits ####

# Explanation: frequently, trees were surveyed after their initial survey observation
# where "TreeStatus" was recorded as "RD" (recently dead) or "D" (dead). We will
# remove these extra survey visits and retain only that initial survey of RD/D.

grte.live <- subset(grte_liveTRUST, TreeStatus == "L") # create dataframe of only L surveys
grte.dead <- subset(grte_liveTRUST, TreeStatus != "L") # create dataframe of only RD/D surveys

grte.dead$InitialDead <- grte.dead$YearFirstDied == grte.dead$SurveyYear # identify surveys where YearFirstDied = SurveyYear

with(grte.dead, table(InitialDead)) # review counts of initial and extra surveys of dead trees
grte.dead <- subset(grte.dead, InitialDead == "TRUE") # remove the extra surveys
grte.dead <- droplevels(grte.dead)

grte.dead <- select(grte.dead, -InitialDead) # remove the "InitialDead" column

grte_clean <- rbind(grte.live, grte.dead) # reunite dead and live tree surveys

# Step 5. Export Data ####

## Part A. Alive/Trusted Records ####
write.csv(grte_liveTRUST, "data/review/grte_wbp_processed_liveTRUSTrecords_LMDMay2025.csv", row.names = FALSE)
# This file goes into the review folder as we do not use data from post-dead surveys.

## Part B. Alive/Trusted Records with Post-Dead Surveys Excluded ####
write.csv(grte_clean, "data/processed_data/grte_wbp_processed_liveTRUSTpostdeadrecords_LMDMay2025.csv", row.names = FALSE)

# END ----
