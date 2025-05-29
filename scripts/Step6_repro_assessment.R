### Reproductive Trees ###
## Lydia M. Druin ##
# GRYN WBP Summary and Plots #

# Stored as "/scripts/Step6_repro_assessment.R" #
# Updated 05/10/25 #

### Purpose ###
# This code examines characteristics about trees that have been observed with
# evidence of cone production at least once, or "known-reproductive" trees.

### Note to User ###


# Set-up ####

### Clean Up Workspace ###

rm(list = ls()) # empty environment/loaded items in workspace

gc() # release memory

### Load Libraries ###

install.packages(c("tidyverse", "santoku", "cowplot")) # only need to run this the first time
# Installing packages every session won't hurt, however!

library(tidyverse)
library(santoku)
library(cowplot)

options(scipen = 999) # trick to force R to stay in numeric notation
options(dplyr.width = Inf, dplyr.print_min = 100)

# Step 1. Load in Data ####

## Part A. Load Source File ####

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
  header = TRUE, na.strings = c("-999", "-1998", "n/a", "na", "NA", "nodata")
)

glimpse(grte) # glimpse at the data--make sure it is what you expect!

## Part B. Create Unique Identifier for Trees ####

grte$ClumpMembership[is.na(grte$ClumpMembership)] <- ""

grte$Tree_unq <- with(grte, paste(Stand_ID, Tree_ID, sep = "."))
grte$Tree_unq <- with(grte, paste(Tree_unq, ClumpMembership, sep = ""))
# We incorporate clump here because some trees "share" a tag number.

## Part C. Exclude Data ####

# Remove Trees Dead at Tagging #

idx <- grte[which(grte$YearFirstDied == grte$TreeAddYear), ]$Tree_unq
idx <- grte[which(grte$Tree_unq %in% idx), ]

length(unique(idx$Tree_unq))
# 57 trees were already dead when they were tagged. (552 total surveys)
# Because we don't know when they died, we will be excluding these data.

# Exclude:
grte <- grte[!(grte$Tree_unq %in% idx$Tree_unq), ]
# Resulting dataframe is of trees that were ALIVE when tagged!

# Remove Untrustworthy Data Records #

with(grte, table(AlertStatus, useNA = "always")) # Review alert statuses for all data records

grte <- subset(grte, AlertStatus != "ALERT")
with(grte, table(AlertStatus, useNA = "always"))

# Step 2. Tidy Data ####

## Part A. Remove Exception Trees ####

with(grte, table(TreeStatus))

# Exception trees are not excluded from the overall data--this is because they
# are monitored before and/or after exception (meaning, unable to be found during
# a given monitoring visit, but are found and surveyed during other visits).
# For blister rust purposes, because a tree was unable to be surveyed during an
# "exception" visit, we will treat these surveys as "no data" and censor them.

grte <- subset(grte, TreeStatus == "L" | TreeStatus == "D" | TreeStatus == "RD")
grte <- droplevels(grte)

with(grte, table(TreeStatus))

## Part B. Create Binary Variables ####

### Beetle Infestation ###

with(grte, table(MtnPineBeetle, useNA = "always"))

grte$BeetlePresence <- ifelse(grte$MtnPineBeetle == "Y", 1, 0)

# Verify:
with(grte, table(BeetlePresence, MtnPineBeetle, useNA = "always"))
# All "BeetlePresence = 0" records are under "MtnPineBeetle = N"? ALL GOOD!

### Blister Rust Infection ###

# Create CankerInfection column for blister rust infection detection #
grte$CankerInfection <- ifelse(grte$BranchInfectionAny == "Y", "Y",
  ifelse(grte$BoleInfectionAny == "Y", "Y", "N")
)

# Create RustInfection column for blister rust infection detection #
grte$RustInfection <- ifelse(grte$CankerInfection == "Y", "Y",
  ifelse(grte$InfectionPresent == "N" | is.na(grte$InfectionPresent), "N", "Y")
)
# If a tree has observed branch cankers, bole cankers, or min. 3 indicators, it will = Y for RustInfection

# How likely is it that the cases where trees had no recorded cankers but
# did have observed indicators actually had blister rust? LMD 2024/2025: untrustworthy.
# So, we will coerce cases where trees have min. 3 indicators but no cankers to
# "RustInfection" = N
grte$RustInfection <- ifelse(grte$CankerInfection == "N" &
  grte$RustInfection == "Y", "N", grte$RustInfection)


with(grte, table(RustInfection, useNA = "always"))

grte$RustPresence <- ifelse(grte$RustInfection == "Y", 1, 0)

# Verify:
with(grte, table(RustPresence, RustInfection, useNA = "always"))
# All "RustPresence = 0" records are under "RustInfection = N"? ALL GOOD!

### Cone Production ###

with(grte, table(ConesPresent, useNA = "always"))
grte$ConePresence <- as.factor(ifelse(grte$ConesPresent == "Y", 1, 0))

# Step 3. Interpolate DBH ####

# DBH was only recorded in 2007 and 2011; many records have missing DBH data!
# So, for trees with a known DBH, we will update missing data to the last-known
# DBH values

## Part A. Create Dataframe of DBH Data ####

# Identify the Earliest Year+Value of DBH:
grte_earliest <- grte %>%
  group_by(Tree_unq) %>%
  filter(DBH_cm > 0) %>%
  summarize(
    meas1 = min(SurveyYear),
    dbh1 = min(DBH_cm)
  ) %>%
  ungroup()
# Add this to the main dataframe:
grte <- left_join(grte, grte_earliest)

# Identify the Newest Year+Value of DBH:
grte_latest <- grte %>%
  group_by(Tree_unq) %>%
  filter(DBH_cm > 0) %>%
  summarize(
    meas2 = max(SurveyYear),
    dbh2 = max(DBH_cm)
  ) %>%
  ungroup()
# Add this to the main dataframe:
grte <- left_join(grte, grte_latest)

# Create DBH-specific dataframe:
tree_dbh_data <- grte %>%
  group_by(SurveyYear, Tree_unq) %>%
  summarize(
    dbh1 = first(dbh1),
    dbh2 = first(dbh2),
    meas1 = first(meas1),
    meas2 = first(meas2)
  )

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
tree_dbh_data$Interp_DBH_Year <- ifelse(tree_dbh_data$dif1 < tree_dbh_data$dif2,
  tree_dbh_data$meas1, tree_dbh_data$meas2
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

grte <- left_join(grte, tree_dbh_data)

glimpse(grte) # Check out the new interpolated DBH data!

with(grte, table(Interp_DBH_Cat, useNA = "always")) # Review the distribution of DBH

# How many individuals are lacking DBH measurements?
grte_sub <- subset(grte, is.na(grte$Interp_DBH_Cat))
length(unique(grte_sub$Tree_unq))
# 23 trees in the processed data do not have DBH (and, by extension, height) data!
# We will handle these missing data later on.

# Step 3. Refine Data ####

## Part A. Identify and Remove Post-Dead Survey Visits ####

# Explanation: frequently, trees were surveyed after their initial survey observation
# where "TreeStatus" was recorded as "RD" (recently dead) or "D" (dead). We will
# remove these extra survey visits and retain only that initial survey of RD/D.

grte.live <- subset(grte, TreeStatus == "L") # create dataframe of only L surveys
grte.dead <- subset(grte, TreeStatus != "L") # create dataframe of only RD/D surveys

grte.dead$InitialDead <- grte.dead$YearFirstDied == grte.dead$SurveyYear # identify surveys where YearFirstDied = SurveyYear

with(grte.dead, table(InitialDead)) # review counts of initial and extra surveys of dead trees
grte.dead <- subset(grte.dead, InitialDead == "TRUE") # remove the extra surveys
grte.dead <- droplevels(grte.dead)

grte.dead <- select(grte.dead, -InitialDead) # remove the "InitialDead" column

grte.tidy <- rbind(grte.live, grte.dead) # reunite dead and live tree surveys

# Step 5. Known-Reproductive Trees Mortality ####

## Part A. Identify Known-Reproductive Trees ####

grte_repro <- grte.tidy %>%
  arrange(Stand_ID, Tree_ID, ClumpMembership, SurveyYear) %>%
  group_by(Tree_unq) %>%
  filter(any(ConePresence == "1"))
glimpse(grte_repro)
# The number of groups indicates the number of trees observed with cone evidence at least once

## Part B. Identify Mortality amongst Known-Reproductive Trees ####

# From this dataframe, we can see how many known-reproductive trees have died:
with(grte_repro, table(TreeStatus))
# The sum of D, L, and RD values will exceed the number of known-reproductive individuals
# because it includes all surveys of these trees (so, all living surveys up until death)

# LMD May 2025: 34 (D + RD) of these trees are dead of 165 total (~21%)
# For reference, ~34% of GRYN-monitored reproductive trees have died

## Part C. Conditions of Dead Trees ####

# Sort data in order and backfill values in BR.overall of dead trees with last-known value:

with(grte_repro, table(TreeStatus, RustPresence, useNA = "always"))
# Note how dead trees do not have RustPresence == 1

grte.dead <- subset(grte_repro, TreeStatus != "L")
grte.live <- subset(grte_repro, TreeStatus == "L")
grte.dead$RustPresence[grte.dead$RustPresence == 0] <- NA
with(grte.dead, table(RustPresence, useNA = "always"))
grte_repro <- rbind(grte.live, grte.dead)

grte_repro <- grte_repro %>% arrange(Stand_ID, Tree_ID, ClumpMembership, SurveyYear)
grte_repro$Rust_Backfill <- grte_repro$RustPresence
grte_repro <- grte_repro %>% fill(Rust_Backfill, .direction = "down")
with(grte_repro, table(RustPresence, Rust_Backfill, useNA = "always"))
# See how the NA values in "RustPresence" now have 0/1 values in "Rust_Backfill"?

grte_repro$Beetle_Backfill <- grte_repro$BeetlePresence
grte_repro <- grte_repro %>% fill(Beetle_Backfill, .direction = "down")
with(grte_repro, table(BeetlePresence, Beetle_Backfill, useNA = "always"))

# Create "Condition" to describe what tree had when it died:
grte_condition <- grte_repro %>% mutate(
  Condition =
    case_when(
      Beetle_Backfill == "1" & Rust_Backfill == "1" ~ "Combination",
      Beetle_Backfill == "1" & Rust_Backfill == "0" ~ "Mountain Pine Beetle",
      Beetle_Backfill == "0" & Rust_Backfill == "1" ~ "Blister Rust",
      Beetle_Backfill == "0" & Rust_Backfill == "0" ~ "Other"
    )
)
with(grte_condition, table(TreeStatus, Condition, useNA = "always"))

# Of known-reproductive trees, when have they died and what conditions did they have?
with(subset(grte_condition, TreeStatus != "L"), table(Condition, SurveyYear))

with(subset(grte_condition, TreeStatus != "L"), table(Condition))
# LMD May 2025: 3 + 18 trees died with beetles (count of beetles + combination)
# 10 + 18 trees died with rust (count of rust + combination)
# 3 of 34 dead known-reproductive trees did not have observed beetles, rust, or fire

## Part D. Cone Production Observation Frequency ####

# We can also see how often we observed cone evidence:
with(grte_repro, table(ConePresence))
# LMD Dec 2024: of 1155 total surveys to known-reproductive trees, we observed
# evidence of cone production 605 times! (~52%) [GRYN is 50%!]

## Part E. Cone Production Frequency ####

grte_freq <- grte %>%
  group_by(Tree_unq) %>%
  count(ConePresence) %>%
  ungroup()
grte_freq <- subset(grte_freq, ConePresence == "1")
glimpse(grte_freq)

summary(grte_freq$n)
# LMD May 2025: known-reproductive trees were observed producing cones 3.7 times on average!

# Step 6. Plot Data ####

## Part A. Plot DBH Distribution of Reproductive Trees ####

grte_dbh <- c("Tree_unq", "Interp_DBH_Cat", "Interp_DBH_Year")
grte_dbh <- grte.tidy[, grte_dbh]

grte_freq <- left_join(grte_freq, grte_dbh, by = "Tree_unq")

repro_dbh <- ggplot(
  data = grte_freq,
  aes(x = Interp_DBH_Cat)
) +
  geom_bar(stat = "count", position = position_dodge(preserve = "single")) +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "none", panel.spacing = unit(0, "cm")
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Diameter-at-Breast-Height (cm)") +
  ylab("Count of Whitebark Pine Trees") +
  labs(title = "Distribution of DBH amongst Cone-Producing Whitebark Pines")
repro_dbh # Call plot; take a look!

ggsave("output/figures/ReproductiveTrees_DBHDistribution_wide.png",
  plot = last_plot(), width = 8,
  height = 6.5, units = c("in")
)
ggsave("output/figures/ReproductiveTrees_DBHDistribution_tall.png",
  plot = last_plot(), width = 6.5,
  height = 8, units = c("in")
)

## Part C. Plot Frequency of Cone Production Observations by DBH ####

dbh_freq <- ggplot(
  data = grte_freq,
  aes(x = Interp_DBH_Cat, y = n)
) +
  geom_boxplot() +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "none", panel.spacing = unit(0, "cm")
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Diameter-at-Breast-Height (cm)") +
  ylab("Frequency of Observing Cone Production per Tree") +
  labs(title = "Frequency of Cone Production by Last-known DBH amongst Whitebark Pines")
dbh_freq

ggsave("output/figures/ConeProduction_Frequency_byDBH_wide.png",
  plot = last_plot(), width = 8,
  height = 6.5, units = c("in")
)
ggsave("output/figures/ConeProduction_Frequency_byDBH_tall.png",
  plot = last_plot(), width = 6.5,
  height = 8, units = c("in")
)

## Part D. Conditions of Dead Known-Reproductive Trees ####

condition_dbhyear <- ggplot(
  data = subset(grte_condition, TreeStatus != "L"),
  aes(x = SurveyYear, fill = Condition)
) +
  geom_bar(stat = "count", position = position_dodge(preserve = "single")) +
  facet_wrap(~Interp_DBH_Cat, strip.position = "bottom", ncol = 5) +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "bottom", panel.spacing = unit(0, "cm"), legend.title = element_blank()
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Year") +
  ylab("Count of Dead Whitebark Pine Trees") +
  labs(title = "Conditions observed by Last-known DBH each year in Dead Known-Reproductive Trees")
condition_dbhyear # Call plot; take a look!

# Save plots!
ggsave("output/figures/ConditionsDBHYear_KnownReproductive_wide.png",
  plot = last_plot(), width = 8,
  height = 6.5, units = c("in")
)
ggsave("output/figures/ConditionsDBHYear_KnownReproductive_tall.png",
  plot = last_plot(), width = 6.5,
  height = 8, units = c("in")
)

condition_year <- ggplot(
  data = subset(grte_condition, TreeStatus != "L"),
  aes(x = SurveyYear, fill = Condition)
) +
  geom_bar(stat = "count", position = position_dodge(preserve = "single")) +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "bottom", panel.spacing = unit(0, "cm"), legend.title = element_blank()
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Year") +
  ylab("Count of Dead Whitebark Pine Trees") +
  labs(title = "Conditions observed each year on Dead Known-Reproductive Trees")
condition_year # Call plot; take a look!

# Save plots!
ggsave("output/figures/ConditionsYear_KnownReproductive_wide.png",
  plot = last_plot(), width = 8,
  height = 6.5, units = c("in")
)
ggsave("output/figures/ConditionsYear_KnownReproductive_tall.png",
  plot = last_plot(), width = 6.5,
  height = 8, units = c("in")
)

condition_dbh <- ggplot(
  data = subset(grte_condition, TreeStatus != "L"),
  aes(x = Interp_DBH_Cat, fill = Condition)
) +
  geom_bar(stat = "count", position = position_dodge(preserve = "single")) +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "bottom", panel.spacing = unit(0, "cm"), legend.title = element_blank()
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Diameter at Breast Height (cm)") +
  ylab("Count of Dead Whitebark Pine Trees") +
  labs(title = "Conditions observed by Last-known DBH on Dead Trees")
condition_dbh # Call plot; take a look!

# Save plots!
ggsave("output/figures/ConditionsDBH_KnownReproductive_wide.png",
  plot = last_plot(), width = 8,
  height = 6.5, units = c("in")
)
ggsave("output/figures/ConditionsDBH_KnownReproductive_tall.png",
  plot = last_plot(), width = 6.5,
  height = 8, units = c("in")
)

# END ----
