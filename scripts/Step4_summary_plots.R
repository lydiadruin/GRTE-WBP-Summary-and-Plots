### Graphical Summaries ###
## Lydia M. Druin ##
# GRTE WBP Summary and Plots #

# Stored as "/scripts/Step4_summary_plots.R" #
# Updated 05/08/25 #

### Purpose ###
# This code is for generating graphical summary review data of monitored
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
grte <- read.csv(sourcepath,
  header = TRUE, na.strings = c("-999", "na", "NA", "nodata", "")
)

glimpse(grte) # glimpse at the data--make sure it is what you expect!

# Step 2. Format Data ####

## Part A. Tree Status ####

grte$TreeStatus <- as.factor(with(grte, ifelse(TreeStatus == "L", "Alive", "Dead")))
with(grte, table(TreeStatus, useNA = "always")) # newly alphabetized!

## Part B. DBH Classes ####

with(grte, table(Interp_DBH_Cat, useNA = "always"))
grte$Interp_DBH[is.na(grte$Interp_DBH)] <- -999

grte$Interp_DBH_Class <- as.factor(with(grte, ifelse(Interp_DBH < 0, "no data",
  ifelse(Interp_DBH <= 2.5, ">0 to 2.5",
    ifelse(Interp_DBH <= 10, ">2.5 to 10",
      ifelse(Interp_DBH <= 30, ">10 to 30", ">30")
    )
  )
)))
grte$Interp_DBH_Class <- factor(grte$Interp_DBH_Class, levels = c(">0 to 2.5", ">2.5 to 10", ">10 to 30", ">30", "no data"))
with(grte, table(Interp_DBH_Class)) # in numerical order!

## Part C. Cone Presence ####

grte$ConesPresent <- as.factor(with(
  grte,
  ifelse(ConesPresent == "Y", "yes", "no")
))
# Review; is data grouped by "yes/no" now?
with(grte, table(ConesPresent, useNA = "always"))

## Part D. Beetle Presence ####

grte$MtnPineBeetle <- as.factor(with(
  grte,
  ifelse(MtnPineBeetle == "Y", "yes", "no")
))
# Review; is data grouped by "yes/no" now?
with(grte, table(MtnPineBeetle, useNA = "always"))

## Part E. Rust Presence ####

grte$RustInfection <- as.factor(with(
  grte,
  ifelse(RustInfection == "Y", "yes", "no")
))
# Review; is data grouped by "yes/no" now?
with(grte, table(RustInfection, useNA = "always"))

# Step 3. Plotting ####

## Part A. Cone Presence ####

# Remove NAs in ConesPresent:
grte.cone_presence <- grte %>% drop_na(ConesPresent)

# Make first plot
cone_presence <- ggplot(
  data = subset(grte.cone_presence, TreeStatus == "Alive"),
  aes(x = SurveyYear, fill = ConesPresent)
) +
  geom_bar(stat = "count", position = position_dodge(preserve = "single")) +
  facet_wrap(~Interp_DBH_Class, strip.position = "bottom", ncol = 5) +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "bottom", panel.spacing = unit(0, "cm")
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Survey Year") +
  ylab("Count of Whitebark Pine Trees") +
  labs(title = "Cone Presence observed by DBH by Survey Year")
cone_presence + guides(fill = guide_legend(title = "Cone Presence")) # Call plot; take a look!

ggsave("output/figures/ConePresenceCounts_wide_LMDMay2025.png",
  plot = last_plot(), width = 8,
  height = 6.5, units = c("in")
)
ggsave("output/figures/ConePresenceCounts_tall_LMDMay2025.png",
  plot = last_plot(), width = 6.5,
  height = 8, units = c("in")
)

## Part B. Beetle Presence ####

grte.beetle_presence <- grte %>% drop_na(MtnPineBeetle) # remove NAs

beetle_presence <- ggplot(
  data = subset(grte.beetle_presence, TreeStatus == "Alive"),
  aes(x = SurveyYear, fill = MtnPineBeetle)
) +
  geom_bar(stat = "count", position = position_dodge(preserve = "single")) +
  facet_wrap(~Interp_DBH_Class, strip.position = "bottom", ncol = 5) +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "bottom", panel.spacing = unit(0, "cm")
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Survey Year") +
  ylab("Count of Whitebark Pine Trees") +
  labs(title = "Mountain Pine Beetle Infestations Observed by DBH by Year")
beetle_presence + guides(fill = guide_legend(title = "Mountain Pine Beetle Infestation Detected")) # Call plot; take a look!

ggsave("output/figures/BeetlePresence_wide_LMDMay2025.png",
  plot = last_plot(), width = 8,
  height = 6.5, units = c("in")
)

## Part C. Blister Rust ####

grte.blisterrust <- grte %>% drop_na(RustInfection) # remove NAs

overall_BR <- ggplot(
  data = subset(grte.blisterrust, TreeStatus == "Alive"),
  aes(x = SurveyYear, fill = RustInfection)
) +
  geom_bar(stat = "count", position = position_dodge(preserve = "single")) +
  facet_wrap(~Interp_DBH_Class, strip.position = "bottom", ncol = 5) +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "bottom", panel.spacing = unit(0, "cm")
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Survey Year") +
  ylab("Count of Whitebark Pine Trees") +
  labs(title = "Blister Rust Infections observed by DBH by Year")
overall_BR + guides(fill = guide_legend(title = "Blister Rust Infection Detected"))

# Export:
ggsave("output/figures/OverallBlisterRust_wide_LMDMay2025.png",
  plot = last_plot(), width = 8,
  height = 6.5, units = c("in")
)
ggsave("output/figures/OverallBlisterRust_tall_LMDMay2025.png",
  plot = last_plot(), width = 6.5,
  height = 8, units = c("in")
)

stand_BR <- ggplot(
  data = subset(grte.blisterrust, TreeStatus == "Alive"),
  aes(x = SurveyYear, fill = RustInfection)
) +
  geom_bar(stat = "count", position = position_dodge(preserve = "single")) +
  facet_wrap(~Stand_ID, strip.position = "bottom", ncol = 5) +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "bottom", panel.spacing = unit(0, "cm")
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Survey Year") +
  ylab("Count of Whitebark Pine Trees") +
  labs(title = "Blister Rust Infections observed in each Stand by Year")
stand_BR + guides(fill = guide_legend(title = "Blister Rust Infection Detected"))

# Export:
ggsave("output/figures/AllStandsBlisterRust_wide_LMDMay2025.png",
  plot = last_plot(), width = 8,
  height = 6.5, units = c("in")
)
ggsave("output/figures/AllStandsBlisterRust_tall_LMDMay2025.png",
  plot = last_plot(), width = 6.5,
  height = 8, units = c("in")
)

annualstand_BR <- ggplot(
  data = subset(grte.blisterrust, TreeStatus == "Alive" & SamplePanel == "Annual"),
  aes(x = SurveyYear, fill = RustInfection)
) +
  geom_bar(stat = "count", position = position_dodge(preserve = "single")) +
  facet_wrap(~Stand_ID, strip.position = "bottom", ncol = 5) +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "bottom", panel.spacing = unit(0, "cm")
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Survey Year") +
  ylab("Count of Whitebark Pine Trees") +
  labs(title = "Blister Rust Infections observed in each Stand by Year")
annualstand_BR + guides(fill = guide_legend(title = "Blister Rust Infection Detected"))

# Export:
ggsave("output/figures/AnnualStandsBlisterRust_wide_LMDMay2025.png",
       plot = last_plot(), width = 8,
       height = 6.5, units = c("in")
)
ggsave("output/figures/AnnualStandsBlisterRust_tall_LMDMay2025.png",
       plot = last_plot(), width = 6.5,
       height = 8, units = c("in")
)

## Part D. DBH Category ####

grte.dbh_data <- grte %>% subset(Interp_DBH_Class != "no data")

latestDBH <- ggplot(
  data = subset(grte.dbh_data, TreeStatus == "Alive"),
  aes(x = SurveyYear, fill = Interp_DBH_Class)
) +
  geom_bar(stat = "count", position = position_dodge(preserve = "single")) +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Survey Year") +
  ylab("Count of Whitebark Pine Trees") +
  labs(title = "Tree Size Class observed by Year")
latestDBH + guides(fill = guide_legend(title = "Diameter at Breast Height (cm)"))

ggsave("output/figures/TreeSizeClassDBH_wide_LMDMay2025.png",
  plot = last_plot(), width = 8,
  height = 6.5, units = c("in")
)
ggsave("output/figures/TreeSizeClassDBH_tall_LMDMay2025.png",
  plot = last_plot(), width = 6.5,
  height = 8, units = c("in")
)

latestDBH_stand <- ggplot(
  data = subset(grte.dbh_data, TreeStatus == "Alive"),
  aes(x = SurveyYear, fill = Interp_DBH_Class)
) +
  geom_bar(stat = "count", position = position_dodge(preserve = "single")) +
  facet_wrap(~Stand_ID, strip.position = "bottom", ncol = 5) +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Survey Year") +
  ylab("Count of Whitebark Pine Trees") +
  labs(title = "Tree Size Class observed in each Stand by Year")
latestDBH_stand + guides(fill = guide_legend(title = "Diameter at Breast Height (cm)"))

ggsave("output/figures/TreeSizeClassDBH_byStand_wide_LMDMay2025.png",
  plot = last_plot(), width = 8,
  height = 6.5, units = c("in")
)
ggsave("output/figures/TreeSizeClassDBH_byStand_tall_LMDMay2025.png",
  plot = last_plot(), width = 6.5,
  height = 8, units = c("in")
)

latestDBH_annualstand <- ggplot(
  data = subset(grte.dbh_data, TreeStatus == "Alive" & SamplePanel == "Annual"),
  aes(x = SurveyYear, fill = Interp_DBH_Class)
) +
  geom_bar(stat = "count", position = position_dodge(preserve = "single")) +
  facet_wrap(~Stand_ID, strip.position = "bottom", ncol = 5) +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Survey Year") +
  ylab("Count of Whitebark Pine Trees") +
  labs(title = "Tree Size Class observed in each Stand by Year")
latestDBH_annualstand + guides(fill = guide_legend(title = "Diameter at Breast Height (cm)"))

# Even though these are annually visited sites that have data recorded every year,
# Not all years will have data presented on this graph! For example, there is
# no data plotted for 2010 at Stewarts Draw; this is because every tree
# surveyed that year was dead, and this graph only presents live tree data.

ggsave("output/figures/TreeSizeClassDBH_byAnnualStand_wide_LMDMay2025.png",
       plot = last_plot(), width = 8,
       height = 6.5, units = c("in")
)
ggsave("output/figures/TreeSizeClassDBH_byAnnualStand_tall_LMDMay2025.png",
       plot = last_plot(), width = 6.5,
       height = 8, units = c("in")
)

## Part E. Tree Status ####

# Note: these plots display the number of trees observed dying in a given time period.

status_dbh <- ggplot(
  data = grte,
  aes(x = SurveyYear, fill = TreeStatus)
) +
  geom_bar(stat = "count", position = position_dodge(preserve = "single")) +
  facet_wrap(~Interp_DBH_Class, strip.position = "bottom", ncol = 5) +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "bottom", panel.spacing = unit(0, "cm")
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Survey Year") +
  ylab("Count of Whitebark Pine Trees") +
  labs(title = "Tree Status observed by DBH each Year")
status_dbh + guides(fill = guide_legend(title = "Tree Status")) # Call plot; take a look!

# Save plots!
ggsave("output/figures/ObservedTreeStatusDBH_wide_LMDMay2025.png",
  plot = last_plot(), width = 8,
  height = 6.5, units = c("in")
)
ggsave("output/figures/ObservedTreeStatusDBH_tall_LMDMay2025.png",
  plot = last_plot(), width = 6.5,
  height = 8, units = c("in")
)

status_stand <- ggplot(
  data = grte,
  aes(x = SurveyYear, fill = TreeStatus)
) +
  geom_bar(stat = "count", position = position_dodge(preserve = "single")) +
  facet_wrap(~Stand_ID, strip.position = "bottom", ncol = 5) +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "bottom", panel.spacing = unit(0, "cm")
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Survey Year") +
  ylab("Count of Whitebark Pine Trees") +
  labs(title = "Tree Status observed by Stand each Year")
status_stand + guides(fill = guide_legend(title = "Tree Status")) # Call plot; take a look!

# Save plots!
ggsave("output/figures/ObservedTreeStatusStand_wide_LMDMay2025.png",
       plot = last_plot(), width = 8,
       height = 6.5, units = c("in")
)
ggsave("output/figures/ObservedTreeStatusStand_tall_LMDMay2025.png",
       plot = last_plot(), width = 6.5,
       height = 8, units = c("in")
)

status_annualstand <- ggplot(
  data = subset(grte, SamplePanel == "Annual"),
  aes(x = SurveyYear, fill = TreeStatus)
) +
  geom_bar(stat = "count", position = position_dodge(preserve = "single")) +
  facet_wrap(~Stand_ID, strip.position = "bottom", ncol = 5) +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "bottom", panel.spacing = unit(0, "cm")
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Survey Year") +
  ylab("Count of Whitebark Pine Trees") +
  labs(title = "Tree Status observed by Stand each Year")
status_annualstand + guides(fill = guide_legend(title = "Tree Status")) # Call plot; take a look!

# Save plots!
ggsave("output/figures/ObservedTreeStatusAnnualStand_wide_LMDMay2025.png",
       plot = last_plot(), width = 8,
       height = 6.5, units = c("in")
)
ggsave("output/figures/ObservedTreeStatusAnnualStand_tall_LMDMay2025.png",
       plot = last_plot(), width = 6.5,
       height = 8, units = c("in")
)

status_time <- ggplot(
  data = grte,
  aes(x = SurveyYear, fill = TreeStatus)
) +
  geom_bar(stat = "count", position = position_dodge(preserve = "single")) +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "bottom", panel.spacing = unit(0, "cm")
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Survey Year") +
  ylab("Count of Whitebark Pine Trees") +
  labs(title = "Tree Status observed each Year")
status_time + guides(fill = guide_legend(title = "Tree Status")) # Call plot; take a look!

# Save plots!
ggsave("output/figures/ObservedTreeStatusTime_wide_LMDMay2025.png",
  plot = last_plot(), width = 8,
  height = 6.5, units = c("in")
)
ggsave("output/figures/ObservedTreeStatusTime_tall_LMDMay2025.png",
  plot = last_plot(), width = 6.5,
  height = 8, units = c("in")
)

## Part F. Tree Condition ####

# Sort data in order and backfill NA values in RustInfection/MtnPineBeetle with last-known value:
grte <- grte %>% arrange(Stand_ID, Tree_ID, ClumpMembership, SurveyYear)

grte$Rust_Backfill <- grte$RustInfection
grte <- grte %>% fill(Rust_Backfill, .direction = "down")
with(subset(grte, TreeStatus == "Dead"), table(Rust_Backfill, useNA = "always"))

grte$MPB_Backfill <- grte$MtnPineBeetle
grte <- grte %>% fill(MPB_Backfill, .direction = "down")
with(subset(grte, TreeStatus == "Dead"), table(MPB_Backfill, useNA = "always"))

with(subset(grte, TreeStatus == "Dead"), table(MPB_Backfill, Rust_Backfill, useNA = "always"))

# Create "Condition" to describe what tree had when it died:
grte_condition <- grte %>% mutate(
  Condition =
    case_when(
      MPB_Backfill == "Y" & Rust_Backfill == "Y" ~ "Combination",
      MPB_Backfill == "Y" & Rust_Backfill == "N" ~ "Mountain Pine Beetle",
      MPB_Backfill == "N" & Rust_Backfill == "Y" ~ "Blister Rust",
      MPB_Backfill == "N" & Rust_Backfill == "N" ~ "Other"
    )
)

with(grte_condition, table(TreeStatus, Condition, useNA = "always"))

grte_condition$Condition <- factor(grte_condition$Condition, levels = c("Mountain Pine Beetle", "Blister Rust", "Combination", "Other"))

with(grte_condition, table(TreeStatus, Condition))

# Make Plots!
condition_dbhyear <- ggplot(
  data = subset(grte_condition, TreeStatus == "Dead"),
  aes(x = SurveyYear, fill = Condition)
) +
  geom_bar(stat = "count", position = position_dodge(preserve = "single")) +
  facet_wrap(~Interp_DBH_Class, strip.position = "bottom", ncol = 5) +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "bottom", panel.spacing = unit(0, "cm"), legend.title = element_blank()
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Diameter at Breast Height (cm)") +
  ylab("Count of Dead Whitebark Pine Trees") +
  labs(title = "Conditions observed each Year by DBH on Dead Trees")
condition_dbhyear # Call plot; take a look!

# Save plots!
ggsave("output/figures/ConditionsDBHYear_wide_LMDMay2025.png",
  plot = last_plot(), width = 8,
  height = 6.5, units = c("in")
)
ggsave("output/figures/ConditionsDBHYear_tall_LMDMay2025.png",
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
  xlab("SurveyYear") +
  ylab("Count of Dead Whitebark Pine Trees") +
  labs(title = "Conditions observed each Year on Dead Trees")
condition_year # Call plot; take a look!

# Save plots!
ggsave("output/figures/ConditionsYear_wide_LMDMay2025.png",
  plot = last_plot(), width = 8,
  height = 6.5, units = c("in")
)
ggsave("output/figures/ConditionsYear_tall_LMDMay2025.png",
  plot = last_plot(), width = 6.5,
  height = 8, units = c("in")
)

condition_dbh <- ggplot(
  data = subset(grte_condition, TreeStatus == "Dead"),
  aes(x = Interp_DBH_Class, fill = Condition)
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
  labs(title = "Conditions observed by DBH on Dead Trees")
condition_dbh # Call plot; take a look!

# Save plots!
ggsave("output/figures/ConditionsDBH_wide_LMDMay2025.png",
  plot = last_plot(), width = 8,
  height = 6.5, units = c("in")
)
ggsave("output/figures/ConditionsDBH_tall_LMDMay2025.png",
  plot = last_plot(), width = 6.5,
  height = 8, units = c("in")
)

# END ----