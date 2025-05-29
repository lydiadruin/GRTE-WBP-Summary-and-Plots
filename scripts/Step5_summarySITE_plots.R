### Graphical Summaries of Site Based Data ###
## Lydia M. Druin ##
# GRTE WBP Summary and Plots #

# Stored as "scripts/Step5_summarySITE_plots.R" #
# Updated 05/08/25 #

### Purpose ###
# This code is for generating graphical summary review data of the understory
# of monitored whitebark pine stands in Grand Tetons National Park #

### Note to User ###
# You MUST run "Step1_data_joining.R" and complete processing steps BEFORE
# running this code and "Step2_data_processing.R as well as
# "Step3_summary_numeric.R" to generate the understory data.

### Note to User ###

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

sourcefile = "Understory_SiteBasedData_LMDMay2025.csv"
# Note to user: If you are incorporating post-2023 data, this is a point where
# you need to change the above file name to *your* new file name! Make sure it is
# saved as a .csv file, and copy+paste the file name into the above line while
# retaining the quotation marks around the file name + ".csv" at the end!

path <- "data/processed_data"

sourcepath <- paste(path, sourcefile, sep = "/")
understory <- read.csv(sourcepath,
                 header = TRUE)

glimpse(understory) # glimpse at the data--make sure it is what you expect!

# Step 2. Plot Stand Characteristics ####

## Part A. Format Stand Characteristic Data ####

sitechars <- c("elev_meters", "slope_percent", "aspect_degrees")

# We want to restrict the data to one record per site per survey year
sitedata <- understory[order(understory$SurveyYear, decreasing = TRUE), ] # Sort data
sitedata <- sitedata[!duplicated(sitedata$Stand_ID), ] # Remove duplicate transects in each sampling period

## Part B. Plot Stand Characteristics Distribution ####
par(mfrow = c(1, 3), mar = c(2, 2, 2, 2))
for (n in 1:length(sitechars)) {
  hist(sitedata[, sitechars[n]], main = sitechars[n], xlab = "")
  abline(v = mean(sitedata[, sitechars[n]]), col = "blue")
  print(c(
    sitechars[n], min(sitedata[, sitechars[n]], na.rm = TRUE),
    mean(sitedata[, sitechars[n]], na.rm = TRUE),
    max(sitedata[, sitechars[n]], na.rm = TRUE)
  ))
}

# Must save plot manually with export option in R studio

# Step 3. Plot Understory Data ####

## Part A. Seedling/Sapling Data ####

seedlingcounts_stand = understory %>%
  select(SeedSap_BR_Absent, SeedSap_BR_PresentUnknown, Stand_ID) %>%
  group_by(Stand_ID) %>%
  summarize(Absent = mean(SeedSap_BR_Absent),
            Present = mean(SeedSap_BR_PresentUnknown)) %>%
  ungroup() %>%
  pivot_longer(-Stand_ID) %>%
  ggplot(aes(y = Stand_ID, x = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "bottom", panel.spacing = unit(0, "cm")
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Mean Count of Whitebark Pine Trees <140cm") +
  ylab("Stand") +
  labs(title = "Mean Count of Small Whitebark Pine Trees by Blister Rust Status")
seedlingcounts_stand + guides(fill = guide_legend(title = "Blister Rust")) # Call plot; take a look!

# Save plots!
ggsave("output/figures/SeedSapCounts_RustStatus_Stand_wide_LMDMay2025.png",
       plot = last_plot(), width = 8,
       height = 6.5, units = c("in")
)
ggsave("output/figures/SeedSapCounts_RustStatus_Stand_tall_LMDMay2025.png",
       plot = last_plot(), width = 6.5,
       height = 8, units = c("in")
)

seedlingcounts_year = understory %>%
  select(SeedSap_BR_Absent, SeedSap_BR_PresentUnknown, SurveyYear) %>%
  group_by(SurveyYear) %>%
  summarize(Absent = mean(SeedSap_BR_Absent),
            Present = mean(SeedSap_BR_PresentUnknown)) %>%
  ungroup() %>%
  pivot_longer(-SurveyYear) %>%
  ggplot(aes(x = SurveyYear, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "bottom", panel.spacing = unit(0, "cm")
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Survey Year") +
  ylab("Mean Count of Whitebark Pine Trees <140cm") +
  labs(title = "Mean Count of Small Whitebark Pine Trees by Blister Rust Status")
seedlingcounts_year + guides(fill = guide_legend(title = "Blister Rust")) # Call plot; take a look!

# Save plots!
ggsave("output/figures/SeedSapCounts_RustStatus_Year_wide_LMDMay2025.png",
       plot = last_plot(), width = 8,
       height = 6.5, units = c("in")
)
ggsave("output/figures/SeedSapCounts_RustStatus_Year_tall_LMDMay2025.png",
       plot = last_plot(), width = 6.5,
       height = 8, units = c("in")
)

## Part B. Midden Data ####

# Fix weird Stand_ID for Lake Taminah first:
understory <- understory %>% mutate(Midden_Excav = recode(Midden_Excav, ">10" = "15"))
understory$Midden_Excav = as.numeric(understory$Midden_Excav)

midden_stand = understory %>%
  select(Midden_Undis, Midden_Excav, Stand_ID) %>%
  group_by(Stand_ID) %>%
  summarize(Undisturbed = mean(Midden_Undis, na.rm = TRUE),
            Excavated = mean(Midden_Excav, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_longer(-Stand_ID) %>%
  ggplot(aes(y = Stand_ID, x = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "bottom", panel.spacing = unit(0, "cm")
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Mean Count of Middens") +
  ylab("Stand") +
  labs(title = "Mean Count of Middens in each Stand")
midden_stand # Call plot; take a look!

# Save plots!
ggsave("output/figures/MeanMiddenCounts_Stand_wide_LMDMay2025.png",
       plot = last_plot(), width = 8,
       height = 6.5, units = c("in")
)
ggsave("output/figures/MeanMiddenCounts_Stand_tall_LMDMay2025.png",
       plot = last_plot(), width = 6.5,
       height = 8, units = c("in")
)

midden_year = understory %>%
  select(Midden_Undis, Midden_Excav, SurveyYear) %>%
  group_by(SurveyYear) %>%
  summarize(Undisturbed = mean(Midden_Undis, na.rm = TRUE),
            Excavated = mean(Midden_Excav, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_longer(-SurveyYear) %>%
  ggplot(aes(x = SurveyYear, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_grey() +
  theme(
    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    legend.position = "bottom", panel.spacing = unit(0, "cm")
  ) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Survey Year") +
  ylab("Mean Count of Middens") +
  labs(title = "Mean Count of Middens by Survey Year")
midden_year  # Call plot; take a look!

# Save plots!
ggsave("output/figures/MeanMiddenCounts_Year_wide_LMDMay2025.png",
       plot = last_plot(), width = 8,
       height = 6.5, units = c("in")
)
ggsave("output/figures/MeanMiddenCounts_Year_tall_LMDMay2025.png",
       plot = last_plot(), width = 6.5,
       height = 8, units = c("in")
)

# END ----