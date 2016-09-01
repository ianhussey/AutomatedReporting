########################################################################
# Automated mixed within-between anova and ηg2 reporting, for use in knittr scripts
# Ian Hussey

# all results checked against those returned by JASP

# usage:
# 1. customise the working directory line, containing setwd() below
# 2. run script

# to do:
# rounding of F and p values needs to be more adaptive, e.g., including < values.
# confidence intervals needed for ηg2. However, I'm not aware of a package that does this.

########################################################################
# Clean workspace
rm(list=ls())

########################################################################
# dependencies 
library(lsr)  # for eta2
library(MBESS)  # for 95% CI on eta2
library(dplyr)
library(tidyr)
library(ez)
library(psych)  # for describeBy()
library(weights)  # for rd(), a round() alternative 

########################################################################
# data acquisition
setwd("~/git/Automated Reporting/")
data_df <- read.csv("dataset.csv")

########################################################################
# reshape data for analysis
reshaped_df <- 
  data_df %>%
  gather(timepoint, outcome_variable, c(timepoint_1, timepoint_2))

#convert participant code to factor
reshaped_df$participant <- factor(reshaped_df$participant)

# 2X2 mixed within-between anova
# outcome_variable as DV, condition as between subjects IV and timepoint_1 as within subjects IV
# ezANOVA makes it easier to specify the model than aov() and easier to access the returned list. 
anova <- ezANOVA(data = reshaped_df,
                 dv = outcome_variable,
                 within = timepoint,
                 between = condition,
                 wid = participant,
                 type = 3)

# descriptive stats by cell
descriptives <-
  reshaped_df %>%
  select(condition, timepoint, outcome_variable) %>%
  describeBy(list(reshaped_df$condition, reshaped_df$timepoint),  # do seperate stats for these cells
             fast=TRUE,  # subset of descriptive stats
             ranges = FALSE,
             trim=0) 

########################################################################
## extract individual stats

# anova results
# nb generalised eta squared is returned here because it has equivalent interpretations
# and is unaffected by the factorial design, whereas eta2 and partial eta2 are.
# See Olejnik & Algina (2003), Bakeman (2005), and Lakens (2013).
anova_main_effect_condition_df1   <-  round(anova$ANOVA$DFn[1], 2)
anova_main_effect_condition_df2   <-  round(anova$ANOVA$DFd[1], 2)
anova_main_effect_condition_F     <-  round(anova$ANOVA$F[1], 2)
anova_main_effect_condition_p     <-  round(anova$ANOVA$p[1], 5)
anova_main_effect_condition_geta2 <-  round(anova$ANOVA$ges[1], 2)

anova_main_effect_timepoint_df1   <-  round(anova$ANOVA$DFn[2], 2)
anova_main_effect_timepoint_df2   <-  round(anova$ANOVA$DFd[2], 2)
anova_main_effect_timepoint_F     <-  round(anova$ANOVA$F[2], 2)
anova_main_effect_timepoint_p     <-  round(anova$ANOVA$p[2], 5)
anova_main_effect_timepoint_geta2 <-  round(anova$ANOVA$ges[2], 2)

anova_interaction_effect_df1      <-  round(anova$ANOVA$DFn[3], 2)
anova_interaction_effect_df2      <-  round(anova$ANOVA$DFd[3], 2)
anova_interaction_effect_F        <-  round(anova$ANOVA$F[3], 2)
anova_interaction_effect_p        <-  round(anova$ANOVA$p[3], 5)
anova_interaction_effect_geta2    <-  round(anova$ANOVA$ges[3], 2)

# NHST
# condition main effect
if (anova_main_effect_condition_p < 0.05) {
  condition_significance      <- "A significant main effect was found for condition, "
} else {
  condition_significance      <- "No main effect was found for condition, "
}

# timepoint main effect
if (anova_main_effect_timepoint_p < 0.05) {
  timepoint_significance      <- "A significant main effect was found for time point, "
} else {
  timepoint_significance      <- "No main effect was found for time point, "
}

# interaction effect
if (anova_interaction_effect_p < 0.05) {
  interaction_significance    <- "A significant condition*time point interaction effect was found, "
} else {
  interaction_significance    <- "No condition*time point interaction effect was found, "
}

# round p values using APA rules
# condition main effect
if (anova_main_effect_condition_p < 0.001) {
  anova_main_effect_condition_p_APA_format <- "< .001"
} else if (anova_main_effect_condition_p < 0.01) {
  anova_main_effect_condition_p_APA_format <- paste("= ", rd(anova_main_effect_condition_p, 3), sep = "")  # rd() rounds, converts to string, and removes the leading 0.
} else {
  anova_main_effect_condition_p_APA_format <- paste("= ", rd(anova_main_effect_condition_p, 2), sep = "")
}

# timepoint main effect
if (anova_main_effect_timepoint_p < 0.001) {
  anova_main_effect_timepoint_p_APA_format <- "< .001"
} else if (anova_main_effect_timepoint_p < 0.01) {
  anova_main_effect_timepoint_p_APA_format <- paste("= ", rd(anova_main_effect_timepoint_p, 3), sep = "")  # rd() rounds, converts to string, and removes the leading 0.
} else {
  anova_main_effect_timepoint_p_APA_format <- paste("= ", rd(anova_main_effect_timepoint_p, 2), sep = "")
}

# interaction effect
if (anova_interaction_effect_p < 0.001) {
  anova_interaction_effect_p_APA_format <- "< .001"
} else if (anova_interaction_effect_p < 0.01) {
  anova_interaction_effect_p_APA_format <- paste("= ", rd(anova_interaction_effect_p, 3), sep = "")  # rd() rounds, converts to string, and removes the leading 0.
} else {
  anova_interaction_effect_p_APA_format <- paste("= ", rd(anova_interaction_effect_p, 2), sep = "")
}

# descriptive stats
mean_condition_a_timepoint_1  <- round(descriptives[[1]][["mean"]][[3]], 2)
mean_condition_b_timepoint_1  <- round(descriptives[[2]][["mean"]][[3]], 2)
mean_condition_a_timepoint_2  <- round(descriptives[[3]][["mean"]][[3]], 2)
mean_condition_b_timepoint_2  <- round(descriptives[[4]][["mean"]][[3]], 2)

sd_condition_a_timepoint_1    <- round(descriptives[[1]][["sd"]][[3]], 2)
sd_condition_b_timepoint_1    <- round(descriptives[[2]][["sd"]][[3]], 2)
sd_condition_a_timepoint_2    <- round(descriptives[[3]][["sd"]][[3]], 2)
sd_condition_b_timepoint_2    <- round(descriptives[[4]][["sd"]][[3]], 2)

n_condition_a_timepoint_1     <- round(descriptives[[1]][["n"]][[3]], 2)
n_condition_b_timepoint_1     <- round(descriptives[[2]][["n"]][[3]], 2)
n_condition_a_timepoint_2     <- round(descriptives[[3]][["n"]][[3]], 2)
n_condition_b_timepoint_2     <- round(descriptives[[4]][["n"]][[3]], 2)

########################################################################
## report stats

anova_setup <- "A mixed within-between ANOVA was conducted with the outcome variable as DV, condition as the between subjects IV, and time point as the within subjects IV. "

# anova output
anova_main_effect_condition_output    <- paste("F(", anova_main_effect_condition_df1, ", ", anova_main_effect_condition_df2, ") = ", anova_main_effect_condition_F, ", p ", anova_main_effect_condition_p_APA_format, ", ηg2 = ", anova_main_effect_condition_geta2, ". ", sep = "") 
anova_main_effect_timepoint_output    <- paste("F(", anova_main_effect_timepoint_df1,", ", anova_main_effect_timepoint_df2,") = ", anova_main_effect_timepoint_F, ", p ", anova_main_effect_timepoint_p_APA_format, ", ηg2 = ", anova_main_effect_timepoint_geta2, ". ", sep = "")  
anova_interaction_effect_output       <- paste("F(", anova_interaction_effect_df1, ", ", anova_interaction_effect_df2, ") = ", anova_interaction_effect_F, ", p ", anova_interaction_effect_p_APA_format, ", ηg2 = ", anova_interaction_effect_geta2, ". ", sep = "") 

# descriptive stats output
desc_condition_A_timepoint_1          <- paste("condition A timepoint 1: n = ", n_condition_a_timepoint_1, ", M = ", mean_condition_a_timepoint_1, ", SD = ", sd_condition_a_timepoint_1, sep = "") 
desc_condition_B_timepoint_1          <- paste("condition B timepoint 1: n = ", n_condition_b_timepoint_1, ", M = ", mean_condition_b_timepoint_1, ", SD = ", sd_condition_b_timepoint_1, sep = "") 
desc_condition_A_timepoint_2          <- paste("condition A timepoint 2: n = ", n_condition_a_timepoint_2, ", M = ", mean_condition_a_timepoint_2, ", SD = ", sd_condition_a_timepoint_2, sep = "") 
desc_condition_B_timepoint_2          <- paste("condition B timepoint 2: n = ", n_condition_b_timepoint_2, ", M = ", mean_condition_b_timepoint_2, ", SD = ", sd_condition_b_timepoint_2, sep = "") 

## final summary
anova_output_and_interpretation       <- paste(anova_setup, condition_significance, anova_main_effect_condition_output, timepoint_significance, anova_main_effect_timepoint_output, interaction_significance, anova_interaction_effect_output, sep = "")

## write to disk
sink("output ANOVA mixed within-between.txt")
cat(anova_output_and_interpretation)  # cat() supresses the line number from being printed
cat("\n")
cat("\n")
cat(desc_condition_A_timepoint_1)
cat("\n")
cat(desc_condition_B_timepoint_1)
cat("\n")
cat(desc_condition_A_timepoint_2)
cat("\n")
cat(desc_condition_B_timepoint_2)
sink()

