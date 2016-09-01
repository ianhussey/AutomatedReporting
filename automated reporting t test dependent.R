########################################################################
# Automated reporting of a dependent t test and cohen's d
# for use in knittr scripts

# Ian Hussey (ian.hussey@ugent.be)

# output checked against results returned by JASP

# usage:
# 1. customise the working directory line, containing setwd() below
# 2. run script

# to do:
# report if one sided ttest

########################################################################
# Clean workspace
rm(list=ls())

########################################################################
# dependencies 
library(dplyr)
library(effsize)
library(psych)  # for describeBy()
library(weights)  # for rd(), a round() alternative 

########################################################################
# data acquisition
setwd("~/git/Automated Reporting/")
data_df <- read.csv("dataset.csv")

########################################################################
## tests
# t test
t_test <- t.test(x = data_df$timepoint_1, 
                 y = data_df$timepoint_2,
                 alternative = "two.sided",
                 paired = TRUE,
                 data = data_df)

# effect size
cohens_d <- cohen.d(d = data_df$timepoint_1, 
                    f = data_df$timepoint_2,
                    data = data_df,
                    paired = TRUE)

# descriptive stats by cell
descriptives <-
  data_df %>%
  select(condition, timepoint_1, timepoint_2) %>%
  psych::describe(fast=TRUE,  # subset of descriptive stats
                  ranges = FALSE,
                  trim=0) 

########################################################################
## extract individual stats

# t test
t_test_est <- round(t_test$statistic[[1]], 2)
t_test_df <- round(t_test$parameter[[1]], 2)
t_test_p <- round(t_test$p.value[[1]], 5)

# effect size
d_est <- round(cohens_d$estimate[[1]], 2)
d_ci_lower <- round(cohens_d$conf.int[["inf"]], 2)
d_ci_upper <- round(cohens_d$conf.int[["sup"]], 2)
d_interpretation <- cohens_d$magnitude[[1]]

# round p values using APA rules
if (t_test_p < 0.001) {
  t_test_p_APA_format <- "< .001"
} else if (t_test_p < 0.01) {
  t_test_p_APA_format <- paste("= ", rd(t_test_p, 3), sep = "")  # rd() rounds, converts to string, and removes the leading 0.
} else {
  t_test_p_APA_format <- paste("= ", rd(t_test_p, 2), sep = "")
}

# NHST
if (t_test_p < 0.05) {
  significance <- paste("A dependent t test demonstrated significant differences of ", d_interpretation, " effect size between ", sep = "")
} else {
  significance <- "A dependent t test demonstrated no significant differences between "
}
  
# descriptive stats

mean_timepoint_1 <- round(descriptives[["mean"]][[2]], 2)
mean_timepoint_2 <- round(descriptives[["mean"]][[3]], 2)

sd_timepoint_1 <- round(descriptives[["sd"]][[2]], 2)
sd_timepoint_2 <- round(descriptives[["sd"]][[3]], 2)

########################################################################
## report stats

# t test and d
t_test_and_d_output <- paste(", t(", t_test_df, ") = ", t_test_est, ", p ", t_test_p_APA_format, ", d = ", d_est, ", 95% CI [", d_ci_lower, ", ", d_ci_upper, "]. ", sep = "")

# descriptive stats
descriptives_timepoint_1_output <- paste("time point 1 (M = ", mean_timepoint_1, ", SD = ", sd_timepoint_1, ")", sep = "")
descriptives_timepoint_2_output <- paste("time point 2 (M = ", mean_timepoint_2, ", SD = ", sd_timepoint_2, ")", sep = "")

## final summary
t_test_output_and_interpretation <- paste(significance, descriptives_timepoint_1_output, " and ", descriptives_timepoint_2_output, t_test_and_d_output, sep = "")

## write to disk
sink("output t test dependent.txt")
cat(t_test_output_and_interpretation)  # cat() supresses the line number from being printed
sink()


