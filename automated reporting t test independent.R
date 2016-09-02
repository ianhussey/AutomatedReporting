########################################################################
# Automated reporting of an independent t test and cohen's d
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
t_test <- t.test(formula = change_score ~ condition,  # IV ~ DV adjusted here
                 data = data_df, 
                 alternative = "two.sided",
                 paired = FALSE)

# effect size
cohens_d <- cohen.d(change_score ~ condition, 
                    data = data_df,
                    paired = FALSE)

# descriptive stats by cell
descriptives <-
  data_df %>%
  select(condition, change_score) %>%
  describeBy(data_df$condition,  # do seperate stats for these cells
             fast=TRUE,  # subset of descriptive stats
             ranges = FALSE,
             trim=0) 

########################################################################
## extract individual stats

# t test
t_test_est        <- round(t_test$statistic[[1]], 2)
t_test_df         <- round(t_test$parameter[[1]], 2)
t_test_p          <- round(t_test$p.value[[1]], 5)

# effect size
d_est             <- round(cohens_d$estimate[[1]], 2)
d_ci_lower        <- round(cohens_d$conf.int[["inf"]], 2)
d_ci_upper        <- round(cohens_d$conf.int[["sup"]], 2)
d_interpretation  <- cohens_d$magnitude[[1]]

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
  significance <- paste("An independent t test demonstrated significant differences of ", d_interpretation, " effect size between ", sep = "")
} else {
  significance <- "An independent t test demonstrated no significant differences between "
}
  
# descriptive stats
mean_condition_a <- round(descriptives[[1]][["mean"]][[2]], 2)
mean_condition_b <- round(descriptives[[2]][["mean"]][[2]], 2)

sd_condition_a <- round(descriptives[[1]][["sd"]][[2]], 2)
sd_condition_b <- round(descriptives[[2]][["sd"]][[2]], 2)

n_condition_a <- round(descriptives[[1]][["n"]][[2]], 2)
n_condition_b <- round(descriptives[[2]][["n"]][[2]], 2)


########################################################################
## report stats

# t test and d
t_test_and_d_output <- paste(", t(", t_test_df, ") = ", t_test_est, ", p ", t_test_p_APA_format, ", d = ", d_est, ", 95% CI [", d_ci_lower, ", ", d_ci_upper, "]. ", sep = "")

# descriptive stats
descriptives_condition_a_output <- paste("condition A (n = ", n_condition_a, ", M = ", mean_condition_a, ", SD = ", sd_condition_a, ")", sep = "")
descriptives_condition_b_output <- paste("condition B (n = ", n_condition_b, ", M = ", mean_condition_b, ", SD = ", sd_condition_b, ")", sep = "")

## final summary
t_test_output_and_interpretation <- paste(significance, descriptives_condition_a_output, " and ", descriptives_condition_b_output, t_test_and_d_output, sep = "")

## write to disk
sink("output t test independent.txt")
cat(t_test_output_and_interpretation)  # cat() supresses the line number from being printed
sink()


