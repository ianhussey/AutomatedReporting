########################################################################
# Automated reporting of an independent t test and cohen's d
# for use in knittr scripts

# Ian Hussey (ian.hussey@ugent.be)

# output checked against results returned by JASP

# usage:
# 1. customise the working directory line, containing setwd() below
# 2. run script

# to do:
# report if one sided t test
# 

########################################################################
# Clean workspace
rm(list=ls())

########################################################################
# dependencies 
library(dplyr)
library(effsize)
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
desc_stats <- 
  ez::ezStats(data = data_df,
              dv = .(change_score), 
              wid = .(participant),
              between = .(condition),
              type = 3) %>%
  mutate(Mean = round(Mean, 2),  # round for later reporting
         SD = round(SD, 2)) %>%
  select(-FLSD)

########################################################################
## extract individual stats

# ideally this would be left to schoRsch but D returns NA
#my_t_test_output <- schoRsch::t_out(my_t_test, 
#                                    n.equal = TRUE,
#                                    welch.df.exact = TRUE, 
#                                    welch.n = NA,
#                                    d.corr = FALSE, 
#                                    print = TRUE)
#returns: d = NA for some reason. 

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
t_test_p <- ifelse(t_test_p < 0.001, "< .001", 
                   ifelse(t_test_p < 0.01,
                          paste("= ", rd(t_test_p, 3), sep = ""),  # rd() rounds, converts to string, and removes the leading 0.
                          paste("= ", rd(t_test_p, 2), sep = "")))

## descriptives
a_m   <- desc_stats %>% filter(condition == "a") %>% .$Mean  # convert df to individual variables
b_m   <- desc_stats %>% filter(condition == "b") %>% .$Mean
a_sd  <- desc_stats %>% filter(condition == "a") %>% .$SD
b_sd  <- desc_stats %>% filter(condition == "b") %>% .$SD
a_n   <- desc_stats %>% filter(condition == "a") %>% .$N
b_n   <- desc_stats %>% filter(condition == "b") %>% .$N

########################################################################
# convert output to natural langauge

nhst <- ifelse(t_test_p < 0.05, 
               paste("An independent t test demonstrated significant differences of ", d_interpretation, " effect size between ", sep = ""),
               paste("An independent t test demonstrated non-significant differences of ", d_interpretation, " effect size between ", sep = ""))

# t test and d
t_test_output <- paste(", t(", t_test_df, ") = ", t_test_est, ", p ", t_test_p, ", d = ", d_est, ", 95% CI [", d_ci_lower, ", ", d_ci_upper, "]. ", sep = "")

# descriptive stats
desc_a <- paste("condition A (n = ", a_n, ", M = ", a_m, ", SD = ", a_sd, ")", sep = "")
desc_b <- paste("condition B (n = ", b_n, ", M = ", b_m, ", SD = ", b_sd, ")", sep = "")

########################################################################
## combine and write to disk

## final summary
t_test_text <- paste(nhst, 
                     desc_a, 
                     " and ", 
                     desc_b, 
                     t_test_output, 
                     sep = "")

## write to disk
sink("output t test independent.txt")
cat(t_test_text)  # cat() supresses the line number from being printed
sink()

