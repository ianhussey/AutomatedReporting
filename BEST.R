###################################################################
# Automated reporting of BEST test (bayesian t test alternative)
# for use in knittr scripts

# Author: Ian Hussey (ian.hussey@ugent.be)

# NB USES A NON INFORMATIVE PRIOR BY DEFAULT, SEE KRUSHKE 2013

###################################################################
# Clean the workspace
rm(list=ls())

###################################################################
## Dependencies
library(BEST)
library(dplyr)

###################################################################
## Data acquisition
setwd("~/git/Automated Reporting/")
data_df <- read.csv("dataset.csv")

###################################################################
# analyses

# BEST
# see http://sumsar.net/blog/2014/02/bayesian-first-aid-two-sample-t-test/
## NB USES A NON-INFORMATIVE PRIOR BY DEFAULT
attach(data_df)
BEST <- BESTmcmc(change_score[condition == "a"],
                         change_score[condition == "b"])
detach(data_df)

# save analysis to disk 
save(BEST, file="BEST.RData")
# Load previously saved analysis from disk
#load(file="BEST.RData")

# define ROPE and tidy up output
BEST_output_df <- 
  as.data.frame(summary(BEST, 
                        ROPEeff = c(-0.2,0.2))) %>%  # ROPE defined here
  tibble::rownames_to_column() %>%  # convert rowname to column
  mutate(mode = round(mode, 2),  # round values and rename awkwardly named ones
         HDIlo = round(HDIlo, 2),
         HDIup = round(HDIup, 2),
         percent_greater_than_zero = round(`%>compVal`, 2),
         percent_in_rope = round(`%InROPE`, 2),
         mean = round(mean, 2))

########################################################################
## extract individual stats
  
es_mode                   <- BEST_output_df %>% filter(rowname == "effSz") %>% .$mode  # convert df to individual variables
es_hdi_low                <- BEST_output_df %>% filter(rowname == "effSz") %>% .$HDIlo  
es_hdi_high               <- BEST_output_df %>% filter(rowname == "effSz") %>% .$HDIup  
es_percent_greater_zero   <- BEST_output_df %>% filter(rowname == "effSz") %>% .$percent_greater_than_zero  
es_percent_in_rope        <- BEST_output_df %>% filter(rowname == "effSz") %>% .$percent_in_rope  
m_condition_a             <- BEST_output_df %>% filter(rowname == "mu1") %>% .$mean 
m_condition_b             <- BEST_output_df %>% filter(rowname == "mu2") %>% .$mean 

########################################################################
# convert output to natural langauge

es_size <- ifelse(abs(es_mode) < 0.2, "negligable", 
                  ifelse(abs(es_mode) < 0.5, "small", 
                         ifelse(abs(es_mode) < 0.8, "medium", "large"))) 

es_hid_includes_zero <- 
  ifelse(m_condition_a < m_condition_b & m_condition_b < 0, "did not overlap zero",  # if (a,b) < 0
         ifelse(m_condition_a > m_condition_b & m_condition_a > 0, "did not overlap zero", # if 0 < (a,b)
                "overlapped zero"))  # if a < 0 < b

conclusion <- 
  ifelse(m_condition_a < m_condition_b & m_condition_b < 0, "credible differences",  # if (a,b) < 0
         ifelse(m_condition_a > m_condition_b & m_condition_a > 0, "credible differences", # if 0 < (a,b)
                "no credible differences"))  # if a < 0 < b

BEST_text <- sprintf("A Bayesian BEST test was used to compare differences between the two conditions. We employed the default non-informative prior and parameters described by Krushke (2013). A region of practical equivalence (ROPE) was defined on the effect size (-0.2 < d < 0.2) in order to allow us to assess group equality as well as group differences. The posterior probabilities indicated that %s%% of credible effect sizes were greater than 0, and %s%% were within the ROPE. The most probable effect size was of %s size with a high density interval that %s, Mode d = %s, 95%% HDI [%s, %s]. We therefore concluded that %s existed between the low condition (M = %s) and high condition (M = %s).", 
                     es_percent_greater_zero, 
                     es_percent_in_rope, 
                     es_size, 
                     es_hid_includes_zero, 
                     es_mode,
                     es_hdi_low, 
                     es_hdi_high, 
                     conclusion, 
                     m_condition_a, 
                     m_condition_b)

# write data to disk
sink("output BEST.txt")
BEST_output_df
cat("\n")
cat("\n")
cat(BEST_text)
sink()

# plot
plotAll(BEST,  
        ROPEeff=c(-0.2,0.2),
        showCurve = TRUE)

