########################################################################
# Automated mixed within-between anova reporting
# Ian Hussey

# checked against results returned by JASP

# usage:
# 1. customise the working directory line, containing setwd() below
# 2. run script

# to do:
# none

########################################################################
# Clean workspace

rm(list=ls())

########################################################################
# dependencies 

library(dplyr)
library(tidyr)
library(ez)  # for ezANOVA
library(schoRsch)  # for automated reporting of ez output
library(stringr)

########################################################################
# data acquisition and reshaping

setwd("~/git/Automated Reporting/")
data_df <- read.csv("dataset.csv")

reshaped_df <- 
  data_df %>%
  gather(timepoint,  # reshape data for analysis
         outcome_variable, 
         c(timepoint_1, timepoint_2)) %>%
  mutate(participant = factor(participant),  #convert to factor for ANOVA
         timepoint = factor(timepoint))  #convert to factor for ANOVA

########################################################################
# analysis and descriptives via ez and schoRsch

# 2X2 mixed within-between anova
my_anova <- ez::ezANOVA(data = reshaped_df,
                        dv = outcome_variable,
                        within = timepoint,
                        between = condition,
                        wid = participant,
                        type = 3,
                        detailed = TRUE)

# summarise output
anova_summary <- schoRsch::anova_out(my_anova, 
                                     print = TRUE, 
                                     sph.cor = "GG", 
                                     mau.p = 0.05,
                                     etasq = "generalized", 
                                     dfsep = ", ")

# calculate descriptives
desc_stats <- 
  ez::ezStats(data = reshaped_df,
              dv = .(outcome_variable), 
              wid = .(participant),
              between = .(condition),
              within = .(timepoint),
              type = 3) %>%
  mutate(Mean = round(Mean, 2),  # round for later reporting
         SD = round(SD, 2)) %>%
  select(-FLSD)  # drop the least sig diff tests, unnecessary

########################################################################
# Novel steps beyond ez and schoRsch from here onwards
########################################################################
# extract output

### anova
## output
anova_ouptut <- 
  as.data.frame(anova_summary$`--- FORMATTED RESULTS ------------------------------------`) %>%  # anova_out returns a list of dfs. the appropriate df must be extracted.
  mutate(Text = as.character(Text))  # Text is stored as a factor, which must be converted to characters to be extracted

condition_output    <- anova_ouptut %>% filter(Effect == "condition") %>% .$Text  # convert text df to individual variables
timepoint_output    <- anova_ouptut %>% filter(Effect == "timepoint") %>% .$Text
interaction_output  <- anova_ouptut %>% filter(Effect == "condition:timepoint") %>% .$Text

## p values (for NHST)
anova_p <- 
  as.data.frame(anova_summary$`--- ANOVA RESULTS     ------------------------------------`) %>%  # anova_out returns a list of dfs. the appropriate df must be extracted.
  select(Effect, p) %>%  # select p value column
  mutate(p = as.character(p))

condition_p     <- anova_p %>% filter(Effect == "condition") %>% .$p  # convert text df to individual variables
timepoint_p     <- anova_p %>% filter(Effect == "timepoint") %>% .$p
interaction_p   <- anova_p %>% filter(Effect == "condition:timepoint") %>% .$p

## descriptives
a_1_m   <- desc_stats %>% filter(condition == "a", timepoint == "timepoint_1") %>% .$Mean  # convert df to individual variables
a_2_m   <- desc_stats %>% filter(condition == "a", timepoint == "timepoint_2") %>% .$Mean
b_1_m   <- desc_stats %>% filter(condition == "b", timepoint == "timepoint_1") %>% .$Mean
b_2_m   <- desc_stats %>% filter(condition == "b", timepoint == "timepoint_2") %>% .$Mean
a_1_sd  <- desc_stats %>% filter(condition == "a", timepoint == "timepoint_1") %>% .$SD
a_2_sd  <- desc_stats %>% filter(condition == "a", timepoint == "timepoint_2") %>% .$SD
b_1_sd  <- desc_stats %>% filter(condition == "b", timepoint == "timepoint_1") %>% .$SD
b_2_sd  <- desc_stats %>% filter(condition == "b", timepoint == "timepoint_2") %>% .$SD

########################################################################
# convert output to natural langauge

condition_text    <- paste(ifelse(condition_p < 0.05, 
                                  "A significant main effect was found for condition,",
                                  "No main effect was found for condition,"),
                           condition_output)
timepoint_text    <- paste(ifelse(timepoint_p < 0.05, 
                                  ". A significant main effect was found for time point,",
                                  ". No main effect was found for time point,"),
                           timepoint_output)
interaction_text  <- paste(ifelse(interaction_p < 0.05, 
                                  ". A significant condition*time point interaction effect was found,",
                                  ". No condition*time point interaction effect was found,"),
                           interaction_output)

########################################################################
## combine and write to disk

preamble <- "A mixed within-between ANOVA was conducted with the outcome variable as DV, condition as the between subjects IV, and time point as the within subjects IV. "

## final summary
anova_text <- 
  paste(preamble, 
        condition_text, 
        timepoint_text, 
        interaction_text, 
        ".",  # final full stop needed at end of sentence
        sep = "") %>%
  str_replace_all("  ", " ")  # remove double spaces. not sure how they're generated, but easy to remove here.

## write to disk
sink("output ANOVA mixed within-between.txt")
cat(anova_text)  # cat() supresses the line number from being printed
cat("\n")
cat("\n")
desc_stats
sink()

