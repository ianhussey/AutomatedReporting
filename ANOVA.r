# title: ANCOVA for 2x2 within-between data
# author: Ian Hussey (ian.hussey@ugent.be)
# license: GPLv3+
# usage: change variable names
# notes: DV not working


# dependencies ------------------------------------------------------------


library(tidyverse)
library(afex)   # for ez


# variable names ----------------------------------------------------------


IV_description          <- "experimental condition"
IV_name                 <- "condition"
DV_description          <- "timepoint"
DV_name                 <- c("timepoint_1", "timepoint_2")
identifier_name         <- "participant"


# data acquisition --------------------------------------------------------


setwd("~/git/Automated Reporting/")

data_df <- 
  read.csv("dataset.csv") %>%
  mutate(participant = as.factor(participant))  # set factors
  
sapply(data_df, class)  # check that appropriate variables are factors


# ancova with 2x DV, 2x IV, and one covariate -----------------------------


model_1 <- 
  aov_ez(data = data_df,
         iv = IV_name, 
         dv = DV_name, 
         between = IV_name,
         within = DV_name,
         id = identifier_name,
         check.contrasts = TRUE,  # effects coding. TRUE by default, but explicated here
         return = "nice",  # return a "nice" formatted data frame rather than S3 object 
         anova_table = list(es = "pes",  # effect size: partial eta squared
                            sig.symbols = c("", "", "", "")))  # removes the sig symbols


# extract and combine stats and strings -----------------------------------


test_description <- 
  paste("An ANOVA was conducted with ", 
        DV_description,
        " as the DV, and ",
        IV_description,
        " as the IV. ",
        sep = "")

# returns the main effect only, not the covariate effect
test_F     <- model_1[2, "F"]
test_df    <- model_1[2, "df"]
test_p     <- model_1[2, "p.value"]
test_es    <- model_1[2, "pes"]      
test_text  <- paste("F(", test_df, ") = ", test_F, ", p = ", test_p, ", η2p = ", test_es, sep = "")

# NHST
nhst_text <- 
  ifelse(as.numeric(test_p) < 0.05, 
         paste("A main effect was found for ", 
               IV_description,
               ": ",
               DV_description, 
               " were found to be significantly different, ", 
               sep = ""),
         paste("No main effect was found for ", 
               IV_description,
               ": ",
               DV_description, 
               " were found, ", 
               sep = ""))


# combine -----------------------------------------------------------------


test_output <- 
  paste(test_description, 
        nhst_text,
        test_text, 
        sep = "")


# write to disk -----------------------------------------------------------


sink("ANOVA output.txt")
model_1
cat("\n\n")
cat(test_output)  # cat() supresses the line number from being printed
sink()

