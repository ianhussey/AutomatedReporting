# title: 2 way ancova with η2
# author: Ian Hussey (ian.hussey@ugent.be)
# license: GPLv3+

 
# dependencies ------------------------------------------------------------


library(lsr)  # for eta2
library(MBESS)  # for ci.pvaf(), 95% CI on eta2
library(dplyr)
library(effects)  # for effect(), for adjusted means
library(psych)  # for describeBy()
library(weights)  # for rd(), a round() alternative 


# data acquisition --------------------------------------------------------


setwd("~/git/Automated Reporting/")
data_df <- read.csv("dataset.csv")


# ancova with 2x DV, 2x IV, and one covariate -----------------------------


model1 <- lm(formula = timepoint_2 ~ condition + timepoint_1, 
             data = data_df)  # NB if anova() had been used below there would be ordering effects for the model: must specify as DV ~ covariate + IV
ancova <- etaSquared(model1, 
                     type = 3, 
                     anova = TRUE)  # output full anova results, not just eta2

# descriptive stats by cell
# n
n_per_condition <-
  data_df %>%
  select(condition) %>%
  describeBy(data_df$condition,
             fast=TRUE,  # subset of descriptive stats
             ranges = FALSE,
             trim=0)
n_condition_a <- round(n_per_condition[[1]][["n"]][[1]], 2)
n_condition_b <- round(n_per_condition[[2]][["n"]][[1]], 2)

# adjusted means & sds
adjusted_means <- effect("condition", model1)


# extract individual stats ------------------------------------------------


# ancova
# NB this returns the main effect only, not the covariate effect
# all the below assume that the model has been specified as "DV ~ covariate + IV" in order to return the appropriate rows
ancova_F        <-  round(ancova[2,"F"], 2)         # where 2 specifies the main effect row
ancova_df_1     <-  round(ancova[2,"df"], 2)        # where 2 specifies the main effect row
ancova_df_2     <-  round(ancova[3,"df"], 2)        # where 3 specifies the residuals row
ancova_p        <-  round(ancova[2,"p"], 5)         # where 2 specifies the main effect row
ancova_eta2     <-  round(ancova[2,"eta.sq"], 2)    # where 2 specifies the main effect row

# 90% CI on eta2 (nb 90% not 95%, see Wuensch, 2009; Steiger. 2004)
# from http://daniellakens.blogspot.be/2014/06/calculating-confidence-intervals-for.html
# generically: ci.pvaf(F.value=XX, df.1=XX, df.2=XX, N=XX, conf.level=.90)
# 1. find n
n_df <- data_df %>% dplyr::summarize(n_variable = n())
n_integer <- n_df$n_variable
# 2. 90% CIs
ancova_eta2_ci        <- ci.pvaf(F.value = ancova_F, 
                                 df.1 = ancova_df_1, 
                                 df.2 = ancova_df_2, 
                                 N = n_integer, 
                                 conf.level = .90)
ancova_eta2_ci_lower  <- round(ancova_eta2_ci$Lower.Limit.Proportion.of.Variance.Accounted.for, 2)
ancova_eta2_ci_upper  <- round(ancova_eta2_ci$Upper.Limit.Proportion.of.Variance.Accounted.for, 2)

# NHST
nhst <- ifelse(ancova_p < 0.05, 
               "A main effect for condition was found: after controlling for scores at timepoint 1, scores at timepoint 2 were significantly different between ",
               "No main effect for condition was found: after controlling for scores at timepoint 1, no significant differences were found between scores at timepoint 2 between ")

# round p values using APA rules
ancova_p <- ifelse(ancova_p < 0.001, "< .001",
                   ifelse(ancova_p < 0.01,
                          paste("= ", rd(ancova_p, 3), sep = ""),  # rd() rounds, converts to string, and removes the leading 0.
                          ancova_p_APA_format <- paste("= ", rd(ancova_p, 2), sep = "")))

# descriptive stats
adjusted_mean_condition_a   <- round(data.frame(adjusted_means)[["fit"]][[1]], 2)
adjusted_mean_condition_b   <- round(data.frame(adjusted_means)[["fit"]][[2]], 2)
adjusted_sd_condition_a     <- round(data.frame(adjusted_means)[["se"]][[1]] * sqrt(n_condition_a), 2)  # NB: sd = se * sqrt(n)
adjusted_sd_condition_b     <- round(data.frame(adjusted_means)[["se"]][[2]] * sqrt(n_condition_b), 2)


# convert output to natural langauge --------------------------------------


# ancova
preamble <- "An ANCOVA was conducted with scores at timepoint 2 as the DV, experimental condition as the IV, and scores at timepoint 1 entered as a covariate. "
ancova_output <- paste(", F(", ancova_df_1, ", ", ancova_df_2, ") = ", ancova_F, ", p ", ancova_p_APA_format, ", η2 = ", ancova_eta2, ", 90% CI [", ancova_eta2_ci_lower, ", ", ancova_eta2_ci_upper, "]. ", sep = "") 

# descriptive stats output
desc_a <- paste("condition A (n = ", n_condition_a, ", adjusted M = ", adjusted_mean_condition_a, ", SD = ", adjusted_sd_condition_a, ")", sep = "") 
desc_b <- paste("condition B (n = ", n_condition_b, ", adjusted M = ", adjusted_mean_condition_b, ", SD = ", adjusted_sd_condition_b, ")", sep = "") 


# combine and write to disk -----------------------------------------------


## final summary
ancova_text <- paste(preamble, 
                     nhst, 
                     desc_a, 
                     " and ", 
                     desc_b, 
                     ancova_output, 
                     sep = "")

## write to disk
sink("ANCOVA output.txt")
cat(ancova_text)  # cat() supresses the line number from being printed
sink()

