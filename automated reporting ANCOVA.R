########################################################################
# Automated 2 way ancova and η2 reporting, for use in knittr scripts
# Ian Hussey

# checked against results returned by JASP

# usage:
# 1. customise the working directory line, containing setwd() below
# 2. run script

# to do:
# rounding of F and p values needs to be more adaptive, e.g., including < values.
# ezANOVA() would be an alternative regression method with nicer output

########################################################################
# Clean workspace
rm(list=ls())

########################################################################
# dependencies 
library(lsr)  # for eta2
library(MBESS)  # for 95% CI on eta2
library(dplyr)
library(effects)  # for effect(), for adjusted means
library(psych)  # for describeBy()

########################################################################
# data acquisition
setwd("~/git/Automated Reporting/")
data_df <- read.csv("dataset.csv")

########################################################################
# ancova with timepoint_2 as DV, condition as IV, and timepoint_1 as covariate
model1 <- lm(formula = timepoint_2 ~ timepoint_1 + condition, 
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
data.frame(adjusted_means)  # includes means and se, but not sd. this is converted below.


########################################################################
## extract individual stats

# ancova
# NB this returns the main effect only, not the covariate effect
# all the below assume that the model has been specified as "DV ~ covariate + IV" in order to return the appropriate rows
ancova_F    <-  round(ancova[2,"F"], 2)         # where 2 specifies the main effect row
ancova_df_1 <-  round(ancova[2,"df"], 2)        # where 2 specifies the main effect row
ancova_df_2 <-  round(ancova[3,"df"], 2)        # where 3 specifies the residuals row
ancova_p    <-  round(ancova[2,"p"], 5)         # where 2 specifies the main effect row
ancova_eta2 <-  round(ancova[2,"eta.sq"], 2)    # where 2 specifies the main effect row

# 90% CI on eta2 (nb 90% not 95%, see Wuensch, 2009; Steiger. 2004)
# from http://daniellakens.blogspot.be/2014/06/calculating-confidence-intervals-for.html
# generically: ci.pvaf(F.value=XX, df.1=XX, df.2=XX, N=XX, conf.level=.90)
# 1. find n
n_df <- summarize(data_df, n_variable = n())
n_integer <- n_df$n_variable
# 2. 90% CIs
ancova_eta2_ci_lower <- round(ci.pvaf(F.value=ancova_F, df.1=ancova_df_1, df.2=ancova_df_2, N=n_integer, conf.level=.90)$Lower.Limit.Proportion.of.Variance.Accounted.for, 2)
ancova_eta2_ci_upper <- round(ci.pvaf(F.value=ancova_F, df.1=ancova_df_1, df.2=ancova_df_2, N=n_integer, conf.level=.90)$Upper.Limit.Proportion.of.Variance.Accounted.for, 2)

# NHST
if (ancova_p < 0.05) {
  significance    <- "A main effect for condition was found: after controlling for time point 1 scores, scores at time point 2 were significantly different between "
} else {
  significance    <- "No main effect for condition was found: after controlling for time point 1 scores, no significant differences were found between "
}

# descriptive stats
adjusted_mean_condition_a   <- round(data.frame(adjusted_means)[["fit"]][[1]], 2)
adjusted_mean_condition_b   <- round(data.frame(adjusted_means)[["fit"]][[2]], 2)
# NB: sd = se * sqrt(n)
adjusted_sd_condition_a     <- round(data.frame(adjusted_means)[["se"]][[1]] * sqrt(n_condition_a), 2)
adjusted_sd_condition_b     <- round(data.frame(adjusted_means)[["se"]][[2]] * sqrt(n_condition_b), 2)


########################################################################
## report stats

acnova_setup <- "An ANCOVA was conducted with time point 2 as the DV, condition as the IV, and time point 1 as a covariate. "

ancova_output <- paste(", F(", ancova_df_1, ", ", ancova_df_2, ") = ", ancova_F, ", p = ", ancova_p, ", η2 = ", ancova_eta2, ", 90% CI [", ancova_eta2_ci_lower, ", ", ancova_eta2_ci_upper, "]. ", sep = "") 
#ancova_output # returns "F(1, 97) = 6.18, p = 0.01461, η2 = 0.06, 90% CI [0.01, 0.15]"

# descriptive stats output
desc_condition_A  <- paste("condition A (n = ", n_condition_a, ", adjusted M = ", adjusted_mean_condition_a, ", SD = ", adjusted_sd_condition_a, ")", sep = "") 
desc_condition_B  <- paste("condition B (n = ", n_condition_b, ", adjusted M = ", adjusted_mean_condition_b, ", SD = ", adjusted_sd_condition_b, ")", sep = "") 
#desc_condition_A  # returns "condition A (n = 52, adjusted M = -0.01, SD = 0.25)"
#desc_condition_B  # returns "condition B (n = 48, adjusted M = -0.13, SD = 0.25)"

## final summary
ancova_output_and_interpretation <- paste(acnova_setup, significance, desc_condition_A, " and ", desc_condition_B, ancova_output, sep = "")
#ancova_output_and_interpretation

## write to disk
sink("output ANCOVA.txt")
cat(ancova_output_and_interpretation)  # cat() supresses the line number from being printed
sink()

