# Automated Reporting 
###### Run independent t tests, 2X2 mixed within between ANOVAs or ANCOVAs and produce manuscript-ready output and interpretations.

## License
Copyright (c) Ian Hussey 2016 (ian.hussey@ugent.be)

Released under the GPLv3+ open source license. 

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

## Version
0.1.3

## Description & purpose
When run on an appropriately formatted dataset, these scripts 

1. Conduct an given statistical test
2. Extract the and interpret the results results into a manuscript ready text that follows APA style. This includes test statistics, df, p values, effect sizes, descriptive statistics, and the interpretation of the null hypothesis significance test.
3. Save this text to a `.txt` file that can be pasted straight into a manuscript. 

This has three primary purposes and advantages:

1. It removes the possibility that a test will be correctly run but incorrectly reported. This happens far more frequently than we would like to admit: recent analysis of 250,000 published psychology papers suggests that roughly 50% of these included at least one p value that is inconsistent with its reported test statistic and degrees of freedom (Nuijten, Hartgerink, van Assen, Epskamp, & Wicherts, 2015). Of course, checking for this consistency before submission is also possible using the statscheck library employed in Nuijten et al (2015; see my `Statistical Reporting Assessment` repository for an implementation). However, the congruence between the test statistic, df, and p value is only one aspect of correctly reporting such analyses.
2. Pre registration of analytic strategies can be made more precise. Indeed, entire results sections can be specified before the data is collected, and "written" with by running the script after the data has been collected and processed. 
3. When used alongside Rmarkdown/knittr, these scripts allow you to write an entire methods section or manuscript in Rstudio. This way, the manuscript and the logic and implementation of the analytic strategy are fundamentally entwined, and therefore are made to be reproducable. 

## Included tests

- Independent t test (Welch).
- 2X2 mixed within-between ANOVA (for between groups pre-post designs).
- ANCOVA with single covariate (time point 2 as DV, condition as IV, time point 1 as covariate: for between groups pre-post designs).
- Bayesian BEST test (Kruschke, 2013), a Bayesian t test alternative.

## Input
The included dataset comes from a study on evaluative learning. The scripts employ the following variables:

```
- participant
- condition
- timepoint_1
- timepoint_2
- change_score  # i.e., timepoint_2 - timepoint_1
```

## Output
Each script writes standard output to the current working directory. This includes descriptive statistics as well as the test's output. The significance is also interpreted (as is the effect size in the case of the t test). For example, the t test produces the following:

*"An independent t test demonstrated significant differences of medium effect size between condition A (n = 52, M = 0.11, SD = 0.33) and condition B (n = 48, M = -0.1, SD = 0.35), t(96.77) = 3.07, p = 0.003, d = 0.61, 95% CI [0.2, 1.03]."*

This output can therefore be pasted directly into a results section.

## Requirements
- [R - v3.3.1](https://www.r-project.org/) or later
	- The included data processing script is written in R. I reccomend you run it in [RStudio](https://www.rstudio.com/), a very user friendly interface for R.
	
Several R packages are used by the scripts. See the dependencies listed in each for these. 

## Usage
1. For any of the scripts, you must customise the working directory line, containing setwd().
2. Ensure that the `dataset.csv` is correctly named, in the correct location, and contains the necessary columns.
2. Run script.

## Known issues
1. None.

## To do list
1. Full knittr integration: put the scripts inside .Rmd scripts.
2. schorRsh use could do with imporement - ANOVA only right now, some issues with ANCOVA and t test.

## Changelog
0.1.3

- BEST test (Krushke, 2013), a Bayesian t test alternative.

0.1.2

- better use of schoRsch. 

0.1.1 

- p values are rounded and reported using APA formatting. 


