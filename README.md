# Automated Reporting 
###### Run t tests, ANOVAs, or ANCOVAs and produce manuscript-ready output and interpretations. For 2X2 mixed within-between designs.

## License
Copyright (c) Ian Hussey 2016 (ian.hussey@ugent.be)

Released under the GPLv3+ open source license. 

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

## Version
0.1

## Description & purpose
When run on an appropriately formatted dataset, these scripts conduct an independent t test, ANOVA or ANCOVA and output the results to a .txt file in a format that can be pasted straight into a manuscript. This includes test statistics, df, p values, effect sizes, descriptive statistics, and the interpretation of the null hypothesis significance test.

This has three primary purposes and advantages:

1. It removes the possibility that a test will be correctly run but incorrectly reported. This happens far more frequently than we would like to admit: recent analysis of 250,000 published psychology papers suggests that roughly 50% of these included at least one p value that is inconsistent with its reported test statistic and degrees of freedom (Nuijten, Hartgerink, van Assen, Epskamp, & Wicherts, 2015). Of course, checking for this consistency before submission is also possible using the statscheck library employed in Nuijten et al (2015; see my `Statistical Reporting Assessment` repository for an implementation). However, the congruence between the test statistic, df, and p value is only one aspect of correctly reporting such analyses.
2. Pre registration of analytic strategies can be made more precise. Indeed, entire results sections can be specified before the data is collected, and "written" with by running the script after the data has been collected and processed. 
3. When used alongside Rmarkdown/knittr, these scripts allow you to write an entire methods section or manuscript in Rstudio. This way, the manuscript and the logic and implementation of the analytic strategy are fundamentally entwined, and therefore are made to be reproducable. 

## Analytic strategy options

There are at least three ways of analysing 2X2 mixed within-between designs (i.e., pre/post, intervention vs. control) within a frequentist NHST framework. From what I gather, individuals and communities often employ a given strategy out of habit rather than careful forethought. 

### Option 1: 2X2 mixed within between ANOVA
- Assess interaction effect.
- Design does not assume identical scores at either timepoint.
	
### Option 2: Independent t test on pre-post change scores
- Assess difference between conditions.
- Design does not assume identical scores at either timepoint.
- Mathmathetically equivalent to 2X2 ANOVA, with more succinct output and easier interpretation.

### Option 3: 2-way ANCOVA with timepoint 2 as DV, condition as IV, and timepoint 1 as covariate
- Assess main effect for condition.
- Design assumes identical scores at timepoint 1. I.e., that both groups are adjusted to be equal before the intervention. 
- Higher power to detect differences than option 1 or 2.

## Input
The included dataset comes from a study on evaluative learning. The scripts employ the following variables:

```
- participant  # uniqe ID needed for the ANCOVA
- condition  # two levels, output refers to these as A and B
- timepoint_1  # for ANOVA and ANCOVA
- timepoint_2  # for ANOVA and ANCOVA
- change_score  # for t test, i.e., timepoint_2 - timepoint_1
```

## Output
Each script writes standard output to the current working directory. This includes descriptive statistics as well as the test's output. The significance is also interpreted (as is the effect size in the case of the t test). For example, the t test produces the following:

*"An independent t test demonstrated significant differences of medium effect size between condition A (n = 52, M = 0.11, SD = 0.33) and condition B (n = 48, M = -0.1, SD = 0.35), t(96.77) = 3.07, p = 0.0028, d = 0.61, 95% CI [0.2, 1.03]."*

This output can therefore be pasted directly into a results section.

Note, however, that extreme values of the p statistic are not interpreted (i.e., `< .0001` returns = `.0000`). Additionally, this value is not rounded to APA standards (i.e., `= .0085` is returned rather than `< .01`). Finally, the leading `0` of p values is returned, where technically it should not be included, according to APA guidelines (i.e., currently it returns `0.0110` rather than `.0110`). 

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
1. Interpret extreme values of the p statistic (i.e., `< .0001` returns = `.0000`) and rounded to APA standards (i.e., `= .0085` is returned rather than `< .01`). 
2. Remove the leading `0` of p values (i.e., set to `.0110` rather than `0.0110`). 
3. Full knittr integration: put the scripts inside .Rmd scripts.



