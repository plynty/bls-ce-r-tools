# BLS CE R tools
This repository contains tools for analyzing the U.S. Bureau of Labor Statistics, Consumer Expenditure Surveys, using R.
###CE_PUMD_Wrangler.R
This R script creates a multitude of functions that helps to load and subset the CE PUMD.
####How to use
+ Fill out the directory parameters
  + interview_dir is the directory which the interview files are located.
  + diary_dir is the directory which the diary files are located.
  + stub_dir is the directory which the stubfiles are located.
  + year is the year of the CE PUMD you are studying
+ Source the R script
+ Use whichever functions you wish to read in and subset CE PUMD

###plyntyCalculations.R
This R script calculates the plynty expenditure percentages based on subset parameters.
####How to use
+ Fill out the parameters
  + my_dir is the directory which the repository is located
  + year is the year of the CE PUMD you are studying
  + minAge is the low end of the age range of which you want to subset
  + maxAge is the high end of the age range of which you want to subset
  + incomeBrackets is a vector of income levels of which the income classes should represent
+ Source the R script
