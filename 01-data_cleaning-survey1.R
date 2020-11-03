#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("~/R/Help_temp/2-PS3")

# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(interest,
         registration,
         vote_2016,
         vote_intention,
         vote_2020,
         ideo5,
         employment,
         foreign_born,
         gender,
         census_region,
         hispanic,
         race_ethnicity,
         household_income,
         education,
         state,
         congress_district,
         age)


#### What else???? ####
# Maybe make some age-groups?
# Maybe check the values?
# Is vote a binary? If not, what are you going to do?
# We need to make the survey data and post-strat data the same:

# education needs to change to different levels
#     "Middle School or less"                     - "3rd Grade or less" or "Middle School - Grades 4 - 8"
#     "Completed some high school"                - "Completed some high school"
#     "High school graduate"                      - "High school graduate" or "Other post high school vocational training"
#     "Completed some college, but no degree"     - "Completed some graduate, but no degree" or "Associate Degree"
#     "College Degree (such as B.A., B.S.)"       - "College Degree (such as B.A., B.S.)"
#     "More than College"                         - "Masters degree", "Completed some graduate, but no degree", or "Doctorate degree"

reduced_data<-  
  reduced_data %>%
  mutate(vote_trump = ifelse(vote_2020=="Donald Trump", 1, 0)) %>%
  mutate(employment = ifelse(employment=="Full-time employed", 1, 0),
         race = ifelse(race_ethnicity=="White", 1, 0),
         gender = ifelse(gender=="Male", 1, 0),
         state = factor(state, levels = unique(.$state)),
         education = recode_factor(education,
                                   `3rd Grade or less` = "Middle School or less",
                                   `Middle School - Grades 4 - 8` = "Middle School or less",
                                   `Completed some high school` = "Completed some high school",
                                   `High school graduate` = "High school graduate",
                                   `Other post high school vocational training` = "High school graduate",
                                   `Completed some graduate, but no degree` = "Completed some graduate, but no degree",
                                   `Associate Degree` = "Completed some graduate, but no degree",
                                   `College Degree (such as B.A., B.S.)` = "College Degree (such as B.A., B.S.)",
                                   `Masters degree` = "More than College",
                                   `Completed some graduate, but no degree` = "More than College",
                                   `Doctorate degree` = "More than College")) %>%
  subset(select = c(age, employment, gender, race, household_income, education, state, vote_trump)) %>%
  # filter out don't know and NA
  na.omit()

# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "survey_data.csv")

