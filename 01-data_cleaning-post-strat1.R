#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from census data
# Author: Juntong Lin
# Data: 22 October 2020
# Contact: juntong.lin@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.

raw_data2 <- read_csv("usa_00001.csv.gz")


# Add the labels
raw_data2 <- labelled::to_factor(raw_data2)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data2 <- 
  raw_data2 %>% 
  select(STATEICP,
         HHINCOME,
         PERWT,
         SEX,
         RACE,
         AGE,
         EDUC,
         EMPSTAT)
         

#### What's next? ####
# Clean these variables to make them comparable to survey data
reduced_data2 <- 
  reduced_data2 %>%
  mutate(age = AGE,
         employment = ifelse(EMPSTAT==1, 1, 0),
         gender = ifelse(SEX == 1, 1, 0),
         race = ifelse(RACE == 1, 1, 0),
         household_income = cut(HHINCOME,
                                c(-Inf, seq(15000,100000, 5000), seq(125000, 200000, 25000), 250000, Inf),
                                include.lowest = TRUE, right = FALSE,
                                labels = c("Less than $14,999","$15,000 to $19,999","$20,000 to $24,999","$25,000 to $29,999",
                                           "$30,000 to $34,999","$35,000 to $39,999","$40,000 to $44,999","$45,000 to $49,999",
                                           "$50,000 to $54,999","$55,000 to $59,999","$60,000 to $64,999","$65,000 to $69,999",
                                           "$70,000 to $74,999","$75,000 to $79,999","$80,000 to $84,999","$85,000 to $89,999",
                                           "$90,000 to $94,999","$95,000 to $99,999","$100,000 to $124,999","$125,000 to $149,999",
                                           "$150,000 to $174,999","$175,000 to $199,999","$200,000 to $249,999","$250,000 and above")),
         education = cut(EDUC, c(-1,2,5,6,9,10,11),
                         labels = c("Middle School or less",
                                    "Completed some high school",
                                    "High school graduate",
                                    "Completed some college, but no degree",
                                    "College Degree (such as B.A., B.S.)",
                                    "More than College"))) %>%
  # join table to make state name two letters
  inner_join(pscl::state.info %>%
               as_tibble() %>%
               mutate(state = state.abb[match(state,state.name)]) %>%
               rename(STATEICP = icpsr)) %>%
  mutate(state = factor(state, levels = unique(.$state))) %>%
  subset(select = c(age, employment, gender, race, household_income, education, state, PERWT)) %>%
  # filter out don't know and NA
  na.omit()


## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

reduced_data3 <- 
  reduced_data2 %>%
  group_by(age, employment, gender, race, household_income, education, state) %>%
  summarise(n = sum(PERWT))

reduced_data3 <- 
  reduced_data3 %>% 
  # Only want >= 18, legal to vote
  filter(age >= 18)

# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data3, "census_data.csv")



         