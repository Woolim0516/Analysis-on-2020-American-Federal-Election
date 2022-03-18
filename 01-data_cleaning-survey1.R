#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from Democracy Fund + UCLA Nationscape
# Author: Guemin Kim, Yena Joo, Woolim Kim
# Data: 27 October 2020
# Contact: guemin.kim@mail.utoronto.ca, yena.joo@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from Democracy Fund + UCLA Nationscape and save the folder that you're 
# interested in  
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
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
         age,
         health_subsidies,
         medicare_for_all,
         foreign_born)


#### What else???? ####
# Maybe make some age-groups?
# Maybe check the values?
# Is vote a binary? If not, what are you going to do?
# Considering the questions above, try to mutate the variables in the data
# You are more than welcome to modify the code based on your choices of variables

# remove unrealistic observations from the data
reduced_data <-
  reduced_data %>% 
  filter(age != "less than 1 year old") %>%
  filter(age != "90 (90+ in 1980 and 1990)")

reduced_data$age <- as.integer(reduced_data$age)

# mutate variables that could be used in the modeling
reduced_data <- 
  reduced_data %>%
  filter(age>=18) %>%
  mutate(age_group = case_when(age <= 29 ~ "18-29 year olds",
                               age %in% c(30:44) ~ "30-44 year olds",
                               age %in% c(45:64) ~ "45-64 year olds",
                               age >=65 ~ "65 years and older")) %>%
  mutate(vote_Trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0)) %>%
  mutate(vote_Biden = 
           ifelse(vote_2020=="Joe Biden", 1, 0)) %>%
  mutate(household_income = case_when(household_income == "Less than $14,999" ~ "Less than $14,999",
                                      household_income %in% c("$15,000 to $19,999", "$20,000 to $24,999") ~ "$15,000 to $24,999",
                                      household_income %in% c("$25,000 to $29,999", "$30,000 to $34,999") ~ "$25,000 to $34,999",
                                      household_income %in% c("$35,000 to $39,999", "$40,000 to $44,999 ") ~ "$35,000 to $44,999",
                                      household_income %in% c("$45,000 to $49,999", "$50,000 to $54,999") ~ "$45,000 to $54,999",
                                      household_income %in% c("$55,000 to $59,999", "$60,000 to $64,999",
                                                              "$65,000 to $69,999", "$70,000 to $74,999") ~ "$55,000 to $74,999",
                                      household_income %in% c("$75,000 to $79,999", "$80,000 to $84,999",
                                                              "$85,000 to $89,999", "$90,000 to $94,999",
                                                              "$95,000 to $99,999") ~ "$75,000 to $99,999",
                                      household_income %in% c("$100,000 to $124,999", "$125,000 to $149,999") ~ "$100,000 to $149,999",
                                      household_income %in% c("$150,000 to $174,999", "$175,000 to $199,999",
                                                              "$200,000 to $249,999", "$250,000 and above") ~ "$150,000 and over")) %>%
  mutate(race = case_when(race_ethnicity == "White" ~ "White",
                                    race_ethnicity == "Black, or African American" ~ "Black",
                                    race_ethnicity %in% c("American Indian or Alaska Native", "Pacific Islander (Native Hawaiian)") ~ "Native",
                                    race_ethnicity %in% c("Asian (Asian Indian)", "Asian (Chinese)",
                                                          "Asian (Filipino)", "Asian (Japanese)", "Asian (Korean)",
                                                          "Asian (Other)", "Asian (Vietnamese)") ~ "Asian",
                                    race_ethnicity %in% c("Pacific Islander (Guamanian)", "Pacific Islander (Other)",
                                                          "Pacific Islander (Samoan)", "Some other race") ~ "Other")) %>%
  mutate(education = case_when(education %in% c("3rd Grade or less", "Middle School - Grades 4 - 8", "Completed some high school") ~ "Didn't graduate from high school",
                               education  == "High school graduate" ~ "High school graduate",
                               education %in% c("Associate Degree", "Completed some college, but no degree",
                                                "Completed some graduate, but no degree", "Other post high school vocational training") ~ "Some college or associate degree",
                               education %in% c("College Degree (such as B.A., B.S.)", "Doctorate degree ", "Masters degree") ~ "Bachelor's degree or higher"))

# only select certain variables you need in the data
reduced_data <- 
  reduced_data %>% 
  select(gender, age_group, race, education, household_income, state, vote_Trump, vote_Biden, vote_intention)

# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "survey_data.csv")

