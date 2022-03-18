#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from IPUMS USA
# Author: Guemin Kim, Woolim Kim and Yena Joo
# Data: 27 October 2020
# Contact: guemin.kim@mail.utoronto.ca, yena.joo@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
# (You might need to change this if you use a different dataset)
raw_data <- read_dta("usa_00004.dta.gz")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(sex, 
         age,
         race, 
         hinscaid ,
         hinscare,
         educ,
         ftotinc,
         bpl,
         statefip)


#### What's next? ####
# we are going to mutate variables so that they match with those in the Survey data

# convert age and household income variables to integers
reduced_data$age <- as.integer(reduced_data$age)
reduced_data$ftotinc <- as.integer(reduced_data$ftotinc)

# list of states in America
state <- c("alabama", "alaska", "arizona", "arkansas", "california", "colorado",
           "connecticut", "delaware", "florida", "georgia", "hawaii", "idaho",
           "illinois", "indiana", "iowa", "kansas", "kentucky", "louisiana",
           "maine", "maryland", "massachusetts", "michigan", "minnesota",
           "mississippi", "missouri", "montana", "nebraska", "nevada", "new hampshire",
           "new jersey", "new mexico", "new york", "north carolina", "north dakota",
           "ohio", "oklahoma", "oregon", "pennsylvania", "rhode island", "south carolina",
           "south dakota", "tennessee", "texas", "utah", "vermont", "virginia",
           "washington", "west virginia", "wisconsin", "wyoming", "american samoa",
           "district of columbia")

# mutate demographic variables
reduced_data <- 
  reduced_data %>%
  filter(age>=18) %>%
  mutate(age_group = case_when(age <= 29 ~ "18-29 year olds",
                               age %in% c(30:44) ~ "30-44 year olds",
                               age %in% c(45:64) ~ "45-64 year olds",
                               age >=65 ~ "65 years and older")) %>%
  mutate(race = case_when(race == "white" ~ "White",
                          race == "black/african american/negro" ~ "Black",
                          race == "american indian or alaska native" ~ "Native",
                          race %in% c("chinese", "japanese", "other asian or pacific islander") ~ "Asian",
                          race %in% c("other race, nec", "two major races", "three or more major races") ~"Other")) 
reduced_data <- 
  reduced_data %>%
  mutate(education = case_when(educ %in% c("n/a or no schooling", "nursery school to grade 4",
                                           "grade 5, 6, 7, or 8", "grade 9", "grade 10", "grade 11") ~ "Didn't graduate from high school",
                               educ == "grade 12" ~ "High school graduate",
                               educ %in% c("1 year of college", "2 years of college") ~ "Some college or associate degree",
                               educ %in% c("4 years of college", "5+ years of college") ~ "Bachelor's degree or higher"))%>%
  mutate(household_income = case_when(ftotinc %in% c(0:14999) ~ "Less than $14,999",
                                      ftotinc %in% c(15000:24999) ~ "$15,000 to $24,999",
                                      ftotinc %in% c(25000:34999) ~ "$25,000 to $34,999",
                                      ftotinc %in% c(35000:44999) ~ "$35,000 to $44,999",
                                      ftotinc %in% c(45000:54999) ~ "$45,000 to $54,999",
                                      ftotinc %in% c(55000:74999) ~ "$55,000 to $74,999",
                                      ftotinc %in% c(75000:99999) ~ "$75,000 to $99,999",
                                      ftotinc %in% c(100000:149999) ~ "$100,000 to $149,999",
                                      ftotinc >= 150000 ~ "$150,000 and over")) %>%
  mutate(gender = case_when(sex == "male" ~ "Male",
                            sex == "female" ~ "Female")) 

# mutate state names with their abbreviated names
reduced_data <- 
  reduced_data %>%
  mutate(state = case_when(statefip == "alaska" ~ "AK",
                           statefip == "alabama" ~ "AL",
                           statefip == "arizona" ~ "AZ",
                           statefip == "arkansas" ~ "AR",
                           statefip == "california" ~ "CA",
                           statefip == "colorado" ~"CO",
                           statefip == "connecticut" ~ "CT",
                           statefip == "district of columbia" ~ "DC",
                           statefip == "delaware" ~ "DE",
                           statefip == "florida" ~ "FL",
                           statefip == "georgia" ~ "GA",
                           statefip == "hawaii" ~ "HI",
                           statefip == "iowa" ~ "IA",
                           statefip == "idaho" ~ "ID",
                           statefip == "illinois" ~ "IL",
                           statefip == "indiana" ~ "IN",
                           statefip == "kansas" ~ "KS",
                           statefip == "kentucky" ~ "KY",
                           statefip == "louisiana" ~ "LA",
                           statefip == "massachusetts" ~ "MA",
                           statefip == "maryland" ~ "MD",
                           statefip == "maine" ~ "ME",
                           statefip == "michigan" ~ "MI",
                           statefip == "minnesota" ~ "MN",
                           statefip == "missouri" ~ "MO",
                           statefip == "mississippi" ~ "MS",
                           statefip == "montana" ~ "MT",
                           statefip == "north carolina" ~ "NC",
                           statefip == "north dakota" ~ "ND",
                           statefip == "nebraska" ~ "NE",
                           statefip == "new hampshire" ~ "NH",
                           statefip == "new jersey" ~ "NJ",
                           statefip == "new mexico" ~ "NM",
                           statefip == "nevada" ~ "NV",
                           statefip == "new york" ~ "NY",
                           statefip == "ohio" ~ "OH",
                           statefip == "oklahoma" ~ "OK",
                           statefip == "oregon" ~ "OR",
                           statefip == "pennsylvania" ~ "PA",
                           statefip == "rhode island" ~ "RI",
                           statefip == "south carolina" ~ "SC",
                           statefip == "south dakota" ~ "SD",
                           statefip == "tennessee" ~ "TN",
                           statefip == "texas" ~ "TX",
                           statefip == "utah" ~ "UT",
                           statefip == "virginia" ~ "VA",
                           statefip == "vermont" ~ "VT",
                           statefip == "washington" ~ "WA",
                           statefip == "wisconsin" ~ "WI",
                           statefip == "west virginia" ~ "WV",
                           statefip == "wyoming" ~ "WY")) %>%
  mutate(foreign_born = ifelse(bpl %in% state, "The United States", "Another country")) 


## You can use other variables to split by changing
## count(age_group, gender, race, state, education, household_income) to count(age, marriage_status, ....)
data <- 
  reduced_data %>%
  select(age_group, gender, race, state, education, household_income) %>%
  count(age_group, gender, race, state, education, household_income) %>%
  group_by(age_group, gender, race, state, education, household_income) %>%
  rename(count = n)

# calculate proportions in each cell
data <- 
  data %>% 
  mutate(cell_prop = count/sum(data$count))

# Saving the census data as a csv file in my
# working directory
write_csv(data, "census_data.csv")