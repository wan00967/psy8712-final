# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(haven) 
library(tidyverse)

# Data Import and Cleaning
gss_data <- read_sav("../data/General Social Survey, 2022.sav")

gss_cleaned <- gss_data %>%
  # Select necessary variables
  select(WRKSTAT, AGE, EDUC, MARITAL, CHILDS, EQWLTH, HAPPY) %>%
  # Remove rows with NA values in essential columns except WRKSTAT
  filter(!is.na(AGE) & !is.na(EDUC) & !is.na(MARITAL) & !is.na(CHILDS) & !is.na(EQWLTH) & !is.na(HAPPY)) %>%
  # Correctly defining the factor levels for WRKSTAT
  mutate(WRKSTAT = factor(WRKSTAT, levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                          labels = c("Working full time", "Working part time", "With a job, but not at work",
                                     "Unemployed, laid off", "Retired", "In school", "Keeping house", "Other")),
         EDUC = factor(EDUC),
         MARITAL = factor(MARITAL),
         CHILDS = as.integer(CHILDS),  # Ensure CHILDS is an integer for analysis
         EQWLTH = factor(EQWLTH, ordered = TRUE),
         HAPPY = factor(HAPPY, ordered = TRUE))


gss_shiny <- gss_data %>%
  select(HAPPY, DEGREE, SEX, WRKSLF, BORN, RACE, AGE, INCOME) %>%
  
  # Convert labelled data to factors and handle missing values
  mutate(
    HAPPY = factor(HAPPY, levels = c(1, 2, 3), labels = c("Very Happy", "Pretty Happy", "Not Too Happy")),
    DEGREE = factor(DEGREE, levels = c(0, 1, 2, 3, 4), labels = c("Less than High School", "High School", "Associate/Junior College", "Bachelors", "Graduate")),
    SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
    WRKSLF = factor(WRKSLF, levels = c(1, 2), labels = c("Self-employed", "Someone else")),
    BORN = factor(BORN, levels = c(1, 2), labels = c("Yes", "No")),
    RACE = factor(RACE, levels = c(1, 2, 3), labels = c("White", "Black", "Other")),
    AGE = as.numeric(AGE),  # Convert AGE to a plain numeric variable
    INCOME = as.numeric(INCOME)  # Ensure INCOME is also a plain numeric
  ) %>%
  
  # Filter out missing values in key variables, if necessary
  filter(!is.na(HAPPY) & !is.na(DEGREE) & !is.na(SEX) & !is.na(WRKSLF) & !is.na(BORN) & !is.na(RACE) & !is.na(AGE) & !is.na(INCOME)) %>%
  
  # Optionally convert INCOME to a numeric variable if it's coded categorically
  mutate(
    INCOME = as.numeric(as.character(INCOME)) # Make sure INCOME is appropriately coded if necessary
  )

## save data for shiny app
saveRDS(gss_shiny, "../shiny/data.rds")
