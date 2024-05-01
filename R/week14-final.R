# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(haven) 
library(tidyverse)
library(caret)

# Data Import and Cleaning
gss_data <- read_sav("../data/General Social Survey, 2022.sav")

gss_cleaned <- gss_data %>%
  select(WRKSTAT, AGE, EDUC, MARITAL, CHILDS, EQWLTH, HAPPY, INCOME) %>%
  filter(!is.na(AGE) & !is.na(EDUC) & !is.na(MARITAL) & !is.na(CHILDS) & !is.na(EQWLTH) & !is.na(HAPPY)& !is.na(INCOME)) %>%
  mutate(
    WRKSTAT = factor(WRKSTAT, levels = c(1, 2, 4),
                     labels = c("Working_full_time", "Working_part_time", "Unemployed_laid_off")),
    EDUC = factor(EDUC),
    MARITAL = factor(MARITAL),
    CHILDS = as.integer(CHILDS),
    EQWLTH = factor(EQWLTH, ordered = TRUE),
    HAPPY = factor(HAPPY, ordered = TRUE),
    INCOME = as.numeric(INCOME)
  ) %>%
  filter(WRKSTAT %in% c("Working_full_time", "Working_part_time", "Unemployed_laid_off"))



gss_shiny <- gss_data %>%
  select(HAPPY, DEGREE, SEX, WRKSLF, BORN, RACE, AGE, INCOME) %>%
  
  # Convert labeled data to factors and handle missing values
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
  
  # convert INCOME to a numeric variable if it's coded categorically
  mutate(
    INCOME = as.numeric(as.character(INCOME)) # Make sure INCOME is appropriately coded if necessary
  )

## save data for shiny app
saveRDS(gss_shiny, "../shiny/data.rds")

# Analysis
## RQ1: how well can individuals' income be predicted based on their age, education level, marital status, and number of children?
set.seed(123)
index <- createDataPartition(gss_cleaned$INCOME, p = 0.75, list = FALSE)
gss_cleaned_train <- gss_cleaned[index,]
gss_cleaned_test <- gss_cleaned[-index,]

training_folds <- createFolds(gss_cleaned_train$INCOME, k = 10)

reuseControl <- trainControl(
  method = "cv",
  number = 10,
  search = "grid",
  indexOut = training_folds,
  verboseIter = TRUE,
  savePredictions = "final"
)

mod_vec <- c("lm", "glmnet", "ranger", "xgbTree")  # Suitable for regression
mod_ls <- list()

for (i in seq_along(mod_vec)) {
  method <- mod_vec[i]
  pre_process <- if (method %in% c("lm", "glmnet")) {
    c("center", "scale", "nzv", "medianImpute")
  } else {
    "medianImpute"
  }
  
  mod <- train(INCOME ~ .,
               data = gss_cleaned_train,
               method = method,
               metric = "RMSE",
               trControl = reuseControl,
               preProcess = pre_process)
  mod_ls[[i]] <- mod
}

# Function to extract model results and compute performance metrics
results <- function(train_mod) {
  algo <- train_mod$method
  
  # Extracting the best R-squared value from the training results
  cv_rsq <- max(train_mod$results$Rsquared)
  
  # Predicting on the test set
  preds <- predict(train_mod, gss_cleaned_test)
  
  # Computing the R-squared value on the holdout test set
  ho_rsq <- cor(preds, gss_cleaned_test$INCOME)^2
  
  # Formatting the results
  cv_rsq_formatted <- format(round(cv_rsq, 2), nsmall = 2)
  ho_rsq_formatted <- format(round(ho_rsq, 2), nsmall = 2)
  
  return(c("algo" = algo, "cv_rsq" = cv_rsq_formatted, "ho_rsq" = ho_rsq_formatted))
}

# Applying the results function to each model and creating a summary table
as_tibble(t(sapply(mod_ls, results)))

## RQ2: is there a significant difference in how different educational levels perceive income inequality?
### Ensuring `EQWLTH` is treated as a numeric for ANOVA
gss_cleaned$EQWLTH_numeric <- as.numeric(gss_cleaned$EQWLTH)

### ANOVA to compare EQWLTH across different levels of EDUC
anova_result <- aov(EQWLTH_numeric ~ EDUC, data = gss_cleaned)

### Output the ANOVA summary
summary(anova_result)

## RQ3: how does age correlate with income, and how does this correlation differ between males and females?
correlations <- gss_shiny %>%
  group_by(SEX) %>%
  summarize(correlation = cor(AGE, INCOME, use = "complete.obs"))

print(correlations)


