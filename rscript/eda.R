library(tidyverse)
library(readr)
library(janitor)
library(scales)
library(here)
library(visdat)
library(ggmap)
library(caret)
library(caTools)

base_df <- read_csv(here::here('sensitive', 'base_data.csv')) %>% 
  clean_names()

vis_dat(base_df)
vis_miss(base_df)


filtered_data <- base_df %>% 
  filter(! is.na(race_desc), 
         ! is.na(first_gen), 
         ! is.na(act_score),
         ! is.na(act_math),
         ! is.na(hs_gpa),
         ! is.na(sorhsch_class_rank),
         ! is.na(sorhsch_class_size),
         ! is.na(sorhsch_percentile)
  )

model_base <-  filtered_data %>% 
  select(student_pidm, term_code, index_score, act_score, act_math, hs_gpa, sorhsch_percentile, sorhsch_class_size, fall_up_two) %>% 
  mutate(outcome = if_else(fall_up_two %in% c('Retained', 'Graduated'), 'S', 'L')) %>%
  mutate(outcome = as.factor(outcome)) %>% 
  mutate(year = str_sub(term_code, 1, 4)) %>% 
  select(-fall_up_two, -term_code) %>% 
  filter(between(year, 2012, 2018) )


act_tib <- model_base %>% 
  select(act_score, outcome)

act_train <- model_base %>% 
  filter(year != '2018') 

act_test <- model_base %>% 
  filter(year == '2018')

fitControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)


act_mod <- train(
  outcome ~ act_score,
  data = act_train,
  method = "glm",
  family = "binomial",
  trControl = fitControl
)

summary(act_mod)

p <- predict(act_mod, act_test, type = "prob")

act_predictions <- modelr::add_predictions(act_test, act_mod, var = 'pred', type = 'prob')

# ROC Curve
colAUC(act_predictions$pred[2], act_predictions$outcome, plotROC = TRUE)

# AUC
act_mod

# Now we do a similar plan for index score, then for gpa.

index_tib <- model_base %>% 
  select(index_score, outcome)

index_train <- model_base %>% 
  filter(year != '2018') 

index_test <- model_base %>% 
  filter(year == '2018')
  
index_mod <- train(
  outcome ~ index_score,
  data = index_train,
  method = "glm",
  family = "binomial",
  trControl = fitControl
)

summary(index_mod)

index_predictions <- modelr::add_predictions(index_test, index_mod, var = 'pred', type = 'prob')

# ROC Curve
colAUC(index_predictions$pred[2], index_predictions$outcome, plotROC = TRUE)

# AUC
index_mod

# Coefficients
index_mod$finalModel

# Summary Statistics
coef(summary(index_mod$finalModel))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      ACT Math
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

math_train <- model_base %>% 
  filter(year != '2018') 

math_test <- model_base %>% 
  filter(year == '2018')

math_mod <- train(
  outcome ~ act_math,
  data = math_train,
  method = "glm",
  family = "binomial",
  trControl = fitControl
)

summary(math_mod)

math_predictions <- modelr::add_predictions(math_test, math_mod, var = 'pred', type = 'prob')

# ROC Curve
colAUC(math_predictions$pred[2], math_predictions$outcome, plotROC = TRUE)

# AUC
math_mod


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      HS Rank
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rank_train <- model_base %>% 
  filter(year != '2018') 

rank_test <- model_base %>% 
  filter(year == '2018')

rank_mod <- train(
  outcome ~ sorhsch_percentile,
  data = rank_train,
  method = "glm",
  family = "binomial",
  trControl = fitControl
)

summary(rank_mod)

rank_predictions <- modelr::add_predictions(rank_test, rank_mod, var = 'pred', type = 'prob')

# ROC Curve
colAUC(rank_predictions$pred[2], rank_predictions$outcome, plotROC = TRUE)

# AUC
rank_mod

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      HS Rank + GPA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rank_gpa_train <- model_base %>% 
  filter(year != '2018') %>% 
  as.data.frame()

rank_gpa_test <- model_base %>% 
  filter(year == '2018')

preProcValues  <- preProcess(rank_gpa_train, method = 'BoxCox')

rank_gpa_mod <- train(
  outcome ~ sorhsch_percentile + hs_gpa,
  data = rank_gpa_train,
  method = "glm",
  family = "binomial",
  preProcess = c('BoxCox', 'center', 'scale'),
  trControl = fitControl
)

summary(rank_gpa_mod)

rank_gpa_predictions <- modelr::add_predictions(rank_gpa_test, rank_gpa_mod, var = 'pred', type = 'prob')

# ROC Curve
colAUC(rank_gpa_predictions$pred[2], rank_gpa_predictions$outcome, plotROC = TRUE)

# AUC
rank_gpa_mod

# Coefficients
rank_gpa_mod$finalModel

# Summary Statistics
coef(summary(rank_gpa_mod$finalModel))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                     GPA alone
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

gpa_train <- model_base %>% 
  filter(year != '2018') 

gpa_test <- model_base %>% 
  filter(year == '2018')

gpa_mod <- train(
  outcome ~ hs_gpa,
  data = gpa_train,
  method = "glm",
  family = "binomial",
  preProcess = c('BoxCox'),
  trControl = fitControl
)

summary(gpa_mod)

gpa_predictions <- modelr::add_predictions(gpa_test, gpa_mod, var = 'pred', type = 'prob')

# ROC Curve
colAUC(gpa_predictions$pred[2], gpa_predictions$outcome, plotROC = TRUE)

# AUC
gpa_mod
