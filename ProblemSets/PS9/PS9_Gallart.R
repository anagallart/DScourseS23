library(tidyverse)
library(tidymodels)
library(glmnet)
library(magrittr)
library(rsample)
library(dplyr)
library(parsnip)
library(yardstick)

## --------------------------------------
## 4. load in data
## --------------------------------------

# go to this url and grab the housing data
housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
# manually add the names to the columns 
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")
head(housing)


## 5.--------------------------------------
set.seed(123456)

## --------------------------------------
## 6. create two datasets 
## --------------------------------------
housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)



## --------------------------------------
## 7. recipe() takes log housing value to a factor
## --------------------------------------

housing_recipe <- recipe (medv ~ ., data = housing ) %>%
  # convert outcome variable to logs
  step_log(all_outcomes()) %>%
  # convert 0/1 chas to a factor
  step_bin2factor (chas) %>%
  # create interaction term between crime and nox
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:
                     ptratio :b: lstat:dis:nox) %>%
  # create square terms of some continuous variables
  step_poly (crim ,zn ,indus ,rm ,age ,rad ,tax ,ptratio ,b,
             lstat ,dis ,nox , degree =6)

# Run the recipe
housing_prep <- housing_recipe %>% prep( housing_train , retain = TRUE)
housing_train_prepped <- housing_prep %>% juice
housing_test_prepped <- housing_prep %>% bake(new_data = housing_test)
# create x and y training and test data
housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x <- housing_test_prepped %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select(medv)
housing_test_y <- housing_test_prepped %>% select(medv)

#### whats dim of housing_train? how many more x variables than the original housing data?
dim(housing) # 506 14
dim(housing_train) # 404 14
dim(housing_train_x) # 404 74, 60more x variables 
dim(housing_train_y) # 404 1



## --------------------------------------
## 8. estimate LASSO model to predict log median house value
## --------------------------------------

tune_spec <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# define a grid over which to try different values of lambda
lambda_grid <- grid_regular(penalty(), levels = 50)
# 6-fold cross-validation
rec_folds_train <- vfold_cv(housing_train_prepped, v = 6) #in sample
rec_folds_test <- vfold_cv(housing_test_prepped, v = 6) #out of sample

# Workflow
rec_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec) 

# Tuning results
rec_res_train <- rec_wf %>%
  tune_grid(
    resamples = rec_folds_train,
    grid = lambda_grid
  )  #penalty = 0.1526418

rec_res_test <- rec_wf %>%
  tune_grid(
    resamples = rec_folds_test,
    grid = lambda_grid
  )


top_rmse_train  <- show_best(rec_res_train, metric = "rmse")
best_rmse_train <- select_best(rec_res_train, metric = "rmse")
top_rmse_test  <- show_best(rec_res_test, metric = "rmse")
best_rmse_test <- select_best(rec_res_test, metric = "rmse")

# Now train with tuned lambda
final_lasso_train <- finalize_workflow(rec_wf, best_rmse_train)
final_lasso_test <- finalize_workflow(rec_wf, best_rmse_test)

# Print out results in train set
last_fit(final_lasso_train, split = housing_split) %>%
  collect_metrics() %>% print
### rmse standard estimator: 0.170
### rsq standard estimator: 0.809
top_rmse_train %>% print(n = 1)
## penalty: 0.00139, rmse standard estimator mean 0.0632 with n 6 and std_err 0.00503



last_fit(final_lasso_test, split = housing_split) %>%
  collect_metrics() %>% print
### rmse standard estimator: 0.180
### rsq standard estimator: 0.786
top_rmse_test %>% print(n = 1)
## penalty: 0.00910, rmse standard estimator mean 0.0658 with n 6 and std_err 0.00580 



## --------------------------------------
## 9. estimate Ridge regression model to predict log median house value
## --------------------------------------

tune_spec_ridge <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 0       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# define a grid over which to try different values of lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# 6-fold cross-validation
rec_folds_train <- vfold_cv(housing_train_prepped, v = 6) #in sample
rec_folds_test <- vfold_cv(housing_test_prepped, v = 6) #out of sample

# Workflow
rec_wf_ridge <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec_ridge)

# Tuning results
rec_res_ridge_train <- rec_wf_ridge %>%
  tune_grid(
    resamples = rec_folds_train,
    grid = lambda_grid
  )

rec_res_ridge_test <- rec_wf_ridge %>%
  tune_grid(
    resamples = rec_folds_test,
    grid = lambda_grid
  )


top_rmse_ridge_train  <- show_best(rec_res_ridge_train, metric = "rmse")
best_rmse_ridge_train <- select_best(rec_res_ridge_train, metric = "rmse")
top_rmse_ridge_test  <- show_best(rec_res_ridge_test, metric = "rmse")
best_rmse_ridge_test <- select_best(rec_res_ridge_test, metric = "rmse")

# Now train with tuned lambda
final_ridge_train <- finalize_workflow(rec_wf_ridge, best_rmse_ridge_train) #in sample
final_ridge_test <- finalize_workflow(rec_wf_ridge, best_rmse_ridge_test) # out of sample


# Print out results in test set
last_fit(final_ridge_train, split = housing_split) %>%
  collect_metrics() %>% print
### rmse standard estimator: 0.173
### rsq standard estimator: 0.803
top_rmse_ridge_train %>% print(n = 1)
## penalty: 0.0000000001, rmse standard estimator mean 0.0713 with n 6 and std_err 0.00602
#           0.0233                                     0.0691                     0.00339

last_fit(final_ridge_test, split = housing_split) %>%
  collect_metrics() %>% print
top_rmse_ridge_test %>% print(n = 1)
## penalty: 0.0000000001, rmse standard estimator mean 0.0571 with n 6 and std_err 0.00653


