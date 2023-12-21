# Kaggle: House Prices - Advanced Regression Techniques 
# 
# Source: https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques/data




# Setup -------------------------------------------------------------------


library(tidyverse)
library(tidymodels)




# Data Preparation --------------------------------------------------------


# import data

house_train <-
  read_csv("./data/house-prices/train.csv")

glimpse(house_train)

house_test <-
  read_csv("./data/house-prices/test.csv")

glimpse(house_test)



# remove columns with many missing values

remove_cols <- c(
  names(house_train)[map_dbl(house_train, ~ mean(!is.na(.x))) < .5],
  names(house_test)[map_dbl(house_test, ~ mean(!is.na(.x))) < .5]
) |> 
  unique()


house_test <- 
  house_test |> 
  select(-all_of(remove_cols)) |> 
  mutate_if(is.character, factor) |> 
  mutate(MSSubClass = factor(MSSubClass),
         MoSold = factor(MoSold, labels = month.abb))

house_train <- 
  house_train |> 
  select(-all_of(remove_cols)) |> 
  mutate_if(is.character, factor) |> 
  mutate(MSSubClass = factor(MSSubClass),
         MoSold = factor(MoSold, labels = month.abb))


# define resamples

house_folds <- vfold_cv(house_train, v = 5, strata = SalePrice)
  


# Define pre-processing steps ---------------------------------------------

library(rules)

base_rec <-
  recipe(SalePrice ~ ., data = house_train) |> 
  step_impute_knn(all_nominal_predictors()) |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_center(all_numeric_predictors()) |> 
  step_scale(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_zv(all_predictors())

pca_rec <- 
  transform_rec |> 
  step_pca(all_predictors(), num_comp = tune())


# compile

preproc <-
  list("base" = base_rec,
       "pca" = pca_rec)

models <-
  list(
    "linear_reg" = linear_reg(engine = "glmnet", penalty = tune()),
    "cubist" = cubist_rules(committees = tune()),
    "mlp" = mlp(hidden_units = tune()),
    "boosted_tree" = boost_tree(mtry = tune())
  ) |> 
  map(~ set_mode(.x, "regression"))




# Tuning parameters -------------------------------------------------------

# define a workflow 

lm_wflow <- workflow(preprocessor = preproc[[2]], spec = models[[1]])

lm_wflow |> extract_parameter_set_dials()


# setting grid values

lm_params <- 
  lm_wflow |> 
  extract_parameter_set_dials() |> 
  update(num_comp = num_comp(c(0, 40)),
         penalty = penalty(c(-1e3, 0)))

lm_grid <-
  lm_params |> 
  grid_regular(levels = 3)


# tune hyperparameters

lm_tuned <-
  lm_wflow |> 
  tune_grid(resamples = house_folds,
            grid = lm_grid)


# find the best hyperparameters

lm_best_params <-
  select_best(lm_tuned, metric = "rmse")


# save the final workflow

lm_final_wflow <-
  lm_wflow |> 
  finalize_workflow(lm_best_params)

lm_final_fit <- fit(lm_final_wflow, data = house_train)



# Tuning multiple workflows -----------------------------------------------

# setup multiple workflows

all_workflows <- workflow_set(preproc = preproc, models = models)
all_workflows


# tune parameters for all workflows

grid_results <- 
  all_workflows |> 
  workflow_map(
    seed = 1503,
    resamples = house_folds,
    grid = 5
  )


# find the best model with the best hyperparameters

grid_results |> 
  filter(wflow_id != "pca_boosted_tree") |> # error during training
  rank_results(rank_metric = "rmse") |> 
  filter(.metric == "rmse")

grid_results |> 
  extract_workflow_set_result("base_boosted_tree") |> 
  select_best(metric = "rmse")


# get final workflow

best_params <-
  grid_results |> 
  extract_workflow_set_result("base_boosted_tree") |> 
  select_best(metric = "rmse")
  
house_final_wflow <-
  grid_results |> 
  extract_workflow("base_boosted_tree") |> 
  finalize_workflow(best_params)


# make final predictions 

house_final_fit <-
  house_final_wflow |> 
  fit(data = house_train)

house_preds <- predict(house_final_fit, house_test)

# make submissions

house_submissions <- 
  bind_cols(select(house_test, Id), house_preds) |> 
  rename(SalePrice = .pred)

write_csv(house_submissions, "./data/house-prices/my_submissions.csv")
