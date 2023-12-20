# Stars dataset
# 
# Source: https://www.kaggle.com/datasets/adityakadiwal/water-potability/





# Setup -------------------------------------------------------------------

library(tidyverse)
library(tidymodels)





# Data Preparation --------------------------------------------------------

# Import data from the internet

water <-
  read_csv('./data/water_potability.csv') |> 
  janitor::clean_names("upper_camel") |> 
  rename("pH" = "Ph") |> 
  mutate(Potability = factor(Potability))

glimpse(water)

summary(water)

count(water, Potability) |> mutate(prop = n / sum(n))

purrr::map_dbl(water, function(x) {mean(!is.na(x))}) |> 
  scales::percent()







# Define several workflows ------------------------------------------------

# Set pre-processed data

water_rec <- 
  recipe(Potability ~ ., data = water) |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_discretize(Hardness, Sulfate, Conductivity) |>
  step_dummy(all_nominal_predictors()) |> 
  step_center(pH, Chloramines, OrganicCarbon, Trihalomethanes) |> 
  step_scale(pH, Chloramines, OrganicCarbon, Trihalomethanes) |> 
  step_range(Turbidity)

prep(water_rec) |> bake(new_data = NULL)


# Define several model

library(baguette)

define_models <- function() {
  bagtree_wflow <- workflow(water_rec, spec = bag_tree(mode = "classification"))
  btdtree_wflow <- workflow(water_rec, spec = boost_tree(mode = "classification"))
  rf_wflow <- workflow(water_rec, spec = rand_forest(mode = "classification"))
  
  list(bagtree_wflow, btdtree_wflow, rf_wflow)
}

model_names <- c("bagged tree", "boosted tree", "random forest")





# Resampling: Validation Set ----------------------------------------------

# Split data into train, test, and validation

set.seed(123)
water_split <- 
  initial_validation_split(water, prop = c(0.6, 0.2), strata = Potability)

water_train <- training(water_split)
water_validation <- validation(water_split)
water_test <- testing(water_split)



# Evaluate models

results <- list(model_names = model_names,
                train_accuracy = double(),
                val_accuracy = double(),
                test_accuracy = double())

for (water_wflow in define_models()) {
  water_fit <- fit(water_wflow, water_train)
  
  # evaluate train accuracy
  
  preds_train <- predict(water_fit, water_train)
  
  train_acc <- 
    bind_cols("truth" = water_train$Potability, preds_train) |> 
    accuracy(truth = truth, estimate = .pred_class) |> 
    pull(.estimate)
  
  
  # evaluate validation accuracy
  
  preds_val <- predict(water_fit, water_validation)
  
  val_acc <- 
    bind_cols("truth" = water_validation$Potability, preds_val) |> 
    accuracy(truth = truth, estimate = .pred_class) |> 
    pull(.estimate)
  
  
  # evaluate test accuracy (it's shouldn't actually be here)
  
  preds_test <- predict(water_fit, water_test)
  
  test_acc <- 
    bind_cols("truth" = water_test$Potability, preds_test) |> 
    accuracy(truth = truth, estimate = .pred_class) |> 
    pull(.estimate)
  
  
  # save the results
  
  results[["train_accuracy"]] <- c(results[["train_accuracy"]], train_acc)
  results[["test_accuracy"]] <- c(results[["test_accuracy"]], val_acc)
  results[["val_accuracy"]] <- c(results[["val_accuracy"]], test_acc)
}


# Compiling the results

results <- as.data.frame(results) |> tibble()

results |> 
  pivot_longer(cols = -model_names) |> 
  mutate(name = str_replace(name, "_accuracy", "") |> 
           str_to_upper() |> 
           factor(levels = c("TRAIN", "VAL", "TEST"))) |> 
  ggplot(aes(y = fct_rev(name), x = value, fill = model_names)) +
  geom_col(position = "dodge") +
  facet_wrap(model_names ~ ., nrow = 3) +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_brewer(type = "qual", palette = 2, guide = "none") +
  labs(title = "Using Validation Sets", x = NULL, y = NULL) +
  theme_light()






# Resampling: Cross-validation --------------------------------------------

# Split data into train and test

set.seed(123)
water_split <- initial_split(water, strata = Potability)

water_train <- training(water_split)
water_test <- testing(water_split)


# Cross-validation split

water_folds <- vfold_cv(water_train, v = 10, strata = Potability)
water_folds


# Playing with the first fold

water_folds[[1,1]][[1]]

sample_fold_train <- analysis(water_folds$splits[[1]])
sample_fold_test <- assessment(water_folds$splits[[1]])

sample_model <- define_models()[[2]]

fit(sample_model, data = sample_fold_train) |> 
  predict(sample_fold_test) |> 
  count(.pred_class)


# Evaluate the resampling set

water_wflow <- define_models()[[2]]

water_res_cv <- fit_resamples(water_wflow, resamples = water_folds)

water_res_cv$.metrics

collect_metrics(water_res_cv)


# Evaluate on test data

fit(water_wflow, data = water_train) |> 
  predict(water_test) |> 
  bind_cols("truth" = water_test$Potability) |> 
  # this should be similar to resampling evaluation
  accuracy(truth = truth, estimate = .pred_class) 



# Resampling: Bootstrapping -----------------------------------------------

# Create bootstraped data

water_folds <- bootstraps(water_train, times = 25, strata = Potability)

water_res_boot <- 
  purrr::map2_dfr(model_names,
                  define_models(),
                  function(model_name, water_wflow) {
    fit_resamples(water_wflow, resamples = water_folds) |> 
      collect_metrics() |> 
      mutate(model_name = model_name) |> 
      select(model_name, .metric, mean, std_err)
  })


rf_model <- define_models()[[3]]

rf_fit <- fit(rf_model, data = water_train)

rf_preds <- predict(rf_fit, water_test)

bind_cols("truth" = water_test$Potability,
          rf_preds) |> 
  accuracy(truth = truth, estimate = .pred_class)



