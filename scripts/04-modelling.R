# Kaggle Melbourne Housing Dataset
# 
# source data:
#   https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market




# Setup -------------------------------------------------------------------

# Uncomment code below to install packages for modelling!

# install.packages("xgboost")
# install.packages("glmnet")


library(tidyverse)
library(tidymodels)



# Data Preparation --------------------------------------------------------


# import data
data_path <- "./data/MELBOURNE_HOUSE_PRICES_LESS.csv"

housing <- read_csv(data_path)

# clean
housing <- 
  housing |> 
  filter(!is.na(Price)) |> 
  mutate(Suburb = factor(Suburb),
         Type = factor(Type),
         Method = factor(Method),
         SellerG = factor(SellerG),
         Date = dmy(Date),
         Postcode = factor(Postcode),
         Regionname = factor(Regionname))

purrr::map(housing, function(x) {sum(is.na(x))})
purrr::map(select_if(housing, is.factor), function(x) {levels(x) |> length()})

# In python, we can also use map function like this
#   list(map(lambda x: len(x), iterators))



# Data splitting ----------------------------------------------------------

# `strata` argument
#   A variable in data (single character or name) used to conduct stratified
#   sampling. When not NULL, each resample is created within the stratification
#   variable. Numeric strata are binned into quartiles.

housing_split <- initial_split(housing, prop = 0.8, strata = Price)

housing_train <- training(housing_split)
housing_test <- testing(housing_split)



# Basic modelling ---------------------------------------------------------

# Build model
lm_model <- lm(Price ~ Rooms + Propertycount, data = housing_train)

summary(lm_model)

# Make predictions
lm_preds <- tibble(truth = housing_test$Price, 
                   estimate = predict(lm_model, housing_test))




# Tidy modelling ----------------------------------------------------------

# define model specification
tidy_lm_model <- linear_reg(mode = "regression", engine = "lm")


# build model using formula
tidy_lm_fit <- fit(tidy_lm_model,
                   formula = Price ~ Rooms + Propertycount,
                   data = housing_train)

summary(tidy_lm_fit$fit)

# make predictions
tidy_lm_preds <-
  select(housing_test, Price) |> 
  bind_cols(predict(tidy_lm_fit, housing_test))



# build model using x and y
tidy_lm_fit_xy <- fit_xy(tidy_lm_model,
                         x = select(housing_train, Rooms, Propertycount),
                         y = pull(housing_train, Price))

summary(tidy_lm_fit_xy$fit)

# make predictions
tidy_lm_preds_xy <-
  select(housing_test, Price) |> 
  bind_cols(predict(tidy_lm_fit_xy, housing_test))



# Customizing Engines and Parameters --------------------------------------

tidy_glmnet_preds_1 <-
  linear_reg(engine = "glmnet", penalty = 2, mixture = 0.5) |> 
  fit(formula = Price ~ Rooms + Propertycount, data = housing_train) |> 
  predict(housing_test)

tidy_glmnet_preds_2 <-
  linear_reg(engine = "glmnet", penalty = 5, mixture = 0.8) |> 
  fit(formula = Price ~ Rooms + Propertycount, data = housing_train) |> 
  predict(housing_test)

tidy_glmnet_preds_3 <-
  linear_reg(engine = "glmnet", penalty = 10, mixture = 0.2) |> 
  fit(formula = Price ~ Rooms + Propertycount, data = housing_train) |> 
  predict(housing_test)



# Using Different Models --------------------------------------------------

# boosted tree
tidy_btree_preds <- 
  boost_tree(mode = "regression") |> 
  fit(formula = Price ~ Rooms + Propertycount, data = housing_train) |> 
  predict(housing_test)
  
# generative additive models
tidy_gam_preds <-
  gen_additive_mod(mode = "regression") |> 
  fit(formula = Price ~ Rooms + Propertycount, data = housing_train) |> 
  predict(housing_test)




# Evaluation --------------------------------------------------------------


# evaluate manually
lm_mae <- mean(abs(lm_preds$truth - lm_preds$estimate))
lm_mse <- mean((lm_preds$truth - lm_preds$estimate) ^ 2)
lm_rmse <- sqrt(lm_mse)

tibble(.metric = c("MAE", "RMSE"),
       .estimate = c(lm_mae, lm_rmse))


# evaluate using yardstick
eval_metric <- metric_set(rmse, mae)

eval_metric(tidy_lm_preds, truth = Price, estimate = .pred)
eval_metric(tidy_lm_preds_xy, truth = Price, estimate = .pred)


# evaluate all model predictions

model_IDs <- c("linear model", "elnet 1", "elnet 2", "elnet 3", "btree", "GAM")
              
list(select(tidy_lm_preds, .pred),
     tidy_glmnet_preds_1,
     tidy_glmnet_preds_2,
     tidy_glmnet_preds_3,
     tidy_btree_preds,
     tidy_gam_preds) |> 
  map(function(pred) select(housing_test, Price) |> bind_cols(pred)) |> 
  map(function(pred) eval_metric(pred, truth = Price, estimate = .pred)) |> 
  map2_dfr(model_IDs, function(pred, ID) {mutate(pred, Model_ID = ID)}) |> 
  select(Model_ID, .metric, .estimate) |> 
  pivot_wider(names_from = .metric, values_from = .estimate) |> 
  setNames(c("Model_ID", "RMSE", "MAE")) |> 
  arrange(RMSE)

