# Kaggle Melbourne Housing Dataset
# 
# source data:
#   https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market





# Setup -------------------------------------------------------------------

# Un-comment code below to install packages!

# install.packages("tidyverse")
# install.packages("tidymodels")
# install.packages("xgboost")
# install.packages("glmnet")
# install.packages("scales")


# load packages

library(tidyverse)
library(tidymodels)





# Data Preparation --------------------------------------------------------


# import data

data_path <- "./data/MELBOURNE_HOUSE_PRICES_LESS.csv"

housing <- read_csv(data_path)
str(housing)
glimpse(housing)


# clean data
purrr::map(housing, function(x) {sum(is.na(x))})

housing <- 
  housing |> 
  filter(!is.na(Price)) |> 
  mutate(Suburb = factor(Suburb),
         Type = factor(Type),
         Method = factor(Method),
         SellerG = factor(SellerG),
         Date = dmy(Date),
         # Day = day(Date),
         # Month = month(Date),
         # Year = year(Date),
         Postcode = factor(Postcode),
         Regionname = factor(Regionname))

purrr::map(select_if(housing, is.factor), function(x) {levels(x) |> length()})

# In python, we can also use map function like this
#   list(map(lambda x: len(x), iterators))





# Data splitting ----------------------------------------------------------

# Split data into train (80%) and test (20%)

# `strata` argument
#   A variable in data (single character or name) used to conduct stratified
#   sampling. When not NULL, each resample is created within the stratification
#   variable. Numeric strata are binned into quartiles.

set.seed(123)
housing_split <- initial_split(housing, prop = 0.8, strata = Price)

housing_train <- training(housing_split)
housing_test <- testing(housing_split)





# Basic modelling ---------------------------------------------------------

# Build model

lm_model <- lm(Price ~ Rooms + Propertycount, data = housing_train)

summary(lm_model)

# Di scikit-learn (python)
#   lm_model = LinearRegression()
#   lm_model.fit(X, y)


# Make predictions

predict(lm_model, data = housing_test)

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
predict(tidy_lm_fit, housing_test)

tidy_lm_preds <-
  select(housing_test, Price) |> 
  bind_cols(predict(tidy_lm_fit, housing_test))



# build model using x and y

tidy_lm_fit_xy <- fit_xy(tidy_lm_model,
                         x = select(housing_train, Rooms, Propertycount),
                         y = pull(housing_train, Price))

summary(tidy_lm_fit_xy$fit)
extract_fit_engine(tidy_lm_fit_xy)


# make predictions
predict(tidy_lm_fit_xy, housing_test)

tidy_lm_preds_xy <-
  select(housing_test, Price) |> 
  bind_cols(predict(tidy_lm_fit_xy, housing_test))





# Customizing Engines and Parameters --------------------------------------

# ridge regression, lasso regression, elastic net regression

# loss function: untuk mengupdate parameter model (proses training)
# performance metrics: untuk membandingkan model (di akhir)

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

# random forest
tidy_btree_preds <- 
  rand_forest(mode = "regression") |> 
  fit(formula = Price ~ Rooms + Propertycount, data = housing_train) |> 
  predict(housing_test)

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

rmse(tidy_lm_preds, truth = Price, estimate = .pred)
mae(tidy_lm_preds, truth = Price, estimate = .pred)

eval_metrics <- metric_set(rmse, mae)

eval_metrics(tidy_lm_preds, truth = Price, estimate = .pred)
eval_metrics(tidy_lm_preds_xy, truth = Price, estimate = .pred)


# evaluate all model predictions

model_IDs <- c("Linear Model", "Elastic Net 1", "Elastic Net 2",
               "Elastic Net 3", "XGBoost", "GAM")

results <- 
  list(select(tidy_lm_preds, .pred),
       tidy_glmnet_preds_1,
       tidy_glmnet_preds_2,
       tidy_glmnet_preds_3,
       tidy_btree_preds,
       tidy_gam_preds) |> 
  map(function(pred) select(housing_test, Price) |> bind_cols(pred)) |> 
  map(function(pred) eval_metrics(pred, truth = Price, estimate = .pred)) |> 
  map2_dfr(model_IDs, function(pred, ID) {mutate(pred, Model_ID = ID)}) |> 
  select(Model_ID, .metric, .estimate) |> 
  pivot_wider(names_from = .metric, values_from = .estimate) |> 
  setNames(c("Model_ID", "RMSE", "MAE")) |> 
  arrange(RMSE)

results


# plotting the results

ggplot(data = pivot_longer(results, -Model_ID, names_to = "metric"),
       aes(y = Model_ID, x = value)) +
  geom_col(aes(fill = metric), colour = "black", position = "dodge") +
  labs(title = "Models Performance", 
       subtitle = "Comparison between Linear Regression, Elastic Net, Boosted Trees, and GAM",
       caption = "Kaggle - Melbourne Housing Market",
       x = NULL, y = NULL) +
  facet_grid(metric ~ .) +
  scale_x_continuous(labels = scales::number) +
  scale_fill_brewer(type = "qual", palette = 2, guide = "none") +
  theme_light() +
  theme(plot.title.position = "plot",
        strip.text = element_text(face = "bold"))
