# Glass Classification by UCI Machine Learning 
# 
# Source: https://www.kaggle.com/datasets/uciml/glass

# add section --> ctrl + shift + R
# comment/uncomment --> ctrcl shift c
# add pipe --> ctrl shift m
# restart session --> ctrl shift F10





# Setup -------------------------------------------------------------------

# Un-comment code below to install packages!

# install.packages("tidyverse")
# install.packages("tidymodels")
# install.packages("xgboost")
# install.packages("glmnet")
# install.packages("scales")

library(tidyverse)
library(tidymodels)





# Data Preparation --------------------------------------------------------

data_path <- "./data/glass.csv"

glass <- 
  read_csv(data_path) |> 
  mutate(Type = factor(Type))

purrr::map_dbl(glass, function(x) mean(!is.na(x)))

distinct(glass, Type)
count(glass, Type)


# working with logical vectors

x <- c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE)

mean(x)
sum(x)

c(x, 1) # automatic type casting
c(x, "1")

y <- c(rep("M", 10), rep("F", 15))

y == "M"

y == c("M", "F", "M", "F", "M")

sum(y == "M")
mean(y == "M")




# Split Data --------------------------------------------------------------

# Split data into train and test

set.seed(123)
glass_split <- initial_split(glass, prop = 0.8, strata = Type)

glass_train <- training(glass_split)
glass_test <- testing(glass_split)





# Create Recipe -----------------------------------------------------------

# normalization

x <- c(4,3,3,2,3,1,4,3,3,2,4,5,4,3,5,6,7,5,3,4,5)

mean(x)
sd(x)

y <- (x - mean(x)) / sd(x)

mean(y)
sd(y)


# Define a recipe

glass_rec <- recipe(Type ~ ., data = glass_train)

glass_rec <- step_center(glass_rec, RI, Na, Mg, Al, Si, K, Ca, Ba, Fe)

glass_rec <- step_scale(glass_rec, all_predictors())
  
glass_rec


# Train the recipe (it's different than train a model)

trained_rec <- prep(glass_rec, training = glass_train)
trained_rec



# Apply pre-processing to data

# new_data = NULL or new_data = glass_train
new_glass_train <- bake(trained_rec, new_data = NULL) 

new_glass_test <- bake(trained_rec, new_data = glass_test)

new_glass_train
new_glass_test


# Check the pre-processed data

bind_cols("Variable" = names(glass_train[1:9]),
          purrr::map_dfr(select(glass_train, -Type), function(x) {
            tibble(OLD_mean = mean(x), OLD_stdev = sd(x))
          }),
          purrr::map_dfr(select(new_glass_train, -Type), function(x) {
            tibble(NEW_mean = mean(x), NEW_stdev = sd(x))
          })) |> 
  select(Variable, ends_with("mean"), ends_with("stdev")) |> 
  mutate_if(is.numeric, round, digits = 2)





# Train a Model -----------------------------------------------------------

# With pre-processed data (define > fit > predict)

multinom_model <- multinom_reg(mode = "classification")

multinom_fit <- fit(multinom_model, formula = Type ~ ., data = new_glass_train)

preds_preprocessed <-
  predict(multinom_fit, new_glass_test) |> 
  rename(pred_class_1 = .pred_class)


# With original data

preds_original <- 
  multinom_reg(mode = "classification") |> 
  fit(formula = Type ~ ., data = glass_train) |> 
  predict(glass_test) |> 
  rename(pred_class_2 = .pred_class)





# Evaluate the Model ------------------------------------------------------

predictions <- bind_cols(select(new_glass_test, truth = Type),
                         preds_preprocessed,
                         preds_original)

accuracy(predictions, truth = truth, estimate = pred_class_1)
accuracy(predictions, truth = truth, estimate = pred_class_2)
