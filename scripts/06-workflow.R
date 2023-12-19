# Stars dataset
# 
# Source: https://github.com/YBIFoundation/Dataset/raw/main/Stars.csv





# Setup -------------------------------------------------------------------

# install.packages("dbarts")

library(tidyverse)
library(tidymodels)





# Data Preparation --------------------------------------------------------

# Import data from the internet

stars <-
  read_csv('https://github.com/YBIFoundation/Dataset/raw/main/Stars.csv') |> 
  rename_with(function(x) {str_replace_all(x, " \\(.*\\)$", "")}) |> 
  janitor::clean_names("upper_camel")

stars <-
  stars |> 
  mutate(StarType = factor(StarType),
         StarCategory = factor(StarCategory),
         StarColor = factor(StarColor),
         SpectralClass = factor(SpectralClass))


purrr::map_dbl(stars, ~ unique(.) |> length())
purrr::map_dbl(stars, ~ mean(!is.na(.)))


# Simulate missing values

n_missing <- 35

row_index <- runif(n_missing, 1, nrow(stars)) |> as.integer()
col_index <- runif(n_missing, 1, ncol(stars) - 1) |> as.integer()

for (i in seq_len(n_missing)) {
  stars[row_index[i], col_index[i]] <- NA
}





# Split Data --------------------------------------------------------------

# Split data into train and test

set.seed(123)
stars_split <- initial_split(stars, prop = 0.8, strata = SpectralClass)

stars_train <- training(stars_split)
stars_test <- testing(stars_split)






# Create a Recipe and a Model ---------------------------------------------

stars |> count(StarColor) |> arrange(n) |> mutate(prop = n / sum(n))

# Define the model

stars_rec <-
  recipe(SpectralClass ~ ., data = stars_train) |> 
  # impute missing values
  step_impute_median(Radius, Luminosity) |> 
  step_impute_mean(Temperature, AbsoluteMagnitude) |> 
  step_impute_knn(StarType, StarCategory) |> 
  step_impute_mode(StarColor) |> 
  # transform nominal predictors
  step_other(StarColor, threshold = 0.2) |> 
  step_dummy(StarCategory, StarType, StarColor, one_hot = TRUE) |> 
  # transform numeric predictors
  step_center(Radius, Luminosity, Temperature) |> 
  step_scale(Radius, Luminosity, Temperature) |> 
  # remove zero-variance columns
  step_zv(all_predictors())


# Define a model

stars_model <- rand_forest(mode = "classification", trees = 50)





# Using `workflow` --------------------------------------------------------

# Create a workflow

stars_wflow <- 
  workflow(spec = stars_model) |> 
  # add_model(stars_model) |> 
  add_recipe(stars_rec)


# Train the workflow (incl. the recipe and model)

stars_fit <- fit(stars_wflow, data = stars_train)

stars_fit

extract_fit_parsnip(stars_fit)
extract_recipe(stars_fit)

extract_fit_parsnip(stars_fit) |> extract_fit_engine()




# Generate predictions

predict(stars_fit, stars_test)

preds <- bind_cols(select(stars_test, SpectralClass),
                   predict(stars_fit, stars_test))

accuracy(preds, truth = SpectralClass, estimate = .pred_class)
