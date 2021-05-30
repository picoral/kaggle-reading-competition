# libraries
library(tidyverse)
library(tidymodels)
library(textrecipes)

# set seed
set.seed(42)

# read data in
all_data <- read_csv("data/train.csv")

# split data
data_split <- initial_split(all_data, prop = .8)

# create train and validation
train_data <- training(data_split)
validation_data <- testing(data_split)

# create recipe
my_text_recipe <- recipe(target ~ excerpt, data = train_data) %>%
  step_tokenize(excerpt) %>%
  step_tokenfilter(excerpt, max_tokens = 100) %>%
  step_tf(excerpt) %>%
  prep()

prepped_train_data <- juice(my_text_recipe)
prepped_validation_data <- bake(my_text_recipe, new_data = validation_data)

# linear regression
lm_model <- linear_reg() %>% 
  set_engine('lm') %>% # adds lm implementation of linear regression
  set_mode('regression')

lm_fit <- lm_model %>%
  fit(target ~ ., data = prepped_train_data)

predict(lm_fit, prepped_validation_data) %>%
  bind_cols(validation_data %>% select(target)) %>%
  mutate(loss = .pred - target)

