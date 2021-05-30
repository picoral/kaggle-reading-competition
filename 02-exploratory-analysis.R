source("01-data-wrangling.R")

# libraries
library(tidytext)

# tokenize data
tokenized_train_data <- train_data %>%
  unnest_tokens(word, excerpt)

# summary of target and error
count_summary <- tokenized_train_data %>%
  group_by(word) %>%
  summarize(n = n(),
            mean_target = mean(target, na.rm = ),
            sd_target = sd(target, na.rm = TRUE)) %>%
  arrange(-n)

count_summary %>%
  summarize(mean(n),
            sd(n))
