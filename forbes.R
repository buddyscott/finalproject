library(tidyverse)
library(readr)

forbes1 <- read_csv("data-files/forbes2020.csv", 
                    col_type = cols(.default = col_character())) %>%
  mutate(rank = str_sub(rank, start = 2)) %>%
  mutate(valuation = substr(valuation, 2, nchar(valuation)-1)) %>%
  mutate(value_change = gsub('.{1}$', '', value_change)) %>%
  mutate(debt_to_value = gsub('.{1}$', '', debt_to_value)) %>%
  mutate(revenue = substr(revenue, 2, nchar(revenue)-1)) %>%
  mutate(operating_income = substr(operating_income, 2, 
                                   nchar(operating_income)-1)) %>%
  mutate(rank = as.numeric(rank)) %>%
  mutate(valuation = as.numeric(valuation)) %>%
  mutate(value_change = as.numeric(value_change)) %>%
  mutate(debt_to_value = as.numeric(debt_to_value)) %>%
  mutate(revenue = as.numeric(revenue)) %>%
  mutate(operating_income = as.numeric(operating_income)) %>%
  subset(select = -rank)

forbes2 <- read_csv("data-files/moreforbes.csv", 
                    col_type = cols(.default = col_double(), 
                                    team = col_character()))

forbes_joined <- inner_join(forbes1, forbes2, by = "team")

full_dataset <- inner_join(forbes_joined, nbainfo, by = "team") %>%
  subset(select = -c(valuation.y, debt_to_value.y, revenue.y, 
                     operating_income.y, percent_change)) %>%
  mutate(valuation = valuation.x) %>%
  mutate(debt_to_value = debt_to_value.x) %>%
  mutate(revenue = revenue.x) %>%
  mutate(operating_income = operating_income.x) %>%
  subset(select = -c(valuation.x, debt_to_value.x, revenue.x, 
                     operating_income.x))