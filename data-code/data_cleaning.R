
################################################################################

## Data Cleaning
## ECON 777 PS 1
## Author: Hannah Pitzer
## Date created: 3/5/2024
## Last edited: 3/7/2024

################################################################################

library(tidyverse)
library(dplyr)


# Import data

house.df <- read_csv("data/input/households777.csv") %>%
  dplyr::select(!'...1') %>%
  rename_with(tolower)

house_plan.df <- read_csv("data/input/household_plan_year777.csv") %>%
  dplyr::select(!'...1') %>%
  rename_with(tolower)

plan.df <- read_csv("data/input/plans777.csv") %>%
  dplyr::select(!'...1') %>%
  rename_with(tolower)


# Merge household datasets

house.df <- house.df %>%
  left_join(house_plan.df, by = c('household_id' = 'household_id', 
                                  'year' = 'year', 
                                  'choice' = 'plan_name'))

house.df <- house.df %>%
  mutate(
    sub_premium = pmax(0, premium - subsidy),          # Actual premium paid
    price = sub_premium - monthly_penalty,             # Normalize outside option to 0
    pp_sub_premium = sub_premium / household_size,
    pp_price = price / household_size
  )


# Aggregate to market-level

market.df <- house.df %>%
  group_by(rating_area, year, choice) %>%
  summarise(
    n_indv = sum(household_size), 
    n_household = n(), 
    avg_sub_premium = mean(sub_premium), 
    avg_price = mean(price), 
    avg_pp_sub_premium = mean(pp_sub_premium), 
    avg_pp_price = mean(pp_price)
  )

### Add avg market demographics


# Merge plan data

market.df <- market.df %>%
  group_by(rating_area, year) %>%
  mutate(
    mkt_share_indv = n_indv / sum(n_indv),
    mkt_share_hh = n_household / sum(n_household)
  ) %>%
  left_join(plan.df, by = c('choice' = 'plan_name'))


# 