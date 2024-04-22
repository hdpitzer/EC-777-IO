
################################################################################

## Data Cleaning
## ECON 777 PS 1
## Author: Hannah Pitzer

################################################################################

library(tidyverse)
library(dplyr)


# Import data

house.df <- read_csv("PS 1/data/input/households777.csv") %>%
  dplyr::select(!'...1') %>%
  rename_with(tolower)

house_plan.df <- read_csv("PS 1/data/input/household_plan_year777.csv") %>%
  dplyr::select(!'...1') %>%
  rename_with(tolower)

plan.df <- read_csv("PS 1/data/input/plans777.csv") %>%
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


# Merge plan data

market.df <- market.df %>%
  group_by(rating_area, year) %>%
  mutate(
    mkt_share_indv = n_indv / sum(n_indv),
    mkt_share_hh = n_household / sum(n_household)
  ) %>%
  left_join(plan.df, by = c('choice' = 'plan_name')) %>%
  rename(plan = choice)


# Hausman instruments

market.df <- market.df %>%
  group_by(plan, year) %>%
  mutate(
    plan_yr_avg_price = mean(avg_price),
    plan_yr_avg_pp_price = mean(avg_pp_price),
    n_plan_yr = n()
  ) %>%
  ungroup() %>%
  mutate(
    hausman = (n_plan_yr*plan_yr_avg_price - avg_price) / (n_plan_yr -1), 
    pp_hausman = (n_plan_yr*plan_yr_avg_pp_price - avg_pp_price) / (n_plan_yr - 1)
  ) %>%
  dplyr::select(!c(plan_yr_avg_price, plan_yr_avg_pp_price, n_plan_yr))


# Mean utility -- Berry

market.df <- market.df %>%
  group_by(rating_area, year) %>%
  mutate(
    mean_util_indv = log(mkt_share_indv) - log(mkt_share_indv[plan == "Uninsured"]), 
    mean_util_hh = log(mkt_share_hh) - log(mkt_share_hh[plan == "Uninsured"])
  )


# nest shares

market.df <- market.df %>%
  group_by(rating_area, year, metal_level) %>%
  mutate(
    nest_share_indv = n_indv / sum(n_indv), 
    nest_share_hh = n_household / sum(n_household) 
  )

market.df <- market.df %>%
  filter(plan != "Uninsured")



# Export data

write_tsv(house.df, "PS 1/data/output/indv_data.txt")
write_tsv(market.df, "PS 1/data/output/market_data.txt")

subset.df <- market.df %>%
  filter(rating_area <= 3)

write_tsv(subset.df, "PS 1/data/output/market_subset.txt")
