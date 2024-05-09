
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



# Clean data for simulation

sim.df <- house_plan.df %>% 
  left_join(house.df, by = c('household_id' = 'household_id',
                                 'year' = 'year')) %>% 
  left_join(plan.df, by = c('plan_name' = 'plan_name'))

sim.df <- sim.df %>% 
  mutate(
    av = ifelse(is.na(av), 0, av),
    hmo = ifelse(is.na(hmo), 0, hmo), 
    mon_premium = premium, 
    mon_sub_premium = pmax(0, mon_premium - subsidy),  # Subsidized premium >= 0
    mon_price = mon_sub_premium - monthly_penalty, 
    premium = mon_premium * 12, 
    sub_premium = mon_sub_premium * 12, 
    price = mon_price * 12, 
    premium_pp = premium / household_size,
    sub_premium_pp = sub_premium / household_size, 
    price_pp = price / household_size
  ) 


# Merge household datasets

house.df <- house.df %>%
  left_join(house_plan.df, by = c('household_id' = 'household_id', 
                                  'year' = 'year', 
                                  'choice' = 'plan_name'))

house.df <- house.df %>%
  mutate(
    sub_premium = pmax(0, premium - subsidy),          # Actual premium paid
    price = sub_premium - monthly_penalty,             # Normalize outside option to 0
    premium_pp = premium / household_size,
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
    premium_pp = mean(premium_pp), 
    avg_pp_sub_premium = mean(pp_sub_premium), 
    avg_pp_price = mean(pp_price),
    perc_0to17 = mean(perc_0to17),
    perc_18to25 = mean(perc_18to25),
    perc_26to34 = mean(perc_26to34),
    perc_35to44 = mean(perc_35to44),
    perc_45to54 = mean(perc_45to54),
    perc_55to64 = mean(perc_55to64),
    perc_65plus = mean(perc_65plus)
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


# Merge instruments into simulation data

market.temp <- market.df %>% dplyr::select(hausman, pp_hausman, rating_area, year, plan)
sim.df <- sim.df %>% 
  left_join(market.temp, by = c('rating_area' = 'rating_area',
                                'choice' = 'plan', 'year' = 'year')) 



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
