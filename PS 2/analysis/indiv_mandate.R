
################################################################################

## Individual Mandate Simulation -- 2
## ECON 777 PS 2
## Author: Hannah Pitzer

# written with help from Shirley Cai

################################################################################

mkt_all.df <-market.df %>%
  filter(year == 2018)

market.df <- market.df %>%
  rename(price_pp = avg_pp_price)


logit <- lm(mean_util_indv ~ av + hmo + price_pp, 
            data = market.df)
alpha <- coef(logit)["price_pp"]


# Functions --------------------------------------------------------------------

#' Predicts new consumer plan choice given new prices 
#' 
#' @param sim.df A df that includes all household-level data for simulation
#' @param logit A fixest object that describes the logit demand model 
#' @param new_price A vector of new consumer-facing choices
#' @returns A df of unique households 
predict_choice <- function(sim.df, logit, new_price){
  old_price <- sim.df$price_pp 
  sim.df$price_pp <- new_price
  
  # Predict utilities
  sim.df$sim_util <- predict(logit, newdata = sim.df)
  sim.df <- sim.df %>% 
    mutate(
      choice_prob = inv.logit(sim_util), 
      new_price_pp = new_price, 
      price_pp = old_price
    )
  sim.df$new_price <- new_price 
  sim.df$price_pp <- old_price
  
  # Predict chosen plan 
  sim_choice.df <- sim.df %>% 
    group_by(household_id) %>% 
    slice_max(choice_prob, n = 1, with_ties = TRUE) %>% 
    rename(sim_choice = plan_name)
  set.seed(987)
  sim_choice.df <- sim_choice.df %>% 
    group_by(household_id) %>% 
    slice_sample(n = 1)
  
  return(sim_choice.df)
}

#' Simulates premiums and consumer choices when penalty is changed
#' 
#' @param sim.df A df that includes all household-level data for simulation
#' @param mkt_all.df A df that includes all market data for simulation
#' @param logit A fixest object that describes the logit demand model 
#' @param new_penalty A vector of new penalties 
#' @returns A list including a df of simulated choices and a df of simulated premiums
sim_penalty <- function(sim.df, mkt_all.df, logit, new_penalty){
  alpha <- coef(logit)["price_pp"]
  
  # Predict new consumer choice given new penalty
  sim.df <- sim.df %>% 
    mutate(
      new_price_pp = ((pmax(0, mon_premium - subsidy) - new_penalty) * 12) / household_size, 
      insurer = ifelse(is.na(insurer), "Uninsured", insurer)
    )
  sim_choice.df <- predict_choice(sim.df, logit, sim.df$new_price_pp)
  
  # Calculate new market shares 
  sim_mkt.df <- sim_choice.df %>% 
    group_by(rating_area, sim_choice) %>% 
    summarise(n_indv = sum(household_size))
  sim_mkt.df <- sim_mkt.df %>% 
    group_by(rating_area) %>% 
    mutate(sim_share = n_indv / sum(n_indv)) %>% 
    dplyr::select(!n_indv)
  
  # Calculate new ownership matrix 
  sim_mkt.df <- sim_mkt.df %>% 
    left_join(mkt_all.df, by = c('rating_area' = 'rating_area',
                                 'sim_choice' = 'plan')) %>% 
    filter(sim_choice != "Uninsured", !is.na(insurer))
  sim_Dfirm <- sapply(sim_mkt.df$insurer, FUN = is_equal, sim_mkt.df$insurer)
  sim_D <- sim_Dfirm * get_mnl_partials(alpha, sim_mkt.df$sim_share)
  sim_mkt.df$sim_premium <- (sim_mkt.df$mc + solve(sim_D) %*% sim_mkt.df$sim_share)[,1]
  
  # Return simulated choices and simulated premiums
  return(list(sim_choice.df = sim_choice.df, sim_mkt.df = sim_mkt.df))
}


#' Returns whether a equals b
#' 
#' @param a 
#' @param b 
#' @returns A boolean 
is_equal <- function(a, b) {
  return(a == b)
}


#' Returns a matrix of logit partials (ds/dp)
#' 
#' @param alpha A scalar of estimated price parameter
#' @param share A vector of market shares 
#' @returns A matrix 
get_mnl_partials <- function(alpha, share){
  partials <- -alpha * tcrossprod(share, share) 
  diag(partials) <- alpha * share * (1 - share) 
  return(partials)
}

#' Calculates price elasticities from a logit price parameter. 
#' 
#' @param alpha A scalar of estimated price parameter
#' @param share A vector of market shares
#' @param price A vector of prices 
#' @param Dmarket A matrix that indicates which observations are in the same market
#' @returns A vector of own-price and cross-price elasticities. Own-price elasticity is returned as a positive value. 
compute_mnl_elas <- function(alpha, share, price, Dmarket){
  # Compute elasticity matrix 
  partials <- get_mnl_partials(alpha, share) * Dmarket
  elas_mat <- t(tcrossprod(price, 1/share)) * partials
  
  # Own-price elasticity 
  own_elas <- mean(diag(elas_mat), na.rm = TRUE)
  
  # Cross-price elasticity 
  diag(elas_mat) <- 0
  cross_elas <- sum(elas_mat) / sum(elas_mat > 0)
  
  return(c(-own_elas, cross_elas))
}

#' Calculates price elasticities from a nested logit price parameter. See http://www.econ.ucla.edu/ackerber/acnew5.pdf for derivations.
#' 
#' @param alpha A scalar that is the estimated price parameter
#' @param sigma A scalar that is the estimated nesting parameter
#' @param share A vector of market shares
#' @param nest_share A vector within nest shares
#' @param price A vector of prices 
#' @param Dmarket A matrix that indicates which observations are in the same market
#' @param Dnest A matrix that indicates which observations are in the same nest, excluding itself
#' @returns A vector of own-price elasticity and cross-price elasticities within and outside of nest. Own-price elasticity is returned as a positive value. 
compute_nested_elas <- function(alpha, sigma, share, nest_share, price, Dmarket, Dnest){
  Ddiff <- abs(1-Dnest)
  
  # Compute elasticity matrix 
  partials_same <- -alpha * (sigma/(1-sigma) * nest_share + share) %*% t(share) 
  partials_diff <- -alpha * tcrossprod(share, share) 
  partials <- (partials_same * Dnest + partials_diff * Ddiff) * Dmarket
  diag(partials) <- alpha * share * (1/(1-sigma) - sigma/(1-sigma)*nest_share - share) 
  elas_mat <- t(tcrossprod(price, 1/share)) * partials
  
  # Own-price elasticity 
  own_elas <- mean(diag(elas_mat), na.rm = TRUE)
  
  # Cross-price elasticity within nest 
  elas_mat_same <- elas_mat * Dnest
  diag(elas_mat_same) <- 0
  cross_elas_same <- sum(elas_mat_same) / sum(elas_mat_same > 0)
  
  # Cross-price elasticity outside of nest 
  elas_mat_diff <- elas_mat * Ddiff
  diag(elas_mat_diff) <- 0
  cross_elas_diff <- sum(elas_mat_diff) / sum(elas_mat_diff > 0)
  
  return(c(-own_elas, cross_elas_same, cross_elas_diff))
}


## Marginal cost

Dfirm <- sapply(mkt_all.df$insurer, FUN = is_equal, mkt_all.df$insurer)
D <- Dfirm * get_mnl_partials(alpha, mkt_all.df$mkt_share_indv)
mkt_all.df$mc <- (mkt_all.df$premium_pp - solve(D) %*% mkt_all.df$mkt_share_indv)[,1]


# Simulate mandate repeal 

sim.df <- sim.df %>% 
  filter(year == 2018) 

orig_price <- sim.df$price_pp
new_penalty <- rep(0, nrow(sim.df))

## Predict new shares and premiums 

out <- sim_penalty(sim.df, mkt_all.df, logit, new_penalty)
sim_choice.df <- out$sim_choice.df
sim_mkt.df<- out$sim_mkt.df
rm(out)

## Calculate effects of repeal 

sim_mkt.df <- sim_mkt.df %>% 
  mutate(perc_change_prem = (sim_premium - premium_pp) / abs(premium_pp) * 100)

sim_choice.df <- sim_choice.df %>% 
  mutate(exchange = ifelse(choice == "Uninsured", 0, 1), 
         sim_exchange = ifelse(sim_choice == "Uninsured", 0, 1))

## Format 
# Change in premiums 
premium_perc <- sim_mkt.df %>% 
  ungroup() %>% 
  summarize(perc_change = mean(perc_change_prem)) %>% 
  add_column(effect = c("Premiums"), .before = 1)

# Total enrollment 
enroll_tot <- sim_choice.df %>% 
  ungroup() %>% 
  summarize(
    actual = sum(household_size * exchange), 
    simulated = sum(household_size * sim_exchange)
  ) %>% 
  mutate(perc_change = (simulated - actual) / abs(actual) * 100) %>% 
  dplyr::select(perc_change) %>% 
  add_column(effect = c("Enrollment"), .before = 1)

no_penalty <- rbind(premium_perc, enroll_tot)
rm(list = c("premium_perc", "enroll_tot"))

tab_no <- gt(no_penalty) %>% 
  cols_label(
    effect = "Effect", 
    perc_change = "Change (%)"
  ) %>% 
  fmt_number(decimals = 3) %>% 
  opt_horizontal_padding(scale = 3)




# Simulate a different penalty 

temp.df <- sim.df %>% 
  filter(plan_name != "Uninsured") %>% 
  dplyr::select(household_id, premium_pp) %>% 
  group_by(household_id) %>% 
  summarize(
    min_premium = min(premium_pp)
  ) %>% 
  ungroup()

sim.df <- sim.df %>% 
  left_join(temp.df, by = c("household_id" = "household_id"))

## Predict new shares and premiums 

out <- sim_penalty(sim.df, mkt_all.df, logit, sim.df$min_premium)
sim_choice.df <- out$sim_choice.df
sim_mkt.df<- out$sim_mkt.df
rm(out)

## Calculate effects of repeal 

sim_mkt.df <- sim_mkt.df %>% 
  mutate(perc_change_prem = (sim_premium - premium_pp) / abs(premium_pp) * 100)

sim_choice.df <- sim_choice.df %>% 
  mutate(exchange = ifelse(choice == "Uninsured", 0, 1), 
         sim_exchange = ifelse(sim_choice == "Uninsured", 0, 1))

## Format 
# Change in premiums 
premium_perc <- sim_mkt.df %>% 
  ungroup() %>% 
  summarize(perc_change = mean(perc_change_prem)) %>% 
  add_column(effect = c("Premiums"), .before = 1)

# Total enrollment 
enroll_tot <- sim_choice.df %>% 
  ungroup() %>% 
  summarize(
    actual = sum(household_size * exchange), 
    simulated = sum(household_size * sim_exchange)
  ) %>% 
  mutate(perc_change = (simulated - actual) / abs(actual) * 100) %>% 
  dplyr::select(perc_change) %>% 
  add_column(effect = c("Enrollment"), .before = 1)

new_penalty <- rbind(premium_perc, enroll_tot)
rm(list = c("premium_perc", "enroll_tot"))

tab_new <- gt(new_penalty) %>% 
  cols_label(
    effect = "Effect", 
    perc_change = "Change (%)"
  ) %>% 
  fmt_number(decimals = 3) %>% 
  opt_horizontal_padding(scale = 3)



# Export results ---------------------------------------------------------------

gtsave(tab_no, "PS 2/output/sim-no-penalty.tex")
gtsave(tab_new, "PS 2/output/sim-new-penalty.tex")

