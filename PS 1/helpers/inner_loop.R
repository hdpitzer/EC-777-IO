
################################################################################

## BLP -- Inner Loop
## ECON 777 PS 1
## Author: Hannah Pitzer

# written with assistance from Michaela Philip

################################################################################


# Calculate mu 
get_mu <- function(x2, theta, nu) {
  sigma <- diag(theta)
  return(x2 %*% sigma %*% nu)
}

# Predicted shares
predict_share <- function(delta, mu) {
  J <- length(delta)
  prob <- exp(delta + mu) 
  sum_prob <- 1 + sum(prob)
  pred_share <- prob / sum_prob
  return(pred_share)
}


## Inner loop algorithm
# 1 Predicted shares (avg over nu)
predict_share_avg <- function(delta, x2, theta, nus) {
  R <- nrow(nus)
  pred_share <- rep(0, length(delta))
  for (r in 1:R) {
    mu_r <- get_mu(x2, theta, nus[r ,])
    pred_share_r <- predict_share(delta, mu_r)
    pred_share <- pred_share + pred_share_r
  }
  pred_share <- pred_share / R
  return(pred_share)
}

# 2 Contraction mapping
contraction_map <- function(pred_share, observe_share, initial) {
  return(initial + log(observe_share / pred_share))
}

# 3 Create loop
inner_loop <- function(x2, theta, nus, observe_share, initial, max_iter = 1000, tol = 1e-7) {
  for (i in 1:max_iter) {
    pred_share <- predict_share_avg(initial, x2, theta, nus)
    delta <- contraction_map(pred_share, observe_share, initial)
    if (max (abs(delta - initial), na.rm=TRUE) < tol) {
        break
      }
      initial <- delta
    }
  return(list(delta = delta, pred_share = pred_share))
}


