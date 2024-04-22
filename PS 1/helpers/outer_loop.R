
################################################################################

## BLP -- Outer Loop
## ECON 777 PS 1
## Author: Hannah Pitzer

# written with assistance from Michaela Philip

################################################################################


# GMM objective function
gmm_objective <- function(delta, x1, z, beta, W) {
  xi <- delta - x1 %*% beta
  mom <- t(z) %*% xi
  temp <- t(mom) %*% W
  return(t(mom) %*% W %*% mom)
}


# Get beta
get_beta <- function(delta, x1, z, W) {
  A <- t(x1) %*% z %*% W %*% t(z) %*% x1
  b <- t(x1) %*% z %*% W %*% t(z) %*% delta
  return(solve(A, b))
}


# Create loop
outer_loop <- function(x1, z, x2, theta, nus, observe_share, initial, W) {
  obj_theta <- function(theta) {
    inner_res <- inner_loop(x2, theta, nus, observe_share, initial)
    delta <- inner_res$delta
    beta <- get_beta(delta, x1, z, W)
    objective <- gmm_objective(delta, x1, z, beta, W)
    return(objective)
  }
  result <- optim(theta, obj_theta, method = "Nelder-Mead")
  return(result)
}