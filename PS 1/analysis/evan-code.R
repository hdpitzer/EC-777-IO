# Setup BLP simulation

set.seed(6)

# Simulation parameters
ns <- 500  # number of simulation draws
tolerance <- 1e-07 # tolerance for error in delta
max.iterations <- 1000 # max number of iterations to run contraction mapping
covariates <- c("av","hmo")
optimization_method <- "simplex"

# Accounting variables
num_markets <- length(unique(market.df$rating_area)) # number of markets
num_products <- length(unique(market.df$plan)) # number of unique products across markets
total_offerings <- dim(market.df)[1] # total offerings in all markets

# Observed market shares
actual_shares <- market.df$mkt_share_hh 

# Generate random terms for each individual and each covariate
# We assume that the same ns simulated consumers make decisions in each market
# You might want to change this assumption in the future, particularly
# if you know something about the consumers in each market
# This matrix is K * ns, where K is the number of covariates plus 1 (constant term)
V <- matrix(rnorm(ns * (length(covariates)+1)),length(covariates)+1,ns)

# Vector of product characteristics and premium
# This matrix is (total_offerings * K)
X <- as.matrix(cbind(1,market.df[,c(covariates,"avg_price")])) # make sure all entries are numbers
X1 <- X[,c(1,covariates,"avg_price")] # variables that enter non-random part of utility (delta)
X2 <- X[,c(1,covariates)] # variables that enter random part of utility


# Instruments
Z <- cbind(X[,c(1,covariates)],market.df$hausman) # instrument matrix 
#Z <- X # instrument matrix (swap out premium for instruments when you have them)
W <- solve(t(Z) %*% Z) # Weight matrix

# Indicator matrix showing which products belong the the same market
market_indicator <- function(x,markets) {
  return(x == markets)
}
I_market <- sapply(market.df$rating_area,FUN=market_indicator,market.df$rating_area)


# Initialize theta = (theta1,theta2) and delta

# theta1 are the non-random parameters that enter delta
# Following Nevo (2000), we don't actually have to
# compute theta1, because it can be written as a 
# function of theta2 using the FOCs.  This is good
# b/c it reduces the number of parameters to estimate.
# theta2 are the random parameters
# delta is mean utility (initialize using logit)

inside_good_share <- by(market.df$mkt_share_hh, market.df$rating_area, sum)
outside_good_share <- 1 - inside_good_share

# Use OLS to estimate parameters
logit <- lm(mean_util_hh ~ av + hmo + avg_price, data = market.df)
summary(logit)

# Compute initial delta 
delta_initial <- predict(logit) 
delta_initial <- rep(0,dim(market.df)[1]) 	

# Unclear what to initialize theta2 to
theta2_initial <- coef(logit)[c( "(Intercept)","av","hmo")]/2


# Now we define several functions that we will call to execute BLP	

# This function computes the predicted shares, given
# the values of theta = (theta1,theta2) and delta.  It is 
# called by the compute.delta function. 

predict.shares <- function(theta2, delta) {
  
  # delta is total_offerings * 1
  # X2 is (total_offerings) * K
  # diag(theta2) is K * K
  # V is K * ns
  
  # Compute numerator in share equation (dim = total_offerings * ns)
  numerator <- exp(as.vector(delta) + (X2 %*% diag(theta2) %*% V))
  
  # Compute denominator in share equation (dim = total_offerings * ns)
  market_denominator <- aggregate(numerator,by = list(market.df$rating_area, market.df$year),sum)
  #rownames(market_denominator) <- market_denominator[,1]
  market_denominator <- as.matrix(1 + market_denominator[,2:(ncol(numerator) + 1)])
  denominator <- market_denominator[market.df$rating_area,]
  
  # Compute individual choices and product shares over individuals
  ind_choice_matrix <- numerator/denominator
  predicted_shares <- rowSums(ind_choice_matrix)/ns
  
  # Check to make sure no NAN's in predicted shares
  if (any(is.nan(predicted_shares))) {
    stop("NANs in predict.shares()")
  }
  
  return(list(predicted_shares = predicted_shares, ind_choice_matrix = ind_choice_matrix))
}

# This function computes the value of delta that matches predicted shares
# to actual shares for a given value of theta = (theta1, theta2)

compute.delta <- function(theta2, delta = delta_initial) {
  
  iteration <- 0
  
  # Do the contraction mapping
  repeat {
    predicted_shares <- predict.shares(theta2,delta)$predicted_shares
    delta.previous <- delta
    delta <- delta.previous + log(actual_shares) - log(predicted_shares)
    
    # End if error within tolerance
    error <- sqrt(sum((delta - delta.previous)^2)/length(delta.previous))
    if (error < tolerance) break
    
    # End if contraction mapping has exceeded max.iterations number of iterations
    iteration <- iteration + 1
    if (iteration > max.iterations) break
  }
  
  return(delta)
}

# This function computes the objective function value

compute.objective.value <- function(theta2) {
  
  print(theta2)
  
  # Compute the mean utility, delta
  delta <- compute.delta(theta2)
  
  # Compute theta1 from theta2 using FOCs
  theta1 <- solve(t(X1) %*% Z %*% W %*% t(Z) %*% X1) %*% t(X1) %*% Z %*% W %*% t(Z) %*% delta
  
  # Compute unobserved product characteristics, xi
  xi <- delta - X1 %*% theta1 
  
  # Calculate objective value
  obj_value <- t(xi) %*% Z %*% W %*% t(Z) %*% xi
  print(obj_value)
  return(obj_value)
}


####### Estimate

if(optimization_method == "simplex") {
  theta2 <- optim(par = theta2_initial,fn = compute.objective.value,method = "Nelder-Mead")$par    
} else if (optimization_method == "gradient") {
  theta2 <- optim(par = theta2_initial,fn = compute.objective.value,gr = compute.gradient, method = "BFGS")    
}	

# Mean utilities
delta <- compute.delta(theta2)

# Linear coefficient estimates 
theta1 <- solve(t(X1) %*% Z %*% W %*% t(Z) %*% X1) %*% t(X1) %*% Z %*% W %*% t(Z) %*% delta

# Unobserved product characteristics
xi <- delta - X1 %*% theta1 
data$xi <- xi

# Estimated price coefficient
alpha <- as.numeric(-theta1["avg_price",])

# Compute individual choice probabilities
ind_choice_matrix <- predict.shares(theta2,delta)$ind_choice_matrix
