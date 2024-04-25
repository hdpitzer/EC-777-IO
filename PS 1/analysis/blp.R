
################################################################################

## BLP -- 2b
## ECON 777 PS 1
## Author: Hannah Pitzer

# written based on code from Evan Saltzman

################################################################################



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


compute.jacobian <- function(theta2,ind_choice_matrix) {
  
  partial_share_partial_delta <- matrix(0,total_offerings,total_offerings)
  
  for(i in 1:ns) {
    shares <- ind_choice_matrix[,i]
    
    # Cross Partials
    partials_ind <- (shares %*% t(shares))
    
    # Own Partials
    diag(partials_ind) <- shares * (1-shares)
    
    # Add to population partial matrix
    partial_share_partial_delta <- partial_share_partial_delta + partials_ind
  }
  partial_share_partial_delta <- 1/ns * partial_share_partial_delta  * I_market
  
  partial_share_partial_theta2 <- matrix(0,total_offerings,length(theta2))
  
  for(l in 1:length(theta2)) { 
    
    partials_ind_covariate <- rep(0,length(total_offerings))
    
    for(i in 1:ns) { # loop through each individual			
      shares <- ind_choice_matrix[,i]
      inner_sum_by_market <- by(X2[,l] * shares,market.df$rating_area,sum)
      partials_ind_covariate <- partials_ind_covariate + 
        V[l,i] * shares * (X2[,l] - inner_sum_by_market[as.character(market.df$rating_area)])
    }
    partial_share_partial_theta2[,l] <- partials_ind_covariate
  }
  partial_share_partial_theta2 <- 1/ns * partial_share_partial_theta2
  
  jacobian <- matrix(0,total_offerings,length(theta2))
  for(t in unique(market.df$rating_area)) {
    market_indices <- which(market.df$rating_area == t)
    jacobian[market_indices,] <- 
      solve(partial_share_partial_delta[market_indices,market_indices]) %*% 
      partial_share_partial_theta2[market_indices,] 
  }
  
  return(jacobian)
}

# BLP standard errors
# Variance-Covariance matrix 

compute.BLP.standard.errors <- function(theta2,xi,ind_choice_matrix) {
  
  jacobian <- compute.jacobian(theta2,ind_choice_matrix)
  N <- total_offerings
  
  # Standard Errors for theta2
  
  S <- matrix(0,dim(Z)[2],dim(Z)[2])
  for(j in 1:N) {
    Z_j <- Z[j,]
    xi_j <- xi[j,]
    S <- S + 1/N * (Z_j %*% t(xi_j) %*% xi_j %*% t(Z_j)) # same dimension as W
  }
  
  A <- t(jacobian) %*% Z %*% W %*% t(Z) %*% jacobian 
  B <- t(jacobian) %*% Z %*% W %*% S %*% W %*% t(Z) %*% jacobian
  
  # Apply sandwich formula and compute standard errors
  VCOV <- (solve(A) %*% B %*% solve(A))
  standard_errors <- sqrt(diag(VCOV))
  
  # Standard Errors for theta1
  
  A <- t(X1) %*% Z %*% W %*% t(Z) %*% X1
  B <- t(X1) %*% Z %*% W %*% S %*% W %*% t(Z) %*% X1
  
  # Apply sandwich formula and compute standard errors
  VCOV <- (solve(A) %*% B %*% solve(A))
  standard_errors <- c(sqrt(diag(VCOV)),standard_errors)
  
  
  return(standard_errors)
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
market.df$xi <- xi

# Estimated price coefficient
alpha <- as.numeric(-theta1["avg_price",])

# Compute individual choice probabilities
ind_choice_matrix <- predict.shares(theta2,delta)$ind_choice_matrix

# Compute standard errors
standard_errors <- compute.BLP.standard.errors(theta2,xi,ind_choice_matrix)

# Run Nested Logit to include in summary table
nested_logit <- lm(mean_util_hh ~ av + hmo + avg_price + log(nest_share_hh), 
                 data = market.df)
summary(nested_logit)

# Create a fake blp output object to input into table
market.df$sigma1 <- rnorm(total_offerings)
market.df$sigma2 <- rnorm(total_offerings)
market.df$sigma3 <- rnorm(total_offerings)
#market.df$sigma4 <- rnorm(total_offerings)

blp_fake <- lm(mean_util_hh ~ av + hmo + avg_price + sigma1 + sigma2 + sigma3 ,
               data = market.df)

pvalues_logit <- 2 * pnorm(abs(coef(logit)/sqrt(diag(vcov(logit)))), lower.tail=FALSE)
pvalues_nested_logit <- 2 * pnorm(abs(coef(nested_logit)/sqrt(diag(vcov(nested_logit)))), lower.tail=FALSE)
pvalues_blp <- 2 * pnorm(abs(c(theta1,theta2)/standard_errors), lower.tail=FALSE)

latex_tab <- texreg(list(logit, nested_logit,blp_fake),
                    custom.model.names=c("Logit","Nested Logit","BLP"),
                    custom.coef.names=c("Intercept","AV","HMO","Premium",
                           "Log Within Nest Share","St. Dev. Intercept",
                           "St. Dev. AV","St. Dev. HMO"),
                    override.coef=list(coef(logit),coef(nested_logit),c(theta1,abs(theta2))),
                    override.se=list(sqrt(diag(vcov(logit))),sqrt(diag(vcov(nested_logit))),standard_errors), 
                    override.pval=list(pvalues_logit,pvalues_nested_logit,pvalues_blp),
                    digits=3,caption="BLP Estimates",
                    include.rsquared = FALSE, include.adjrs = FALSE, include.nobs = TRUE, 
                    include.fstatistic = FALSE, inline.css = FALSE, include.rmse = FALSE,
                    caption.above = TRUE)	

writeLines(latex_tab, "PS 1/output/blp_table.tex")