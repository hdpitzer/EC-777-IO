
################################################################################

## BLP -- 2b
## ECON 777 PS 1
## Author: Hannah Pitzer

################################################################################

## Describing the steps:

# Slide 52 equation, want random coefs for x_jkt -- sigma_beta_k-> deviation from avg utility
# individual stuff == mu
# pull v from normal dist to calculate mu


#
# get mu -- x_jkt is a subset of X_jt
# take vector do part in brackets-- get mu assuming we have sigma already
# 
# numerator of s_ijt -- slide 53 -- == e^delta+mu
# assuming we have delat and mu -> get predicted shares 
#
# 

## inner loop
#
#Sum all shares and average -- slide 58 step 1 ----gives s_jt
#
# define delta^1 -- slide 58 step 2 
# delta^0 = initial guess
# 
# make loop-- given x_jkt, sigma, v, delta^0, run contraction mapping until difference between delta and delta^0
#
# with delta, obtain xi



## outer loop
#
# call xi, delta from inner loop
# slide 57 step 3 -- GMM objective function
# create moment xi*Z, Z= hausman instruments (+ observed plan characteristics) 
# W -- start with I then use efficient W -------
#
# need to estimate beta - define matrix A = X'ZWWZ'X, B = X'ZWZ'Y (Y=delta from inner loop)
# solve for beta using these matrices
#
# do gmm
# 
# do all of blp, save efficient W, run blp again


source('PS 1/helpers/inner_loop.R')
source('PS 1/helpers/outer_loop.R')

# TEST ########################################################################
# J = 50
# R = 500
# K = 2
# Y = 15
# 
# set.seed(123)  # Set seed for reproducibility
# nus = matrix(rnorm(R * K, 0, 1), nrow = R)
# x2 = matrix(runif(J * K), nrow = J)
# #sigma = matrix(runif(K * K), nrow = K)
# delta = runif(J)
# z = matrix(runif(J * (K+Y)), nrow = J)
# W = diag(K + Y)
# x1 = matrix(runif(J * (K+Y)), nrow = J)
# theta = runif(K)
# 
# # inner loop
# mu_test = get_mu(x2, theta, nus[1 ,])
# share_test = predict_share(delta, mu_test)
# share_0 = predict_share_avg(delta, x2, theta, nus)
# inner_loop_test <- inner_loop(x2, theta, nus, share_0, rep(0, J))
# 
# # outer loop
# beta_test = get_beta(delta, x1, z, W)
# obj_test = gmm_objective(delta, x1, z, beta_test, W)
# outer_loop_test <- outer_loop(x1, z, x2, theta, nus, share_0, W)

###############################################################################
subset.df <- subset.df[subset.df$mkt_share_hh >= 0.01,]

set.seed(456)

# Set up
ns = 500  
covariates <- c("av","hmo")

total_offerings <- dim(subset.df)[1]
observe_share <- subset.df$mkt_share_hh 

nus = matrix(rnorm(ns * (length(covariates)+1), 0, 1), nrow = ns)

x <- as.matrix(cbind(1,subset.df[,c(covariates, "avg_price")]))
x1 <- x[,c(1,covariates, "avg_price")] # variables that enter non-random utility (delta)
x2 <- x[,c(1,covariates)] # variables that enter random part of utility

z <- as.matrix(cbind(1,subset.df[,c(covariates,"hausman")]))

W <- solve(t(z) %*% z)

delta_0 <- rep(0,dim(subset.df)[1])

logit <- lm(mean_util_hh ~ av + hmo + avg_price, 
                data = subset.df)
theta_0 <- coef(logit)[c( "(Intercept)","av","hmo")]

#inner_loop_test <- inner_loop(x2, theta_0, nus, observe_share, delta_0)
theta_hat <- outer_loop(x1, z, x2, theta_0, nus, observe_share, delta_0, W)







