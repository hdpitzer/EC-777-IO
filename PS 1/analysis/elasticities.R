
################################################################################

## Elasticities -- 3a and 3b
## ECON 777 PS 1
## Author: Hannah Pitzer

################################################################################


source("PS 1/analysis/berry_inversion.R")
source("PS 1/analysis/blp.R")


## 3a
# Logit model

logit_alpha <- -coef(logit.ols)["avg_price"]

partials1 <- matrix(0,total_offerings,total_offerings) 
for(i in 1:ns) {
  shares <- ind_choice_matrix[,i]
  
  # Cross Partials
  partials_ind1 <- (logit_alpha * shares %*% t(shares)) * I_market
  
  # Own Partials
  diag(partials_ind1) <- -logit_alpha * shares * (1-shares)
  
  # Add to population partial matrix
  partials1 <- partials1 + partials_ind1
}
partials1 <- 1/ns * partials1

# Own-price
logit_ope <- market.df$avg_price/market.df$mkt_share_hh * diag(partials1)
logit_ope <- mean(logit_ope)

# Cross-price
logit_cpe <- t(market.df$avg_price %*% t(1/market.df$mkt_share_hh)) * partials1 * I_market
diag(logit_cpe) <- 0
logit_cpe <- sum(logit_cpe)/length(which(logit_cpe > 0))


# Nested-logit model

nested_alpha <- -coef(nested.ols)["avg_price"]

partials2 <- matrix(0,total_offerings,total_offerings) 
for(i in 1:ns) {
  shares <- ind_choice_matrix[,i]
  
  # Cross Partials
  partials_ind2 <- (nested_alpha * shares %*% t(shares)) * I_market
  
  # Own Partials
  diag(partials_ind2) <- -nested_alpha * shares * (1-shares)
  
  # Add to population partial matrix
  partials2 <- partials2 + partials_ind2
}
partials2 <- 1/ns * partials2

# Own-price
nested_ope <- market.df$avg_price/market.df$mkt_share_hh * diag(partials2)
nested_ope <- mean(nested_ope)

# Cross-price
nested_cpe <- t(market.df$avg_price %*% t(1/market.df$mkt_share_hh)) * partials2 * I_market
diag(nested_cpe) <- 0
nested_cpe <- sum(nested_cpe)/length(which(nested_cpe > 0))


# BLP model

partials3 <- matrix(0,total_offerings,total_offerings) 
for(i in 1:ns) {
  shares <- ind_choice_matrix[,i]
  
  # Cross Partials
  partials_ind3 <- (alpha * shares %*% t(shares)) * I_market
  
  # Own Partials
  diag(partials_ind3) <- -alpha * shares * (1-shares)
  
  # Add to population partial matrix
  partials3 <- partials3 + partials_ind3
}
partials3 <- 1/ns * partials3

# Own-price
blp_ope <- market.df$avg_price/market.df$mkt_share_hh * diag(partials3)
blp_ope <- mean(blp_ope)

# Cross-price
blp_cpe <- t(market.df$avg_price %*% t(1/market.df$mkt_share_hh)) * partials3 * I_market
diag(blp_cpe) <- 0
blp_cpe <- sum(blp_cpe)/length(which(blp_cpe > 0))


# Create table
row_labels <- c("Own-price elasticity", "Cross-price elasticity")
col_labels <- c("Logit", "Nestsed Logit", "BLP")
values <- matrix(c(logit_ope, nested_ope, blp_ope, logit_cpe, nested_cpe, blp_cpe), nrow = 2, byrow = TRUE)


elasticity_tab <- xtable(values, caption = "Elasticity of Demand")
rownames(elasticity_tab) <- row_labels
colnames(elasticity_tab) <- col_labels


print.xtable(elasticity_tab, file = "PS 1/output/elasticity_table.tex", caption.placement = "top")

  

## 3b
# Logit model

logit_alpha1 <- -coef(logit.ols2)["avg_price"]

partials4 <- matrix(0,total_offerings,total_offerings) 
for(i in 1:ns) {
  shares <- ind_choice_matrix[,i]
  
  # Cross Partials
  partials_ind4 <- (logit_alpha1 * shares %*% t(shares)) * I_market
  
  # Own Partials
  diag(partials_ind4) <- -logit_alpha1 * shares * (1-shares)
  
  # Add to population partial matrix
  partials4 <- partials4 + partials_ind4
}
partials4 <- 1/ns * partials4

# Own-price
logit_ope1 <- market.df$avg_price/market.df$mkt_share_hh * diag(partials4)
logit_ope1 <- mean(logit_ope1)

# Cross-price
logit_cpe1 <- t(market.df$avg_price %*% t(1/market.df$mkt_share_hh)) * partials4 * I_market
diag(logit_cpe1) <- 0
logit_cpe1 <- sum(logit_cpe1)/length(which(logit_cpe1 > 0))


# Nested-logit model

nested_alpha1 <- -coef(nested.ols2)["avg_price"]

partials5 <- matrix(0,total_offerings,total_offerings) 
for(i in 1:ns) {
  shares <- ind_choice_matrix[,i]
  
  # Cross Partials
  partials_ind5 <- (nested_alpha1 * shares %*% t(shares)) * I_market
  
  # Own Partials
  diag(partials_ind5) <- -nested_alpha1 * shares * (1-shares)
  
  # Add to population partial matrix
  partials5 <- partials5 + partials_ind5
}
partials5 <- 1/ns * partials5

# Own-price
nested_ope1 <- market.df$avg_price/market.df$mkt_share_hh * diag(partials5)
nested_ope1 <- mean(nested_ope1)

# Cross-price
nested_cpe1 <- t(market.df$avg_price %*% t(1/market.df$mkt_share_hh)) * partials5 * I_market
diag(nested_cpe1) <- 0
nested_cpe1 <- sum(nested_cpe1)/length(which(nested_cpe1 > 0))




### Organizing

logit_elast <- data.frame(rbind(logit_ope, logit_cpe))

nested_elast <- data.frame(rbind(nested_ope, nested_cpe))

blp_elast <- data.frame(rbind(blp_ope, blp_cpe))

elasticities <- cbind(logit = logit_elast, nested_logit = nested_elast, blp = blp_elast)
rownames(elasticities) <- c('Own Price Elasticity', 'Cross Price Elasticity')
colnames(elasticities) <- c('Logit', 'Nested Logit', 'BLP')
