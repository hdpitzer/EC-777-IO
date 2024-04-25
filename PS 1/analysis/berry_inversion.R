
################################################################################

## Berry Inversion -- 2a and 2c
## ECON 777 PS 1
## Author: Hannah Pitzer

################################################################################

library(ivreg)
library(modelsummary)

## 2a
# Logit estimation

logit.ols <- lm(mean_util_hh ~ av + hmo + avg_price, 
                data = market.df)
logit.iv1 <- ivreg(mean_util_hh ~ av + hmo | avg_price | hausman, 
                   data = market.df)
logit.iv2 <- ivreg(mean_util_hh ~ as.factor(insurer) + av + hmo | avg_price | hausman, 
                   data = market.df)


# Nested logit

nested.ols <- lm(mean_util_hh ~ av + hmo + avg_price + log(nest_share_hh), 
                 data = market.df)
nested.iv1 <- ivreg(mean_util_hh ~ av + hmo +log(nest_share_hh) | avg_price | hausman, 
                    data = market.df)
nested.iv2 <- ivreg(mean_util_hh ~ as.factor(insurer) + av + hmo + log(nest_share_hh) | avg_price | hausman, 
                    data = market.df)




# Results table

pvalues_logit_ols <- 2 * pnorm(abs(coef(logit.ols)/sqrt(diag(vcov(logit.ols)))), lower.tail=FALSE)
pvalues_logit_iv1 <- 2 * pnorm(abs(coef(logit.iv1)/sqrt(diag(vcov(logit.iv1)))), lower.tail=FALSE)
pvalues_logit_iv2 <- 2 * pnorm(abs(coef(logit.iv2)/sqrt(diag(vcov(logit.iv2)))), lower.tail=FALSE)
pvalues_nested_ols <- 2 * pnorm(abs(coef(nested.ols)/sqrt(diag(vcov(nested.ols)))), lower.tail=FALSE)
pvalues_nested_iv1 <- 2 * pnorm(abs(coef(nested.iv1)/sqrt(diag(vcov(nested.iv1)))), lower.tail=FALSE)
pvalues_nested_iv2 <- 2 * pnorm(abs(coef(nested.iv2)/sqrt(diag(vcov(nested.iv2)))), lower.tail=FALSE)

latex_tab <- texreg(list(logit.ols, logit.iv1,logit.iv2, nested.ols, nested.iv1, nested.iv2),
                    custom.model.names=c("OLS","IV1","IV2","OLS","IV1","IV2"),
                    custom.coef.names=c("Intercept","AV", "HMO", "Premium",
                                        "Blue Shield", "Health Net", "Kaiser", "Small insurer", 
                                        "1-λ"),
                    override.coef=list(coef(logit.ols),coef(logit.iv1),coef(logit.iv2),
                                       coef(nested.ols),coef(nested.iv1),coef(nested.iv2)),
                    override.se=list(sqrt(diag(vcov(logit.ols))),sqrt(diag(vcov(logit.iv1))),sqrt(diag(vcov(logit.iv2))),
                                     sqrt(diag(vcov(nested.ols))),sqrt(diag(vcov(nested.iv1))),sqrt(diag(vcov(nested.iv2)))), 
                    override.pval=list(pvalues_logit_ols,pvalues_logit_iv1,pvalues_logit_iv2,
                                       pvalues_nested_ols,pvalues_nested_iv1,pvalues_nested_iv2),
                    digits=3,caption="Berry Inversion Estimates",
                    include.rsquared = FALSE, include.adjrs = FALSE, include.nobs = TRUE, 
                    include.fstatistic = FALSE, inline.css = FALSE, include.rmse = FALSE,
                    caption.above = TRUE)


writeLines(latex_tab, "PS 1/output/berry_table.tex")


## 2c
# Logit estimation

logit.ols2 <- lm(mean_util_hh ~ av + hmo + avg_price + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 + perc_45to54 + perc_55to64  + 
                   perc_0to17:avg_price + perc_18to25:avg_price + perc_26to34:avg_price + perc_35to44:avg_price + perc_45to54:avg_price + 
                   perc_55to64:avg_price, data = market.df)

# Nested logit

nested.ols2 <- lm(mean_util_hh ~ av + hmo + avg_price + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 + perc_45to54 + perc_55to64  + 
                    perc_0to17:avg_price + perc_18to25:avg_price + perc_26to34:avg_price + perc_35to44:avg_price + perc_45to54:avg_price + 
                    perc_55to64:avg_price + log(nest_share_hh), data = market.df)


# Results table

pvalues_logit_ols2 <- 2 * pnorm(abs(coef(logit.ols2)/sqrt(diag(vcov(logit.ols2)))), lower.tail=FALSE)
pvalues_nested_ols2 <- 2 * pnorm(abs(coef(nested.ols2)/sqrt(diag(vcov(nested.ols2)))), lower.tail=FALSE)


latex_tab <- texreg(list(logit.ols2, nested.ols2),
                    custom.model.names=c("Logit","Nested Logit"),
                    custom.coef.names=c("Intercept","AV", "HMO", "Premium","Percent 0 to 17","Percent 18 to 25","Percent 26 to 34",
                                        "Percent 35 to 44","Percent 45 to 54","Percent 55 to 64","Premium \times Percent 0 to 17",
                                        "Premium \times Percent 18 to 25","Premium \times Percent 26 to 34",
                                        "Premium \times Percent 35 to 44","Premium \times Percent 45 to 54","Percent 55 to 64", 
                                        "1-λ"),
                    override.coef=list(coef(logit.ols2),coef(nested.ols2)),
                    override.se=list(sqrt(diag(vcov(logit.ols2))),sqrt(diag(vcov(nested.ols2)))), 
                    override.pval=list(pvalues_logit_ols2,pvalues_nested_ols2),
                    digits=3,caption="Logit and Nested Logit Estimates with Age",
                    include.rsquared = FALSE, include.adjrs = FALSE, include.nobs = TRUE, 
                    include.fstatistic = FALSE, inline.css = FALSE, include.rmse = FALSE,
                    caption.above = TRUE)


writeLines(latex_tab, "PS 1/output/age_table.tex")