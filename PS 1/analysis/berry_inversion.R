
################################################################################

## Berry Inversion -- 2a
## ECON 777 PS 1
## Author: Hannah Pitzer

################################################################################

library(ivreg)
library(modelsummary)


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
library(gt)

models <- list("OLS 1 " = logit.ols, "IV 1 " = logit.iv1, "IV 1 brand" = logit.iv2, 
               "OLS 2" = nested.ols, "IV 2" = nested.iv1, "IV 2 brand" = nested.iv2)

varnames <- c("AV", "HMO", "Premium",
              "Blue Shield", "Health Net", "Kaiser", "Small insurer", 
              "1-λ")

gof_map <- tribble(
  ~raw,      ~clean,          ~fmt,  ~omit,
  "nobs",      "Observations",     0,  FALSE
)

tab <- modelsummary(models,
                    stars = c('*' = .1, '**' = .05, '***' = 0.01),
                    coef_omit = "(Intercept)",
                    coef_rename = varnames,
                    statistic = NULL,
                    gof_map = gof_map,
                    output = "gt")

tab <- tab %>%
  tab_spanner(label = "Logit", columns = 2:4) %>%
  tab_spanner(label = "Nested logit", columns = 5:7)

latex_tab <- as_latex(tab)
writeLines(latex_tab, "PS 1/output/output_table.tex")