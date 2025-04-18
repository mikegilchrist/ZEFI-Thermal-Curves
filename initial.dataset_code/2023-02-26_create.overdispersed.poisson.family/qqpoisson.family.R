#library(tidyverse)
library(brms)

# define custom family
qqpoisson <- function(link_mu = "identity", link_theta = "identity")
    custom_family(
 name = "qqpoisson", 
 dpars = c("mu", "disp"), #expectd value and variance scale (>1)
 links = c(link_mu, link_theta), # only used if parameters are predicted
 lb = c(0, 1),
 ub = c(NA, NA),
 type = "int" #category of response variable
 ## vars: name not arbitrary,
 ##       used to pass additional data to model
 ##       if loop = TRUE, include `[n]` index
 ##vars = "vint1[n]",
 #loop = TRUE,
 #log_lik = NULL, # Not currently used
 #posterior_predict = NULL, # Not currently used
 #posterior_epred = NULL # Not currently used
)

## Define functions used in stan
## lpmf returns the likelihood
## rng returns a sample
## Using `theta` as a variable gives me problems when trying to fit model, so using `disp` instead
##
stan_funs <- "
  real qqpoisson_lpmf(int y, real mu, real theta) {
    return neg_binomial_2_lpmf(y | mu, mu/(theta));
  }
  int qqpoisson_rng(real mu, real theta) {
    return neg_binomial_2_rng(mu, mu/(theta));
  }
"
## export stan_funs to stanvars
stanvars <- stanvar(scode = stan_funs, block = "functions")

