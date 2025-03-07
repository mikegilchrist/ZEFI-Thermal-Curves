## Negative Binomial distribution parameterized by mean (mu) and overdispersion parameter (theta).
## This parameterization is referred to as NEGBIN type I (Cameron and Trivedi, 1998) as cited by
## https://doi.org/10.1080/03610926.2018.1563164
##
## x ~ nbinom_type1(mu, theta), where E(x) = mu, Var(x) = (theta + 1) mu
## This should not be confused with the mu and shape parameterization of nbinom in R or the 'alternative' NB (neg_binomial_2_...) in stan
## Note using disp instead of theta because using theta is a reserved term in `brms`

library(brms) 

run_examples <- FALSE

# helper functions for post-processing of the family
# cribbed from lognormal_natural.R
log_lik_nbinom_type1 <- function(i, prep) {
  mu <- prep$dpars$mu[, i]
  if(NCOL(prep$dpars$disp)==1) {
      disp <- prep$dpars$disp
  } else { disp <- prep$dpars$disp[, i] }   ## [, i] if disp is modelled, without otherwise
  y <- prep$data$Y[i]
  llik <- function(y, mu, disp) { dnbinom(x = y, mu = mu, size = mu/disp, log = TRUE) + log(mu) - 2 * log(disp)}
  llik(y, mu, disp)
  Vectorize(llik(y, mu, disp) )
  
}


posterior_predict_nbinom_type1 <- function(i, prep, ...) {
  mu <- prep$dpars$mu[, i]
  if(NCOL(prep$dpars$disp)==1){ disp <- prep$dpars$disp
  } else { disp <- prep$dpars$disp[, i] }   ## [, i] if disp is modelled, without otherwise
  size = mu/disp
  rnbinom(n = prep$ndraws, mu = mu, size = size)
}

posterior_epred_nbinom_type1 <- function(prep) {
  mu <- prep$dpars$mu
  return(mu)
}

# define custom family
nbinom_type1 <- function(link_mu = "identity", link_disp = "identity")
    custom_family(
 name = "nbinom_type1", 
 dpars = c("mu", "disp"), #expectd value and variance scale (> 0)
 links = c(link_mu, link_disp), 
 lb = c(0, 0),
 ub = c(NA, NA),
 type = "int", #category of response variable
 log_lik = log_lik_nbinom_type1,
 posterior_predict = posterior_predict_nbinom_type1,
 posterior_epred = posterior_epred_nbinom_type1
)

# additionally required Stan code
# Not clear if the arguments should be ints and reals (as in the stan documentation) instead of int and real
stan_nbinom_type1 <- "
  real nbinom_type1_lpmf(int y, real mu, real disp) {
    return neg_binomial_2_lpmf(y | mu, mu/(disp)) + log(mu) - 2 * log(disp);
  }
  int nbinom_type1_rng(real mu, real disp) {
    return neg_binomial_2_rng(mu, mu/(disp));
  }
"

# example

if(run_examples) {
  
  library(dplyr)
  library(tidyr)

  ## ensure ncores =1 if there's <=2 or 4 if there's > 6
  ncores = min(max(parallel::detectCores()-2, 1), 4)
  nchains = ncores
  options(mc.cores = ncores)
  ## create data

  ### define variables
  group <- c(1:2)
  x <- 0:50
  theta <- c(2,5)

  ## Create data
  data <- crossing(group, rep = 1:4, x = x) %>%
    mutate(mu = 10*(1 + x), theta = theta[group]) %>%
    rowwise() %>%
    mutate(y = rnbinom(n = 1, mu = mu, size = mu/theta),
           group = factor(group)) %>%
    select(-c(mu, theta))





  ## fit models
  stanvar <- stanvar(scode = stan_nbinom_type1, block = "functions")

  centered_intercept = FALSE
  
  if(centered_intercept) {
    formula <- formula(y ~ 1 + x)
  } else {
    formula <- formula(y ~ 0 + Intercept + x)
    }
  ## Std regression model
  get_prior(formula = formula, 
            family = nbinom_type1(),
            data = data,
            stanvar = stanvar
            )

  bprior1 <- prior(gamma(0.01, 0.01), class = "b", lb = 0) +
    prior(gamma(0.01, 0.01), class = "disp", lb = 0) +
    ## class intercept only works if you use `1 + ...`
    ## You cannot use it with `0 + Intercept + ...`
    if(centered_intercept) {
      prior(gamma(0.01, 0.01), class = "Intercept", lb = 0)
    } else {
      NULL #prior(gamma(0.01, 0.01), class = "b", coef = "Intercept", lb = 0)
    }

  start_time <- Sys.time()
  
  brm(formula = formula,
      family = nbinom_type1(),
      data = data,
      stanvar = stanvar,
      prior = bprior1,
      control = list(adapt_delta = 0.95,
                     max_treedepth = 12),
      chains = nchains,
      cores = ncores,
      iter = 1000
      ) -> model_test1


  end_time <- Sys.time()
  total-time <- end_time - start_time
  print("Actual elapsed time", total-time)
  
  ## Distributional regression model

  get_prior(bf(y ~ 0 + Intercept + x,
               disp ~ 0 + group),
            family = nbinom_type1(),
            data = data,
            stanvar = stanvar
            )

  bprior2 <- prior(gamma(0.01, 0.01), class = "b", lb = 0) +
    prior(gamma(0.01, 0.01), dpar = "disp", lb = 0)

  brm(bf(y ~ 0 + Intercept + x,
         disp ~ 0 + group),
      family = nbinom_type1(),
      data = data,
      stanvar = stanvar,
      prior = bprior2,
      control = list(adapt_delta = 0.95,
                     max_treedepth = 14),
      chains = nchains,
      cores = ncores,
      ) -> model_test2

  summary(model_test1)
  summary(model_test2)

  model_test1 |> conditional_effects() |> plot(points = TRUE)
  model_test2 |> conditional_effects() |> plot(points = TRUE)

  model_test1 |> conditional_effects(method = "posterior_predict")
  model_test2 |> conditional_effects(method = "posterior_predict")

  loo(model_test1, model_test2)
}
