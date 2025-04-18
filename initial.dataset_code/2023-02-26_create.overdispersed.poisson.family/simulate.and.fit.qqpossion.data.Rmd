library(dplyr)
library(tidyr) ## provides expand/crossing
## Define variables

theta <- 3.5 # variance scale
t_vec <- 0:5
n_vec <- 1:5; # samples/t

## Create data
data <- crossing(t = t_vec, n_vec) %>%
    mutate(m = 10 + 10*t) %>%
    rowwise() %>%
    mutate(s = rnbinom(n = 1, mu = m, size = m/(3.5 - 1)))


source("overdispersed.poisson.family.R")

get_prior(formula = s ~ 1 + t,
          data = data,
          family = qqpoisson)

fit <- brm(s ~ 1 + t,
           family = qqpoisson(),
           data = data,
           stanvars = stanvars
)
