// generated with brms 2.18.0
functions {
}
data {
  int<lower=1> N;  // total number of observations
  int Y[N];  // response variable
  int<lower=1> K_b0;  // number of population-level effects
  matrix[N, K_b0] X_b0;  // population-level design matrix
  int<lower=1> K_b1;  // number of population-level effects
  matrix[N, K_b1] X_b1;  // population-level design matrix
  int<lower=1> K_b2;  // number of population-level effects
  matrix[N, K_b2] X_b2;  // population-level design matrix
  // covariate vectors for non-linear functions
  int C_1[N];
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector<lower=30,upper=200>[K_b0] b_b0;  // population-level effects
  vector<lower=1,upper=19>[K_b1] b_b1;  // population-level effects
  vector<lower=5,upper=50>[K_b2] b_b2;  // population-level effects
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += uniform_lpdf(b_b0 | 30, 200)
    - 1 * log_diff_exp(uniform_lcdf(200 | 30, 200), uniform_lcdf(30 | 30, 200));
  lprior += uniform_lpdf(b_b1 | 1, 19)
    - 1 * log_diff_exp(uniform_lcdf(19 | 1, 19), uniform_lcdf(1 | 1, 19));
  lprior += uniform_lpdf(b_b2 | 1, 100)
    - 1 * log_diff_exp(uniform_lcdf(50 | 1, 100), uniform_lcdf(5 | 1, 100));
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] nlp_b0 = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] nlp_b1 = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] nlp_b2 = rep_vector(0.0, N);
    // initialize non-linear predictor term
    vector[N] mu;
    nlp_b0 += X_b0 * b_b0;
    nlp_b1 += X_b1 * b_b1;
    nlp_b2 += X_b2 * b_b2;
    for (n in 1:N) {
      // compute non-linear predictor values
      mu[n] = fmax(0.01 , nlp_b0[n] * (1 - step(C_1[n] - nlp_b1[n]) * nlp_b2[n]));
    }
    target += poisson_lpmf(Y | mu);
  }
  // priors including constants
  target += lprior;
}
generated quantities {
}
