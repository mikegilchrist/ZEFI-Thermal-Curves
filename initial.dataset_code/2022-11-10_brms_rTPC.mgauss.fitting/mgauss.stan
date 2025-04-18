// generated with brms 2.18.0
functions {
}
data {
  int<lower=1> N;  // total number of observations
  int Y[N];  // response variable
  int<lower=1> K_rmax;  // number of population-level effects
  matrix[N, K_rmax] X_rmax;  // population-level design matrix
  int<lower=1> K_topt;  // number of population-level effects
  matrix[N, K_topt] X_topt;  // population-level design matrix
  int<lower=1> K_a;  // number of population-level effects
  matrix[N, K_a] X_a;  // population-level design matrix
  int<lower=1> K_b;  // number of population-level effects
  matrix[N, K_b] X_b;  // population-level design matrix
  // covariate vectors for non-linear functions
  vector[N] C_1;
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_topt_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector<lower=0>[K_rmax] b_rmax;  // population-level effects
  vector<lower=0>[K_topt] b_topt;  // population-level effects
  vector<lower=0>[K_a] b_a;  // population-level effects
  vector<lower=0>[K_b] b_b;  // population-level effects
  real<lower=0> shape;  // shape parameter
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_topt_1;  // actual group-level effects
  real lprior = 0;  // prior contributions to the log posterior
  r_1_topt_1 = (sd_1[1] * (z_1[1]));
  lprior += gamma_lpdf(b_rmax | 1, 0.005);
  lprior += normal_lpdf(b_topt | 35, 10)
    - 1 * normal_lccdf(0 | 35, 10);
  lprior += gamma_lpdf(b_a | 1, 0.01);
  lprior += normal_lpdf(b_b | 0, 5)
    - 1 * normal_lccdf(0 | 0, 5);
  lprior += gamma_lpdf(shape | 0.01, 0.01);
  lprior += student_t_lpdf(sd_1 | 3, 0, 44.5)
    - 1 * student_t_lccdf(0 | 3, 0, 44.5);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] nlp_rmax = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] nlp_topt = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] nlp_a = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] nlp_b = rep_vector(0.0, N);
    // initialize non-linear predictor term
    vector[N] mu;
    nlp_rmax += X_rmax * b_rmax;
    nlp_topt += X_topt * b_topt;
    nlp_a += X_a * b_a;
    nlp_b += X_b * b_b;
    for (n in 1:N) {
      // add more terms to the linear predictor
      nlp_topt[n] += r_1_topt_1[J_1[n]] * Z_1_topt_1[n];
    }
    for (n in 1:N) {
      // compute non-linear predictor values
      mu[n] = nlp_rmax[n] * exp( - 0.5 * (fabs(C_1[n] - nlp_topt[n]) / nlp_a[n]) ^ nlp_b[n]);
    }
    target += neg_binomial_2_lpmf(Y | mu, shape);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(z_1[1]);
}
generated quantities {
}
