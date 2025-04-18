// Goal: Fit negative binomial (and eventually the restricted version corresponding to the quasi-poisson (overdispersed poisson) models for two piecewise model 
// 2022-12-24:
// - Copied from two.piece_qpoisson_2.0.stan

/* Quasi-Poisson Math

\lambda ~ Gamma(\kappa = r, \theta = p/1-p) = Gamma(\alpha = r, \beta = (1-p)/p)
E(\lambda) = \kappa \theta = \alpha/\beta = r p/(1-p)
Var(\lambda) = E(\lambda) \theta = E(\lambda)/\beta = r (p/(1-p))^2

Y ~ Poisson(\lambda| \lambda ~ Gamma())
  ~ NB(r, p) = NB(\alpha = \kappa = r, \beta = (1-p)/p = = 1/\theta)

E(Y) = \alpha/beta = r p/(1-p) = E(\lambda)
Var(Y) = \alpha/\beta^2 (\beta +1) = Var(\lambda) (\beta + 1) = Var(\lambda)/p
       = r (p/(1-p))^2/p =  r p/(1-p)^2
If we want Var(Y) = \theta E(Y) with \theta > 1, we set p = 1/\theta and keep it constant over range of r values


An alternative formulation (NB2 in stan) is by mean and inverse, scaled dispersion factor.

Y ~ NB2(\mu, \phi)

E(Y) = \mu; Var(Y) = \mu + \mu^2/\phi

for Var(Y) = \theta E(Y) we set phi = \mu/(\theta - 1)
*/ 

data {
  // Model structure
  int<lower=1> N; // # of data points
  int<lower=1> M; // Levels for male
  int<lower=1> X; // Levels for x0
  int<lower=1> Y; // Levels for y0
  int<lower=1> NB; // Levels for NB parameter p/theta
  //int<lower=0, upper = 1> qpoisson_flag;//
  array[N] int<lower=1, upper=M> mm; // male ID of observation
  array[M] int<lower=1, upper=X> xx; // x0 grouping of male ID
  array[M] int<lower=1, upper=Y> yy; // y0 grouping of male ID
  array[M] int<lower=1, upper=NB> nbb; // overdispersion nb/qpoisson parameter grouping
  
  // Data
  array[N] int<lower=0> y; //song_motif
  vector[N] x; // predictor: temp values at each observation
  // This, along with male, is the predictor  // max x value and response (y) at max value
  //
  // Boundaries and priors
  real<lower=1> y0_min; // min value of y0; should be based on any filtering threshold
  real xmax; // max temp
  real<lower=0> y_xmax; // y value at xmax, generally will be 0
  // Ensure model behaves well.
  real x0_min; //min threshold value
  real<lower=x0_min+1> x0_max; //max threshold value
  real<lower=1> sd_y0_prior;
  real<lower=1> sd_sd_y_prior;
  real<lower=1> sd_sd_x_prior;
  real<lower=0> alpha_theta_prior;
  real<lower=0> alpha_phi_prior;
  //vector[100] xp; // points used to create predictions for plotting
}

parameters { // One set of parameters for each level (male) 
  vector<lower=y0_min>[Y] y0; // mean value for individual y1; group indicated by yy
  vector<lower=0>[M] y1;// individual 'normal' motif count when x < x0; individual indicated by mm
  vector<lower=0>[Y] sd_y0; // sd for y1 within each y0
  vector<lower=x0_min,upper=x0_max>[X] x0; //threshold mean; group indicated by xx
  vector<lower=0>[X] sd_x0; // sd for x1 within each x0
  vector<lower=0>[M] x1; // individual 'normal' threshold; individual indicated by mm
  vector<lower=1>[NB] phi; //overdispersion parameter; group indicated by nbb
                      // nb: This parameter can vary independently of mu
                      // qpoisson: This is constrained such that
                      //    $\phi = \mu/(\theta[i]-1)$
                      // where
                      //    $\theta$ is the qpoisson overdispersion parameter for
                      //    NB group i
}

transformed parameters {
// vector<lower=1>[if qpoisson_flag NB : 0] theta


}

model {
  // I see examples where `target` is used as an (undefined) objective function.
  // This line of code runs, but fails to converge 
  // target += poisson_lupmf(y| y_xmax + fmin(tt, a0) * b0);
  // vector formulation
  // vector[N] phi;
  // vector[N] mu;
  real b0;
  real mu;
  real val1;
  real val2;

  
  //Priors
  // Group level
    for (i in 1:Y) {
      //x0[l] ~ uniform(30, 44);
      y0[i] ~ normal(150, sd_y0_prior);
   }
  for (i in 1:NB) {
   //if(qpoisson_flag) {
   //   theta[i] ~ exponential(alpha_theta_prior);
   //   } else {
      phi[i] ~ exponential(alpha_phi_prior);
   //   }
  }
  // Individual Level
    for (m in 1:M) {
      x1[m] ~ normal(x0[xx[m]], sd_x0[xx[m]]);
      y0[m] ~ normal(y0[yy[m]], sd_y0[yy[m]]);
   }

  //Log Likelihood 
  for (n in 1:N) {
    m = mm[n]; // male ID
    val1 = xmax - x[n];
    val2 = xmax - x1[m]; // was xmax - x0[xx[n]];
    b0 = (y_xmax-y1[m])./(xmax - x1[m]); // was (y_xmax-y0[yy[n]])./(xmax - x0[xx[n]]);
    //mu = f_mu(b0[xx[n]], x0[xx[n]], xmax, y_xmax, x[n]);
    mu = y_xmax  - fmin(val1, val2) * b0;
    //phi =  mu/(theta[nbb[m]]-1);
    y[n] ~ neg_binomial_2(mu, phi[nbb[m]]);
  }
}


generated quantities {
  // Get LLik values for each data point
  // These are used in the LOO analysis of model fit(s)
  vector[N] log_lik;
  real b0;
  //real phi;
  real mu;
  real val1;
  real val2;
//  vector[num_elements(xp)] yp; //predicted values based on xp
//  real val1, val2;
//  
//  //yp = y_tmax + fmin(sp, s1) * a1;
//  for(n in 1:num_elements(yp)) {
//     val1 = xmax - xp[n];
//     val2 = xmax - x0[xx[n]];
//      yp[n] = y_xmax + fmin() * a1;
//  }
  for (n in 1:N) {
    val1 = xmax - x[n];
    val2 = xmax - x0[xx[n]];
    b0 = (y_xmax-y0[yy[n]])./(xmax - x0[xx[n]]);
    //mu = f_mu(b0[xx[n]], x0[xx[n]], xmax, y_xmax, x[n]);
    mu = y_xmax  - fmin(val1, val2) * b0;
    // phi = f_phi(mu, theta[nbb[n]]);
    //phi =  mu/(theta[nbb[n]]-1);
    log_lik[n] = neg_binomial_2_lpmf(y[n] | mu, phi[nbb[n]]);
  }
}

