// Goal: Fit qpoisson and models for two piecewise model 
// 2022-12-16:
// - Copied from two.piece_poisson.grouped.by.male.stan
// - Males can have own p value (M = L) which is fixed over all temp,
//   they can share them in an arbitrary # of groups (pooled M < L)
//   Grouping is a determined by the data vector mm

/* Math

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
//functions {
  // Returns expected counts | model parameters and variable x
  // I should be able to vectorize this
//  real f_mu(real b0, real x0, real xmax, real y_xmax, real x) {
//    real val1;
//    real val2;
//
//    val1 = xmax - x;
//    val2 = xmax - x0;
//    return (y_xmax  - fmin(val1, val2) * b0); 
//  }
  // Returns appropriate phi value for NB2 for Quasipoisson error given mu and theta
  //  real f_phi(real mu, real theta) {
  //  real phi = mu/(theta-1);
  //  return phi; 
  //}
//}
data {
  int<lower=1> N; // # of data points
  int<lower=1> L; // Levels = # of males;
  int<lower=1, upper=L> M; // Levels for NB parameter p
  array[N] int<lower=0> y; //song_count
  vector[N] x; // predictor: temp values at each observation
  // This, along with male, is the predictor
  array[N] int<lower=1, upper=L> ll; // male id, one for each observation
  array[N] int<lower=1, upper=M> mm; // qpoisson parameter grouping
  // max x value and response (y) at max value
  real<lower=1> y0_min; // min value of y0; should be based on any filtering threshold
  real xmax; // max temp
  real<lower=0> y_xmax; // y value at xmax, generally will be 0
  // Ensure model behaves well.
  real x0_min; //min threshold value
  real<lower=x0_min+1> x0_max; //max threshold value
  real<lower=1> sd_y0_prior;
  real<lower=0> alpha_theta_prior;
  //vector[100] xp; // points used to create predictions for plotting
}

//transformed data {
//  array[N] row_vector[D] s  = xmax - x;
//  //vector[100] sp = xmax - tp;
//}

parameters { // One set of parameters for each level (male) 
  vector<lower=y0_min>[L] y0; // mean value for y when x < x0
  vector<lower=x0_min,upper=x0_max>[L] x0; //first threshold
  vector<lower=1>[M] theta; //overdispersion parameter
}

transformed parameters {
  //  array[L] real<lower=x0_min,upper=x0_max> s0 = xmax - x0; // first threshold
  //array[L] real<lower=x0_min,upper=x0_max> a0 = - b0; // slope of decrease
  vector[L] b0 ;
  
  b0 = (y_xmax-y0)./(xmax - x0); // slope of decrease
}

model {
  // I see examples where `target` is used as an (undefined) objective function.
  // This line of code runs, but fails to converge 
  // target += poisson_lupmf(y| y_xmax + fmin(tt, a0) * b0);
  // vector formulation
  // vector[N] phi;
  // vector[N] mu;
  real phi;
  real mu;
  real val1;
  real val2;
  
  //Priors
    for (l in 1:L) {
      //x0[l] ~ uniform(30, 44);
      y0[l] ~ normal(150, sd_y0_prior);
   }
  for (m in 1:M) {
    theta[m] ~ exponential(alpha_theta_prior);
   }
  //Log Likelihood 
  for (n in 1:N) {
    val1 = xmax - x[n];
    val2 = xmax - x0[ll[n]];
    //mu = f_mu(b0[ll[n]], x0[ll[n]], xmax, y_xmax, x[n]);
    mu = y_xmax  - fmin(val1, val2) * b0[ll[n]];
    // phi = f_phi(mu, theta[mm[n]]);
    phi =  mu/(theta[mm[n]]-1);
    y[n] ~ neg_binomial_2(mu, phi);
  }
}


generated quantities {
  // Get LLik values for each data point
  // These are used in the LOO analysis of model fit(s)
//  vector[N] log_lik;
//  vector[num_elements(xp)] yp; //predicted values based on xp
//  real val1, val2;
//  
//  //yp = y_tmax + fmin(sp, s1) * a1;
//  for(n in 1:num_elements(yp)) {
//     val1 = xmax - xp[n];
//     val2 = xmax - x0[ll[n]];
//      yp[n] = y_xmax + fmin() * a1;
//  }
//  for (n in 1:N) {
//    log_lik[n] = poisson_lpmf(y[n] | lambda[n]);
//  }
}
