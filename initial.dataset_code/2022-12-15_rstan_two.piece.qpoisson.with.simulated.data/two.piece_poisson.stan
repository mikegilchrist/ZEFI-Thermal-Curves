// initial and working model formulation using t0 and b0 rather than t0 and y0.
// This runs well.
// y0 is calculated as a transformed parameter

data {
  int<lower=0> N;  // # of data points
  int<lower=0> y[N]; //song_count
  vector[N] t; //temp
  //int<lower=0> M; // # of males;
  //int<lower=1> L; //# of line segments
  real tmax; // max temp
  real t0min; //min threshold value
  real t0max; //max threshold value
  real y_tmax; // y value at tmax, generally will be 0
  vector[100] tp; // points used to create predictions for plotting

  
}

transformed data {
  vector[N] s  = tmax - t;
  vector[100] sp = tmax - tp;
}

parameters {
  //  vector[L] b_vals;
  real<upper=0> b0; // slope of decrease
  real<lower=t0min,upper=t0max> t0; //first threshold,
  // need upper bound to avoid chain getting caught at t0 = tmax
}

transformed parameters {
  real s0 = tmax - t0; // first threshold
  real a0 = - b0; // slope of increase
  real y0 = s0*a0; // pre-threshold level
}


model {
  // I see examples where `target` is used as an (undefined) objective function.
  // This line of code runs, but fails to converge 
  // target += poisson_lupmf(y| y_tmax + fmin(tt, a0) * bt0);

  vector[N] lambda = y_tmax + fmin(s, s0) * a0;

  // This runs and converges.
    y ~ poisson(lambda);
}

//generated quantities {
//
//  // Get LLik values for each data point
//  // These are used in the LOO analysis of model fit(s)
//  vector[N] log_lik;
//  vector[100] yp; //predicted values based on xp
//  
//  //yp = y_tmax + fmin(sp, s0) * a0;
//  for(n in 1:100) {
//      yp[n] = y_tmax + fmin(sp[n], s0) * a0;
//  }
//  for (n in 1:N) {
//    log_lik[n] = poisson_lpmf(y[n] | lambda[n]);
//  }
//}

