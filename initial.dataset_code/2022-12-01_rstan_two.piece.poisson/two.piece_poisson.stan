// initial and working model formulation using t1 and b0 rather than t1 and y0.
// This runs well.
// y0 is calculated as a transformed parameter

data {
  int<lower=0> N;  // # of data points
  int<lower=0> y[N]; //song_count
  vector[N] t; //temp
  //int<lower=0> M; // # of males;
  //int<lower=1> L; //# of line segments
  real tmax; // max temp
  real t1min; //min threshold value
  real t1max; //max threshold value
  real y_tmax; // y value at tmax, generally will be 0
  vector[100] tp; // points used to create predictions for plotting

  
}

transformed data {
  vector[N] s  = tmax - t;
  vector[100] sp = tmax - tp;
}

parameters {
  //  vector[L] b_vals;
  real<upper=0> b1; // slope of decrease
  real<lower=t1min,upper=t1max> t1; //first threshold,
  // need upper bound to avoid chain getting caught at t1 = tmax
}

transformed parameters {
  real s1 = tmax - t1; // first threshold
  real a1 = - b1; // slope of increase
  real y0 = s1*a1; // pre-threshold level
  vector[N] lambda = y_tmax + fmin(s, s1) * a1;
}


model {
  // I see examples where `target` is used as an (undefined) objective function.
  // This line of code runs, but fails to converge 
  // target += poisson_lupmf(y| y_tmax + fmin(tt, a1) * bt1);

  // This runs and converges.
  // So long as 
  y ~ poisson(lambda);
}

generated quantities {

  // Get LLik values for each data point
  // These are used in the LOO analysis of model fit(s)
  vector[N] log_lik;
  vector[100] yp; //predicted values based on xp
  
  //yp = y_tmax + fmin(sp, s1) * a1;
  for(n in 1:100) {
      yp[n] = y_tmax + fmin(sp[n], s1) * a1;
  }
  for (n in 1:N) {
    log_lik[n] = poisson_lpmf(y[n] | lambda[n]);
  }
}
