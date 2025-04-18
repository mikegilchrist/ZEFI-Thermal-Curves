// alternative modle formulation using y0| t < t1, rather than t1 and b0
// This formulation runs into problems which haven't been debugged.
// Use one.step_piecewise_poisoon_t1.and.b0.stan instead

data {
  int<lower=0> N;  // # of data points
  int<lower=0> y[N]; //song_count
  vector[N] t; //temp
  //int<lower=0> M; // # of males;
  //int<lower=1> L; //# of line segments
  real tmax; // max temp
  real t1max; //max threshold value
  real y_tmax; // y value at tmax, generally will be 0  
}


parameters {
  //  vector[L] b_vals;
  real<lower=1> y0; // level prior to threshold
  real<lower=35,upper=t1max> t1; //first threshold,
  // need upper bound to avoid chain getting caught at t1 = tmax
}

transformed parameters {
  real<upper=-0.1> b = y0/(tmax - t1); // slope, should be < 0
 }


model {
  y ~ poisson(y0 + fmax(b * (t-t1), -y0) );
}
