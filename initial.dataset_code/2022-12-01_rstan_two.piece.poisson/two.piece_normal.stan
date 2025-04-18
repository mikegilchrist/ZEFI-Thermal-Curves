data {

  int<lower=0> N;  // # of data points
  vector[N] y; //song_count
  vector[N] x; //temp
  //int<lower=0> M; // # of males;
  //int<lower=1> L; //# of line segments
  real tmax; // max temp
  real y_tmax; // y value at tmax, generally will be 0  
}

transformed data {
  vector[N] xt  = tmax - x;
}
    
parameters {
  //  vector[L] b_vals;
  real<upper=0> b1;
  real<lower=0,upper=tmax> x1;
  real<lower=0> sigma;
}

transformed parameters {

  real xt1 = tmax - x1;
  real bt1 = - b1;
}

model {
  y ~ normal(y_tmax + fmin(xt, xt1) * bt1, sigma);
  sigma ~ normal(3, 5);
}
