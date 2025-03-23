
// // // // // // // // // // // // // // // // // // //
// // Multivariate Hierarchical Normal Regression // //
// // // // // With Variable Selection   // // // // //
// // // // // // // // // // // // // // // // // //


// The input data 
data {
  int<lower=0> N; 
  int<lower=0> P;                 
  vector[N] m;                    // Occupancy rate
  int<lower=0>[N] x1;             // Extreme weather events
  vector<lower=0, upper=1>[N] x2; // Unemployment rate    
  vector<lower=0>[N] x3;          // Traffic congestion
  int<lower=0>[N] x4;             // Covid cases
  int<lower=0, upper=1> [N] x5;   // State political affiliation
  vector[N] x6;                   // work from home rate
}

// The parameters accepted by the model
parameters {
  real<lower=0> mu;
  real<lower=0> sigma;
  real vector[7] beta; // coefs
}

// likelihood
model {
  sigma ~ exponential(1);            // fairly sparse priors
  beta ~ normal(0, 1);               

  for (t in 1:N) {
    real mu;
    mu = beta[1] +    // intercept
         beta[2] * x1[t] +
         beta[3] * x2[t] +
         beta[4] * x3[t] +
         beta[5] * x4[t] +
         beta[6] * x5[t] +
         beta[7] * x6[t];
    m[t] ~ normal(mu, sigma); 
  }
}

