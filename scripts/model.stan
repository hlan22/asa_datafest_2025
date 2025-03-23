
// // // // // // // // // // // // // // // // // // //
// // Multivariate Hierarchical Normal Regression // //
// // // // // With Variable Selection   // // // // //
// // // // // // // // // // // // // // // // // //


// The input data 
data {
  int<lower=0> N;
  vector[N] y;
}

// The parameters accepted by the model.
parameters {
  real mu;
  real<lower=0> sigma;
}

// The model to be estimated. 
model {
  y ~ normal(mu, sigma);
}

