
// ------------------------------------------------- //
// -- Multivariate Hierarchical Normal Regression -- //
// ------------ With Variable Selection ------------ //
// --------------- Via Horseshoe ------------------- //
// ------------------------------------------------- //

// The input data 
data {
  int<lower=0> N; 
  vector[N] m;                    // Occupancy rate
  int<lower=0> x1[N];           // Extreme weather events
  real<lower=0, upper=1> x2[N]; // Unemployment rate    
  real<lower=0> x3[N];          // Traffic congestion
  real x4[N];                   // Standardized Covid cases
  int<lower=0, upper=1> x5[N];   // State political affiliation
  // vector[N] x6;                   // work from home rate
}

// The parameters accepted by the model
parameters {
  real<lower=0> sigma;                  // likelihood noise
  real<lower=0, upper=1> tau;           // global shrinkage
  real beta0;
  vector<lower=0, upper=1>[5] lambda;   // local shrinkage per predictor
  vector[5] beta_raw;                   // standardized coefficients
}
transformed parameters {
  vector[5] beta = beta_raw .* lambda * tau; // implement Horseshoe in Stan
}
// likelihood
model {
  // fairly sparse priors
  sigma ~ exponential(1);           
  beta_raw ~ normal(0, 1);   
  tau ~ cauchy(0, 0.5);         // try Cauchy(0, 0.5)
  lambda ~ cauchy(0, 0.5);    
  beta0 ~ normal(0, 1);
  // compute likelihood
  for (t in 1:N) {
    real mu;
    mu = beta0  +    // intercept
         beta[1] * x1[t] +
         beta[2] * x2[t] +
         beta[3] * x3[t] +
         beta[4] * x4[t] +
         beta[5] * x5[t]; // +
    //     beta[6] * x6[t];
    m[t] ~ normal(mu, sigma); 
  }
}

