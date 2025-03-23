// Ethan - For understanding 
// model_hierarchical_regression.stan

data {
  int<lower=1> T;              // Number of observations (time steps)
  int<lower=1> M;              // Number of markets
  int<lower=1> K;              // Number of predictors in the design matrix X
  int<lower=1, upper=M> market[T]; // Market index for each observation
  
  // Additional predictors (each vector of length T)
  vector[T] covid;             // COVID counts
  vector[T] wfh;               // Percentage working from home
  vector[T] xtreme;            // Extreme weather counts
  vector[T] unemp;             // Unemployment rate
  vector[T] s;                 // Seasonal component (assumed known)
  vector[T] y;                 // Outcome: Market Occupancy
}

parameters {
  real alpha0; // Error term of additive decomposition
  vector[M] alpha_market; 
  vector[K] beta;  // Regression coefficients for predictors in X
  vector<lower=0>[K] lambda; 
  real<lower=0> tau;
  real gamma_covid;   // Coefficients for the additional predictors
  real gamma_wfh;
  real gamma_xtreme;
  real gamma_unemp;
  real<lower=0> sigma;   // Standard deviation of the residual error
  real mu_alpha;   // Hyperparameters for the hierarchical market intercepts
  real<lower=0> sigma_alpha;  // Hyperparameters for the hierarchical market intercepts
}

transformed parameters {
  vector[T] m;   // Trend component
  vector[T] mu;  // Overall model mean
  for (t in 1:T) {
    m[t] = gamma_covid   * covid[t] +
           gamma_wfh     * wfh[t] +
           gamma_xtreme  * xtreme[t] +
           gamma_unemp   * unemp[t]; // 
           
    mu[t] = s[t] + alpha0 + alpha_market[ market[t] ] + m[t];
  }
}

model {
  alpha0 ~ normal(0, 10);
  mu_alpha ~ normal(0, 10);
  sigma_alpha ~ cauchy(0, 5);
  alpha_market ~ normal(mu_alpha, sigma_alpha);
  lambda ~ cauchy(0, 1);
  tau ~ cauchy(0, 1);
  beta ~ normal(0, tau * lambda);
  
  // Priors for additional predictors' coefficients
  gamma_covid   ~ normal(0, 5);
  gamma_wfh     ~ normal(0, 5);
  gamma_xtreme  ~ normal(0, 5);
  gamma_unemp   ~ normal(0, 5);
  
  // Prior for the residual error
  sigma ~ cauchy(0, 5);
  y ~ normal(mu, sigma);
}



