library(tidybayes)
library(tidyverse)
library(rstan)
# load data 
folder <- here::here("additional_data", "clean")
df <- read.csv(paste0(folder, "/", "final_data.csv"))
# standardize covid cases to avoid unstable coefficients
df$covid_stdrd <- (df$covid_cases - mean(df$covid_cases))/sd(df$covid_cases)
# select for houston in test case

model <- stan_model(file = here::here("scripts", "model.stan"))
# some stable initial ranges
init_fun <- function() list(
  tau = runif(1, 0.05, 0.2),          
  lambda = runif(5, 0.5, 1.0),          
  beta_raw = rnorm(5, 0, 0.1),          
  beta0 = rnorm(1, 0, 0.5),             
  sigma = runif(1, 0.5, 2)             
)
# go through all fits for all markets
all_fits <- list()
for (mkt in unique(df$market)){
  # get this market
  m_df <- df[df$market == mkt, ]
  # fit the model for this market
  m_fit <- sampling(
    model,
    data = list(
      N  = nrow(m_df),
      m  = m_df$mt,
      x1 = m_df$extreme_weather_num,
      x2 = m_df$unemployment_rate,
      x3 = m_df$hours_traffic,
      x4 = m_df$covid_stdrd,
      x5 = as.numeric(m_df$state_politics == "republican")
    ),
    chains = 4,
    iter = 2000,    
    control = list(adapt_delta = 0.99),
    init = init_fun,
    seed = 1928  
  )
  results <- rstan::extract(m_fit)
  all_fits[[mkt]] <- results
}


saveRDS(all_fits, paste0(folder, "/", "all_models.RData") )
