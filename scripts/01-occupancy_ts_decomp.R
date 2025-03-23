# separate time series components of occupancy data
library(ggplot2)
library(tidyverse)
library(forecast)
library(broom)

occ <- read.csv("data/Major Market Occupancy Data.csv")

# The occupancy rates are reported in percents, 
# and the percents are based on a baseline value of
# occupancy on March 1, 2020.

# -------------------- #
occ <- occ %>%
  mutate(yearquarter = paste0(year, quarter)) %>%
  arrange(yearquarter) 
# ------------------ #

# ------------------ #
# ----- LOESS ------ #
# ------------------ #

# initialize an empty list to store decomposed data
decomposed_list <- list()
for (mkt in unique(occ$market)) {
  
  # get current market
  sub_df <- occ[occ$market == mkt, ]
  
  # create time series object
  ts_data <- ts(sub_df$occupancy_proportion, 
                start = c(2020, 1), 
                frequency = 4)
  
  # decompose using loess
  decomp <- stl(ts_data, s.window = "periodic")
  
  # extract components
  sub_df$mt <- as.numeric(decomp$time.series[, "trend"])
  sub_df$st <- as.numeric(decomp$time.series[, "seasonal"])
  sub_df$remainder <- as.numeric(decomp$time.series[, "remainder"])
  
  # append to list
  decomposed_list[[mkt]] <- sub_df
}
occ_decomposed <- do.call(rbind, decomposed_list)
rownames(occ_decomposed) <- NULL

# --------------------- #
# ---- Simple ARMA ---- #
# --------------------- #
arma_models <- list()
for (mkt in unique(occ$market)) {
  # subset remainder for market
  sub_df <- occ_decomposed[occ_decomposed$market == mkt, ]
  rem_ts <- ts(sub_df$remainder, start = c(2020, 1), frequency = 4)
  
  # fit ARMA model by stepwise selection
  model <- auto.arima(rem_ts, d = 0, seasonal = FALSE, stepwise = TRUE, approximation = FALSE)
  # extract model info
  tidy_summary <- tidy(model)
  tidy_summary$market <- mkt
  
  arma_models[[mkt]] <- tidy_summary
}

# -------------- #
# ---- Fits ---- #
# -------------- #
occ_decomposed <- occ_decomposed %>%
  mutate(preds = mt + st) %>%
  group_by(market) %>%
  arrange(yearquarter) %>%
  mutate(
    yt         = mt + st,
    residual      = occupancy_proportion - yt,
    z1  = lag(residual),
    z2  = lag(residual, 2)
  ) %>%
  ungroup()

# then, estimate the MA component for each market
occ_decomposed$ma_adj <- NA
occ_decomposed$ma_low <- NA
occ_decomposed$ma_high <- NA
for(mkt in unique(occ_decomposed$market)){
  # extract lagged errors
  z1_i <- occ_decomposed$z1[occ_decomposed$market == mkt]
  z2_i <- occ_decomposed$z2[occ_decomposed$market == mkt]
  # check dimension of estimated moving average process
  if(dim(arma_models[[mkt]])[1] == 0){
    # no adjustment needed
    adj <- 0
    low <- 0
    high <- 0
  } else{
    # get MA parameters
    theta1i <- arma_models[[mkt]]$estimate[1]
    theta2i <- arma_models[[mkt]]$estimate[2]
    # extract standard errors
    se1 <- arma_models[[mkt]]$std.error[1]
    se2 <- arma_models[[mkt]]$std.error[2]
    # 95% interval
    Z <- qnorm(0.995)
    # make adjustment
    adj <- theta1i*z1_i + theta2i*z2_i
    low <- (theta1i - Z * se1) * z1_i + (theta2i - Z * se2) * z2_i
    high <- (theta1i + Z * se1) * z1_i + (theta2i + Z * se2) * z2_i
  }
  shrink <- 0.4 # regularize MA due to time series size
  occ_decomposed$ma_adj[occ_decomposed$market == mkt] <- shrink*adj
  occ_decomposed$ma_low[occ_decomposed$market == mkt] <- (shrink)*low
  occ_decomposed$ma_high[occ_decomposed$market == mkt] <- (shrink)*high
}
# MA adj , final version 
occ_f <- occ_decomposed %>%
  mutate(
    ma_adj = ifelse(is.na(ma_adj), 0, ma_adj),
    ma_low = ifelse(is.na(ma_low), 0, ma_low),
    ma_high = ifelse(is.na(ma_high), 0, ma_high),
    ma_pred = preds + ma_adj,
    ma_pred_low = preds + pmin(ma_low, ma_high),
    ma_pred_high = preds + pmax(ma_low, ma_high)
  )
# --- plot --- #
houston_subdf <- occ_f[occ_f$market == "Houston", ]

houston_subdf <- occ_f %>% filter(market == "Houston")

ggplot(houston_subdf, aes(x = seq_along(ma_pred))) +
  geom_ribbon(aes(ymin = ma_pred_low, ymax = ma_pred_high), fill = "lightblue", alpha = 0.9) +
  geom_line(aes(y = occupancy_proportion, color = "Actual"), size = 1) +
  geom_line(aes(y = ma_pred, color = "Predicted"), size = 1) +
  # Labels & styling
  labs(
    title = "Houston: Occupancy",
    subtitle = "With 95% Prediction Interval",
    x = "Year+Quarter", 
    y = "Occupancy Proportion",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "blue")) +
  theme_bw()
# --------------------- #
# ---- save object ---- #
# --------------------- #
out <- occ_decomposed[, c("year", "quarter", "yearquarter", "market", "occupancy_proportion", "mt", "st", "remainder", "z1", "z2")]
out$theta1 <- 0
out$theta2 <- 0
out$se_theta1 <- 0
out$se_theta2 <- 0
for(market in names(arma_models)){
  if(dim(arma_models[[market]])[1] > 0){
    arma_i <- arma_models[[market]]
    out$theta1[out$market == market] =  arma_i$estimate[1]
    out$theta2[out$market == market] =  arma_i$estimate[2]
    out$se_theta1[out$market == market] =  arma_i$std.error[1]
    out$se_theta2[out$market == market] =  arma_i$std.error[2]
  }
}
# save time series data
# write.csv(out, "occupancy_plus_decomp.csv", row.names = FALSE)

out <- read_csv("additional_data/clean/occupancy_plus_decomp.csv")

# ------------- #
# --- Plots --- #
# ------------- #

# plot pre-decomposition (before removing seasonality)
seasonal_occupancy <- ggplot(out,
       aes(
         x = yearquarter,
         y = occupancy_proportion,
         color = market,
         group = market
       )) +
  geom_line(size = 1) +
  labs(
    title = "Occupancy Proportion Time Series by Market",
    # subtitle = "Prior to Decomposition",
    x = "Time (Year-Quarter)",
    y = "Occupancy Proportion",
    color = "Market") + 
  scale_color_brewer(palette = "Paired") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

seasonal_occupancy

# save plot
ggsave(filename = "outputs/seasonal_occupancy.png",
       plot = seasonal_occupancy,
       width = 7,
       height = 6)




deseasonalized_occupancy <- ggplot(out,
       aes(
         x = yearquarter,
         y = mt,
         color = market,
         group = market
       )) +
  geom_line(size = 1) +
  labs(
    title = "Occupancy Proportion Trend Component by Market",
    # subtitle = "After LOESS Decomposition",
    x = "Time (Year-Quarter)",
    y = "Occupancy Proportion Trend",
    color = "Market") + 
  scale_color_brewer(palette = "Paired") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

deseasonalized_occupancy
# save plot
ggsave(filename = "outputs/deseasonalized_occupancy.png",
       plot = deseasonalized_occupancy,
       width = 7,
       height = 6)


# # add averages
# occ_average <- occ_decomposed %>%
#   group_by(yearquarter) %>%
#   summarize(
#     occupancy_proportion = mean(occupancy_proportion, na.rm = TRUE),
#     mt = mean(mt, na.rm = TRUE),
#     st = mean(st, na.rm = TRUE),
#     remainder = mean(remainder, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   mutate(market = "average")