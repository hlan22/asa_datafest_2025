setwd(here::here())
library(gridExtra)
library(grid)
df <- read.csv(here::here("additional_data", "clean", "final_data.csv"))
all_fits <- readRDS(here::here("additional_data", "clean", "all_models.RData"))

markets <- unique((df$market))

vals <- matrix(0, nrow = 4000, ncol = 9)
lambda_avgs <- matrix(0, nrow = 5, ncol = 9)
lambda_low  <-  matrix(0, nrow = 5, ncol = 9)
lambda_high <- matrix(0, nrow = 5, ncol = 9)
P <- 5

for (j in 1:length(markets)) {
   fit_m <- all_fits[[j]]
   lambda_bar <- apply(fit_m$lambda, MARGIN = 2, function(lambda) {
     lambda / fit_m$tau
   })
   lambda_avgs[ , j] <- apply(lambda_bar, MARGIN = 2, median) 
   lambda_high[ , j] <- apply(lambda_bar, MARGIN = 2, function(x){quantile(x, 0.75)}) 
   lambda_low[ , j] <- apply(lambda_bar, MARGIN = 2, function(x){quantile(x, 0.25)}) 
}

# --- BETA ---- #
beta_avgs <- matrix(0, nrow = 5, ncol = 9)
for (j in 1:length(markets)) {
  fit_m <- all_fits[[j]]
  beta_avgs[ , j] <- apply(fit_m$beta_raw, MARGIN = 2, median)
}

# --- PLOT --- #
raw_strings<-unique(paste0(df$market, df$state_politics))

df_bar <- data.frame(
  market = markets,
  lambda_bar = lambda_avgs[P, ],
  lower      = lambda_low[P, ],
  upper      = lambda_high[P, ],
  party = ifelse(grepl("republican", raw_strings), "Republican", "Democrat")
)

med_shrinkage <- ggplot(df_bar, aes(x = reorder(market, lambda_bar), fill = party, y = lambda_bar)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Democrat" = "dodgerblue", "Republican" = "red")) +
  labs(
    title = "Posterior Median Shrinkage for Political Affiliation Predictor, Standardized",
    subtitle = expression("Higher " ~ bar(lambda) ~ " indicates less shrinkage (more relevance)"),
    x = "Major Market",
    y = expression(bar(lambda)),
    fill = "Political Affiliation"
  ) +
  theme_bw() + theme(
    legend.position = c(0.95, 0.05), 
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

med_shrinkage

ggsave(filename = "outputs/median_shrinkage.png",
       plot = med_shrinkage,
       width = 8,
       height = 6
       )

# ------------ One Location ----------- #
chicago_post <- all_fits$Chicago
var_order <- c(
  "Extreme Weather Events",
  "Unemployment Rate",
  "Traffic Congestion",
  "Standardized Covid Cases",
  "State Political Affiliation"
) -> colnames(chicago_post$beta_raw) 
coefs <- chicago_post$beta_raw
coefs_df <- as.data.frame(coefs)
coefs_df$iteration <- 1:nrow(coefs_df)

coefs_long <- coefs_df %>%
  pivot_longer(
    cols = -iteration,
    names_to = "predictor",
    values_to = "value"
  )
coefs_long$predictor <- factor(coefs_long$predictor, levels = var_order)
chicago_plot <- ggplot(coefs_long, aes(x = value, y = reorder(predictor, value), fill = predictor)) +
  stat_halfeye(
    .width = c(0.5, 0.95),
    point_interval = median_qi, 
    slab_color = "black",
    alpha = 0.6
  ) +
  scale_fill_viridis_d() +
  labs(
    title = "Posterior Distributions of Coefficient Estimates",
    subtitle = "Chicago Market Model with Median and 95% Credible Intervals",
    x = "Posterior Coefficient value",
    y = "Predictor"
  ) +
  theme_bw() + theme(legend.position = "none")

ggsave(filename = "outputs/posterior_chicago.png",
       plot = chicago_plot,
       width = 8,
       height = 6
)

# ---------- ALL plots ---------- #
plot_list <- list()

for (mkt in markets) {
  fit <- all_fits[[mkt]]
  colnames(fit$beta_raw) <- var_order  # apply variable names
  
  coefs <- as.data.frame(fit$beta_raw)
  coefs$iteration <- 1:nrow(coefs)
  
  coefs_long <- coefs %>%
    pivot_longer(-iteration, names_to = "predictor", values_to = "value")
  
  coefs_long$predictor <- factor(coefs_long$predictor, levels = var_order)
  
  p <- ggplot(coefs_long, aes(x = value, y = predictor, fill = predictor)) +
    stat_halfeye(
      .width = c(0.5, 0.95),
      point_interval = median_qi, 
      slab_color = "black",
      alpha = 0.6
    ) +
    scale_fill_viridis_d(option = "D") +
    labs(
      title = mkt,
      subtitle = "Posterior Coefficient Distributions",
      x = NULL,
      y = NULL
    ) +
    theme_bw(base_size = 10) +
    theme(legend.position = "none")
  
  plot_list[[mkt]] <- p
}
for (i in seq_along(plot_list)) {
  if (!i %in% c(1, 4, 7)) {
    plot_list[[i]] <- plot_list[[i]] +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  }
}
wrapped_plots <- lapply(plot_list, function(p) {
  arrangeGrob(p, heights = unit(1, "null"), widths = unit(1, "null"))
})
grid_arranged <- do.call(grid.arrange, c(wrapped_plots, ncol = 3))
png("outputs/posterior_grid.png", width = 1400, height = 1000, res = 150)
grid.draw(grid_arranged)
dev.off()
