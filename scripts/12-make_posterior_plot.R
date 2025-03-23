setwd(here::here())
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
ggplot(df_bar, aes(x = reorder(market, lambda_bar), y = lambda_bar, fill = party)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste("Standardized Shrinkage for Predictor", P),
    x = "Market",
    y = expression(bar(lambda)),
    fill = "Political Affiliation"
  ) +
  theme_minimal()
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
