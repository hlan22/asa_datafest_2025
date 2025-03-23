library(dplyr)
folder <- here::here("additional_data", "raw")
#
df <- read.csv(paste0(folder, "/combined.csv"))
wfh_df <- read.csv(paste0(folder, "/work_from_home.csv"))
unep_df <- read.csv(paste0(folder, "/unemployment_major_market.csv"))
resp_df <- read.csv(here::here("additional_data", "clean", "occupancy_plus_decomp.csv"))
# quick fixes for response df
resp_df$market <- gsub("Dallas/Ft Worth", "Dallas", resp_df$market)
resp_df$quarter <- as.numeric(sub("Q", "", resp_df$quarter))
# quick fixes for wfh df
wfh_df <- wfh_df %>%
  filter(market != "Fort Worth") %>%
  mutate(market = recode(market, "San Jose" = "South Bay/San Jose")) %>%
  select(-yearquarter)
# quick fixes for unemployment df
unep_df <- unep_df %>%
  group_by(year, quarter, market) %>%
  summarize(unemployment_rate = mean(unemployment_rate, na.rm = TRUE), .groups = "drop")
unep_df$market <- dplyr::recode(
  unep_df$market,
  "Dallas/Ft Worth" = "Dallas",
  "New York" = "Manhattan",
  "Washington" = "Washington D.C."
)
unep_df <- unep_df %>%  mutate(
  unemployment_rate = unemployment_rate / 100, 
  quarter = as.numeric(sub("Q", "", quarter)))
# select useful things for now
resp_subdf <- resp_df[, c("market", "quarter", "year", "mt")]
join_1_df <- inner_join(resp_subdf, df, by = c("year", "quarter", "market"))
join_2_df <- inner_join(wfh_df, join_1_df, by = c("year", "quarter", "market"))
joined_df <- inner_join(unep_df, join_2_df, by = c("year", "quarter", "market"))
#  YAY it's done :)
final_df <- (joined_df %>%
  arrange(market, year, quarter))
# test out a plot
setwd("C://Users//thetr//OneDrive//Desktop//Misc//Personal Projects//asa_datafest_2025//additional_data//clean//")
write.csv(final_df, "final_data.csv", row.names = FALSE)
