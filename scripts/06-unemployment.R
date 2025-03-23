library(lubridate)
library(tidyverse)
# read data
unemployment <- read.csv(
  "C://Users//thetr//OneDrive//Desktop//datafest//2025 ASA DataFest//Unemployment.csv"
)
unemployment <- unemployment %>%
  mutate(date = make_date(year, month, 1))
# filter for after March 2020
unemp_filtered <- unemployment %>%
  filter(date >= as.Date("2020-03-01"))
# fetch state names
state_abbrev <- setNames(state.abb, state.name)
# add DC manually
state_abbrev["District of Columbia"] <- "DC"
# major markets
cities <- c(
  "Austin, Texas",
  "Chicago, Illinois",
  "Dallas/Ft Worth, Texas",
  "Houston, Texas",
  "Los Angeles, California",
  "New York, New York",
  "Philadelphia, Pennsylvania",
  "San Francisco, California",
  "South Bay/San Jose, California",
  "Washington, District of Columbia"
)
# expanding
city_df <- data.frame(full_place = cities) %>%
  separate(
    full_place,
    into = c("market", "state_full"),
    sep = ", ",
    remove = FALSE
  ) %>%
  mutate(state = state_abbrev[state_full]) %>% select(-full_place, -state_full)
# left join unemployment
city_unemp <-  city_df %>%
  left_join(unemp_filtered, by = "state", relationship = "many-to-many") %>%
  select(market, state, year, quarter, unemployment_rate)

# test out a plot
setwd("C://Users//thetr//OneDrive//Desktop//Misc//Personal Projects//asa_datafest_2025//additional_data//raw//")
write.csv(city_unemp, "unemployment_major_market.csv", row.names = FALSE)
ggplot(city_unemp,
       aes(
         x = paste0(year,quarter),
         y = unemployment_rate,
         color = market,
         group = market
       )) +
  geom_line(size = 1) +
  labs(
    title = "State Unemployment Rate, by Major Market",
    subtitle = "By Year, US Census Data",
    x = "Year",
    y = "Value",
    color = "Market"
  ) + scale_color_brewer(palette = "Paired") + theme_bw()
