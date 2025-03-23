# making plots for covid cases
library(ggplot2)
library(tidyverse)
library(scales)

# set base theme for plots
theme_set(theme_bw())

# read data
covid4 <- read.csv("additional_data/covid_cases.csv")
# Create a combined year-quarter factor for better x-axis labeling
covid4$year_quarter <- factor(paste(covid4$year, covid4$quarter, sep = " Q"))

# make plot for **total** cases
total_city_covid <- ggplot(covid4, aes(x = year_quarter, y = total_cases, group = city, color = city)) +
  geom_line() +
  geom_point(size = 2) +
  labs(title = "Total COVID-19 Cases Over Time by City",
       x = "Year and Quarter",
       y = "Total Cases",
       color = "City") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_brewer(palette = "Paired") +
  scale_y_sqrt(labels = comma)
total_city_covid


# make plot for **new** cases
new_cases_per_city <- covid4 %>%
  ggplot(aes(x = year_quarter, y = new_cases, group = city, color = city)) +
  geom_line() +
  geom_point() +
  labs(title = "Quarterly COVID-19 Cases by City",
       x = "Year and Quarter",
       y = "New Cases (Sqrt Scale)",
       color = "City") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),) +
  scale_y_sqrt(labels = comma) + # from scales package, gives more interpretable numbers
  scale_color_brewer(palette = "Paired")
new_cases_per_city

ggsave(filename = "outputs/covid_cases_per_city_quarterly.png",
       plot = new_cases_per_city)
?ggsave
# bad bar chart
ggplot(covid4, aes(x = year_quarter, y = total_cases, fill = city)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Stacked Total COVID-19 Cases by Year and Quarter",
       x = "Year and Quarter",
       y = "Total Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Aggregate new cases by year and city
covid4_aggregated <- covid4 %>%
  group_by(year, city) %>%
  summarise(new_cases = sum(new_cases), .groups = "drop")

# create a count bar plot
new_cases_per_year <- covid4_aggregated %>%
  ggplot(aes(x = factor(year), y = new_cases, fill = city)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Yearly COVID-19 New Cases by City",
       x = "Year",
       y = "New Cases (Sqrt Scale)",
       fill = "City") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_sqrt(labels = comma) +
  scale_fill_brewer(palette = "Paired")
new_cases_per_year
