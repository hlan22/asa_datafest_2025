

covid4 <- read.csv("additional_data/covid_cases.csv")


library(ggplot2)

# Create a combined year-quarter factor for better x-axis labeling
covid4$year_quarter <- factor(paste(covid4$year, covid4$quarter, sep = " Q"))

ggplot(covid4, aes(x = year_quarter, y = total_cases, group = city, color = city)) +
  geom_line() +
  geom_point(size = 2) +
  labs(title = "Total COVID-19 Cases by Year and Quarter",
       x = "Year and Quarter",
       y = "Total Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

new_cases_per_city <- covid4 %>%
  #filter(!(city == "Chicago")) %>%
  ggplot(aes(x = year_quarter, y = new_cases, group = city, color = city)) +
  geom_line() +
  geom_point() +
  labs(title = "Quarterly COVID-19 Cases per City",
       x = "Year and Quarter",
       y = "New Cases (Sqrt Scale)",
       color = "City") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_sqrt()
new_cases_per_city


ggplot(covid4, aes(x = year_quarter, y = total_cases, fill = city)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Stacked Total COVID-19 Cases by Year and Quarter",
       x = "Year and Quarter",
       y = "Total Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





ggplot(covid4, aes(x = year_quarter, y = total_cases, fill = city)) +
  geom_area() +
  facet_wrap(~city, scales = "free_y") +
  labs(title = "COVID-19 Cases by Year and Quarter (Faceted by City)",
       x = "Year and Quarter",
       y = "Total Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))