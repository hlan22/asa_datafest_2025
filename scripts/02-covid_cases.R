# calculate covid cases for all top 10 markets
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# we want only these counties

# Austin: Travis County, Texas .
# Chicago: Cook County, Illinois . 
# Dallas/Ft Worth: Dallas is in Dallas County, Texas
# Houston: Harris County, Texas . 
# Los Angeles: Los Angeles County, California . 
# Manhattan: New York County, New York .
# Philadelphia: Philadelphia County, Pennsylvania . 
# San Francisco: San Francisco County, California !!!
# South Bay/San Jose: Santa Clara County, California . 
# Washington D.C.: N/A !!!

# Define the target counties and their corresponding states
target_counties <- tibble(
  City = c("Austin", "Chicago", "Dallas", "Houston", "Los Angeles", 
           "Manhattan", "Philadelphia", "San Francisco", "South Bay/San Jose", "Washington D.C."),
  County = c("Travis County", "Cook County", "Dallas County", "Harris County", 
             "Los Angeles County", "New York County", "Philadelphia County", 
             "City and County of San Francisco", "Santa Clara County", "District of Columbia"),
  State = c("TX", "IL", "TX", "TX", "CA", "NY", "PA", "CA", "CA", "DC"),
  # State = c("Texas", "Illinois", "Texas", "Texas", "California", "New York", "Pennsylvania", 
  #           "California", "California", "District of Columbia")
)

# read in data
covid_data <- read_csv("additional_data/covid_confirmed_usafacts.csv")

# checkout data
str(covid_data)
colnames(covid_data)
unique(covid_data$State)
head(covid_data)


covid1 <- covid_data %>%
  pivot_longer(cols = -c(countyFIPS, `County Name`, State, StateFIPS), 
               names_to = "date", values_to = "cases") %>%
  mutate(
    date = as.Date(date),  # Convert column names to actual dates
    year = year(date),
    quarter = quarter(date)
  ) %>%
  filter(!(`County Name`== "Statewide Unallocated"))

# Filter for the desired states and counties
covid2 <- covid %>%
  filter(State %in% target_counties$State) %>% # only keep needed states
  filter(`County Name` %in% target_counties$County) %>% # only keep needed counties
  left_join(target_counties, by = c("County Name" = "County", "State" = "State")) %>% #
  select(City, `County Name`, State, date, year, quarter, cases) %>%
  rename(county = `County Name`,
         state = State,
         city = City,) %>%
  mutate(across(where(is.character), as.factor))

head(covid2)

covid3 <- covid2 %>%
  arrange(county, state, year, quarter, date) %>%  # Sort the data
  group_by(county, state, year, quarter) %>%      # Group by county, state, year, and quarter only
  filter(date == max(date))                       # Keep only the last date in each group

head(covid3)


covid4 <- covid3 %>%
  rename(total_cases = cases) %>%              # Rename cases to total_cases
  arrange(county, state, year, quarter) %>%    # Ensure proper chronological order
  group_by(county, state) %>%                  # Group by county and state to track across quarters
  mutate(new_cases = total_cases - lag(total_cases, default = 0)) %>%  # Calculate new cases
  ungroup()                                    # Remove grouping structure

head(covid4)

# write.csv(covid4, "additional_data/covid_cases.csv")

# edit file to correct chicago values
