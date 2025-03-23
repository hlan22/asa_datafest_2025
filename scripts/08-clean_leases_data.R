# clean data in this script
library(tidyverse)

leases_data <- read_csv("data/Leases.csv")

# change the character variables to factors
# clean column names
leases <- leases_data %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(quarter = as.numeric(recode(quarter, 
                                     "Q1" = "1", 
                                     "Q2" = "2", 
                                     "Q3" = "3", 
                                     "Q4" = "4"))) %>%
  mutate(building_quality = factor(recode(internal_class, A = "high", O = "low")))

# basic eda
colnames(leases)
str(leases)
summary(leases)
nrow(leases)


# test cleaning worked
unique(leases$quarter)
unique(leases$building_quality)

leases_updated <- leases %>%
  select(-c(company_name, address, building_id, building_name, zip, 
            internal_class, # updated it to building_quality
            costarID,
            
            )) %>%
  rename(rentable_building_area = RBA,
         total_leased_sf = leasing,
         
         )

str(leases_updated)

# select top 10 markets to simplify
top_10_markets <- c("Austin", 
                    "Chicago",
                    "Dallas/Ft Worth",
                    "Houston",
                    "Los Angeles",
                    "Manhattan",
                    "Philadelphia", # red
                    "San Francisco",
                    "South Bay/San Jose",
                    "Washington D.C."
                    )


# filter data
leases_top_10 <- leases_updated %>%
  filter(market %in% top_10_markets) %>%
  mutate(state_politics = factor(case_when(
    market %in% c("Philadelphia", "Dallas/Ft Worth", "Austin", "Houston") ~ "republican",
    market %in% c("Chicago", "Los Angeles", "Manhattan", "San Francisco",
                  "South Bay/San Jose", "Washington D.C.") ~ "democratic",
    TRUE ~ "Unknown"
  )))

nrow(leases_top_10)
colnames(leases_top_10)
str(leases_top_10)

# want to join a major market in the US?
# want to know how your current market is doing?
# want to predict who shows up based off of 

# predicting how many people will show up in Q4 based off of 
# finding variables to explain why theyre showing up or not showing up
# 
## wealth distribution
## policy 


summary(leases$market)
unique(leases$market)


summary(leases$city)
summary(leases$year)


leases_top_10 %>%
  group_by(market) %>%
  summarize(quarter_mean = mean(quarter))

# industry amounts


# 1. political state status
# 2. covid cases
# 3. general health metrics? accident rate?
# 4. weather (extreme weather events)
# 5. unemployment rates



# 5. weather (stay home in hot summers or cold winters) -- removed
# 6. statutory holidays -- removed with seasonality I would assume
# 7. general gdp of the city? Less companies involved


summary(leases_top_10$internal_industry)


