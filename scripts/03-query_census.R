library(tidycensus)
library(tidyverse)

# Your variable vector remains unchanged
vars <- c(
  total_pop = "B01003_001",
  median_age = "B01002_001",
  median_income = "B19013_001",
  mean_income = "B19025_001",         
  per_capita_income = "B19301_001",
  median_home_value = "B25077_001",
  median_rent = "B25064_001",
  vacant_units = "B25002_003",
  total_units = "B25002_001",
  bachelors_degree_or_higher = "B15003_022",
  labor_force = "B23025_002",
  unemployed = "B23025_005",
  avg_commute_time = "B08303_001",
  worked_from_home = "B08301_021",
  public_transit = "B08301_010",
  drove_alone = "B08301_003",
  broadband = "B28002_004",
  no_internet = "B28002_013",
  mgmt_business_science_arts = "C24010_002",
  service_jobs = "C24010_003",
  household_size = "B25010_001"
)

# Cities to filter
cities <- c("Austin city, Texas",
            "Chicago city, Illinois",
            "Dallas city, Texas",
            "Fort Worth city, Texas",
            "Houston city, Texas",
            "Los Angeles city, California",
            "New York city, New York",
            "Philadelphia city, Pennsylvania",
            "San Francisco city, California",
            "San Jose city, California",
            "Washington city, District of Columbia")

counties <- c("Travis County, Texas",
              "Cook County, Illinois",
              "Dallas County, Texas",
              "Tarrant County, Texas",
              "Harris County, Texas",
              "Los Angeles County, California",
              "New York County, New York",
              "Philadelphia County, Pennsylvania",
              "San Francisco County, California",
              "Santa Clara County, California",
              "District of Columbia")

county_census_1yr <- map_dfr(2021:2023, function(yr) {
  get_acs(
    geography = "county",
    variables = vars,
    year = yr,
    survey = "acs1",
    output = "wide"
  ) %>%
    filter(NAME %in% counties) %>%
    mutate(
      year = yr,
      vacancy_rate = vacant_unitsE / total_unitsE,
      unemployment_rate = unemployedE / labor_forceE,
      pct_public_transit = public_transitE / labor_forceE,
      pct_worked_from_home = worked_from_homeE / labor_forceE,
      pct_drove_alone = drove_aloneE / labor_forceE
    )
})



market_labels <- c("Austin",
                   "Chicago",
                   "Dallas",
                   "Fort Worth",
                   "Houston",
                   "Los Angeles",
                   "Manhattan",
                   "Philadelphia",
                   "San Francisco",
                   "San Jose",
                   "Washington D.C.")

county_order <- c("Travis County, Texas",
                  "Cook County, Illinois",
                  "Dallas County, Texas",
                  "Tarrant County, Texas",
                  "Harris County, Texas",
                  "Los Angeles County, California",
                  "New York County, New York",
                  "Philadelphia County, Pennsylvania",
                  "San Francisco County, California",
                  "Santa Clara County, California",
                  "District of Columbia")

county_census_1yr$market <- market_labels[match(county_census_1yr$NAME, county_order)]
# small transform for percentages
county_census <- county_census %>%
  mutate(
    pct_public_transit = public_transitE / total_popE,
    pct_worked_from_home = worked_from_homeE / total_popE,
    pct_drove_alone = drove_aloneE / total_popE
  )
# extract wfh percentages
wfh_by_year <- county_census_1yr %>%
  select(market, year, pct_worked_from_home)%>%
  arrange(market, year)
# impute missing  years
imputed_wfh <- wfh_by_year %>%
  group_split(market) %>%
  map_dfr(function(df) {
    model <- lm(pct_worked_from_home ~ year, data = df)
    preds <- predict(model, newdata = data.frame(year = c(2020, 2024)))
    
    tibble(
      market = df$market[1],
      year = c(2020, 2024),
      pct_worked_from_home = preds
    )
  })
wfh_by_year <- rbind(wfh_by_year, imputed_wfh) %>%
  arrange(market, year)
