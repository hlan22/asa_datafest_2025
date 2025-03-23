# map the major markets and add info about political rates
# install.packages(c("usmap", "ggrepel"))
# Load required packages
library(ggplot2)
library(usmap)
library(ggrepel)
library(dplyr)

# Get US map data
us_map <- map_data("state")

# Define the 10 major market cities and their approximate coordinates
major_cities <- tibble(
  city = c("Austin", "Chicago", "Dallas", "Houston", "Los Angeles", 
           "Manhattan", "Philadelphia", "San Francisco", "South Bay/San Jose", "Washington D.C."),
  county = c("Travis County", "Cook County", "Dallas County", "Harris County", 
             "Los Angeles County", "New York County", "Philadelphia County", 
             "City and County of San Francisco", "Santa Clara County", "District of Columbia"),
  state = c("TX", "IL", "TX", "TX", "CA", "NY", "PA", "CA", "CA", "DC"),
  lon = c(-97.7431, -87.6298, -96.7970, -95.3698, -118.2437, 
          -73.9945, -75.1652, -122.4194, -121.8863, -77.0369),
  lat = c(30.2672, 41.8781, 32.7767, 29.7604, 34.0522, 
          40.7831, 39.9526, 37.7749, 37.3382, 38.9072)
)

# Plot the US map with labeled cities
plain_map_with_cities <- ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), 
               fill = "grey90", color = "black", linewidth = 0.2) +
  geom_point(data = major_cities, 
             aes(x = lon, y = lat), 
             color = "red", 
             size = 3) +
  geom_text_repel(data = major_cities, 
                  aes(x = lon, y = lat, label = city), 
                  size = 3, 
                  color = "black", 
                  box.padding = 0.5, 
                  max.overlaps = Inf) +
  coord_map("albers", lat0 = 30, lat1 = 40) +  # Albers projection for better US map appearance
  labs(title = "US Map with 10 Major Market Cities") +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )
plain_map_with_cities

us_map <- map_data("state")

# Using your target_counties as major_cities
major_cities <- tibble(
  city = c("Austin", "Chicago", "Dallas", "Houston", "Los Angeles", 
           "Manhattan", "Philadelphia", "San Francisco", "South Bay/San Jose", "Washington D.C."),
  lon = c(-97.7431, -87.6298, -96.7970, -95.3698, -118.2437, 
          -73.9945, -75.1652, -122.4194, -121.8863, -77.0369),
  lat = c(30.2672, 41.8781, 32.7767, 29.7604, 34.0522, 
          40.7831, 39.9526, 37.7749, 37.3382, 38.9072)
)

# Enhanced US map with labeled cities
enhanced_map_with_cities <- ggplot() +
  # Add a subtle background fill for the map
  geom_polygon(data = us_map, aes(x = long, 
                                  y = lat, group = group), 
               fill = "#E6E6E6", 
               color = "black", 
               linewidth = 0.3, alpha = 0.9) +
  # Use distinct, larger points with slight transparency
  geom_point(data = major_cities, 
             aes(x = lon, y = lat), 
             color = "#D53E4F", size = 4, alpha = 0.9, shape = 19) +
  # Improve label readability with larger font, subtle background, and better positioning
  geom_label_repel(data = major_cities, 
                   aes(x = lon, y = lat, label = city), 
                   size = 3, 
                   color = "black", 
                   fill = "white", 
                   alpha = 0.7, 
                   box.padding = 0.8, 
                   label.padding = 0.3,
                   max.overlaps = Inf,
                   seed = 42) +  # Seed for reproducible label placement
  # Keep Albers projection for accurate US map appearance
  coord_map("albers", lat0 = 30, lat1 = 40) +
  # Enhance title and add a subtitle for context
  labs(
    title = "Major Market Cities in the U.S.",
    # subtitle = "Highlighted Locations for Post-COVID Office Occupancy Analysis",
    # caption = "Source: Custom Dataset of Target Counties"
  ) +
  # Refine theme for presentation
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
    plot.caption = element_text(size = 10, hjust = 1, color = "grey50"),
    legend.position = "none"
  )

enhanced_map_with_cities

# ggsave(filename = "outputs/city_map.png",
#        plot = enhanced_map_with_cities)


# Add political affiliation to us_map
state_affiliation_with_swing <- tibble(
  region = tolower(state.name),
  affiliation = case_when(
    region %in% tolower(c("Alabama", "Alaska", "Arkansas", "Florida", "Idaho", "Indiana", "Iowa", 
                          "Kansas", "Kentucky", "Louisiana", "Mississippi", "Missouri", "Montana", 
                          "Nebraska", "North Dakota", "Ohio", "Oklahoma", "South Carolina", 
                          "South Dakota", "Tennessee", "Texas", "Utah", "West Virginia", "Wyoming")) ~ "Republican",
    region %in% tolower(c("California", "Colorado", "Connecticut", "Delaware", "Hawaii", "Illinois", 
                          "Maryland", "Massachusetts", "Minnesota", "New Hampshire", "New Jersey", 
                          "New Mexico", "New York", "Oregon", "Rhode Island", "Vermont", "Virginia", 
                          "Washington")) ~ "Democratic",
    region %in% tolower(c("Arizona", "Georgia", "Michigan", "Nevada", "Pennsylvania", "Wisconsin")) ~ "Swing",
    TRUE ~ "Other"  # DC handled separately if needed
  )
)

state_affiliation <- tibble(
  region = tolower(state.name),
  affiliation = case_when(
    region %in% tolower(c("Alabama", "Alaska", "Arkansas", "Florida", "Idaho", "Indiana", "Iowa", 
                          "Kansas", "Kentucky", "Louisiana", "Mississippi", "Missouri", "Montana", 
                          "Nebraska", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
                          "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                          "West Virginia", "Wyoming")) ~ "Republican",
    region %in% tolower(c("Arizona", "California", "Colorado", "Connecticut", "Delaware", "Georgia", 
                          "Hawaii", "Illinois", "Maine", "Maryland", "Massachusetts", "Michigan", 
                          "Minnesota", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                          "New York", "Oregon", "Pennsylvania", "Rhode Island", "Vermont", 
                          "Virginia", "Washington", "Wisconsin")) ~ "Democratic",
    TRUE ~ NA_character_  # Shouldn’t trigger since all 50 states are covered
  )
) %>%
  # Add District of Columbia
  add_row(region = "district of columbia", affiliation = "Democratic")



# Merge affiliation with us_map
us_map <- us_map %>% 
  left_join(state_affiliation, by = "region")

# Enhanced map with political affiliation
political_map_with_cities <- ggplot() +
  # State polygons with political fill
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group, fill = affiliation), 
               color = "black", linewidth = 0.3, alpha = 0.9) +
  # City points
  geom_point(data = major_cities, 
             aes(x = lon, y = lat), 
             color = "black", size = 4, alpha = 0.9, shape = 19) +
  # City labels
  geom_label_repel(data = major_cities, 
                   aes(x = lon, y = lat, label = city), 
                   size = 3, color = "black", fill = "white", alpha = 0.7, 
                   box.padding = 0.8, label.padding = 0.3, max.overlaps = Inf, seed = 42) +
  # Albers projection
  coord_map("albers", lat0 = 30, lat1 = 40) +
  # Custom fill colors for political affiliation
  scale_fill_manual(values = c("Republican" = "red", #D53E4F", 
                               "Democratic" = "dodgerblue", 
                               "Swing" = "#A9A9A9"), #C594FF"),
                    name = "State Political Affiliation (2020-2024)") +
  # Titles and labels
  labs(
    title = "Major Market Cities and Political Affiliation in the U.S.",
    # subtitle = "States Colored by Presidential Election Outcomes"
  ) +
  # Theme adjustments
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

political_map_with_cities

ggsave(filename = "outputs/political_map.png",
       plot = political_map_with_cities,
       width = 8,
       height = 6
       )
