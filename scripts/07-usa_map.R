install.packages(c("usmap", "ggrepel"))
# Load required packages
library(ggplot2)
library(usmap)
library(ggrepel)

# Set default theme and color palette as per your preferences
theme_set(theme_bw())
scale_colour_discrete <- function(...) scale_colour_brewer(palette = "Paired", ...)
scale_fill_discrete <- function(...) scale_fill_brewer(palette = "Paired", ...)

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
ggplot() +
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

