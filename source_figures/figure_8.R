### Replication of Figure 8

# Purpose: Analyze rent growth (2011-2021) vs. percentage of rental units in selected cities

# Clear environment and load required packages
rm(list = ls())
pacman::p_load(
  tidyverse, httr, jsonlite, readxl, ggrepel, tidycensus
)

## DATA PREPARATION

# Retrieve data for the 25 largest US cities
all_city_data <- get_acs(
  geography = "place",
  variables = c("B25008_001E", "B25008_002E", "B25008_003E", "B25003_001E", "B25003_002E", "B25003_003E", "B01003_001E"),
  year = 2021,
  survey = "acs5",
  output = "wide"
)

# Sort cities by population and keep the top 25
top_25 <- all_city_data %>%
  arrange(desc(B01003_001E)) %>%
  slice(1:25) %>%
  mutate(
    percentage_rentalunits = B25003_003E / B25003_001E * 100,
    percentage_renters = B25008_003E / B25008_001E * 100,
    year = 2021
  )

# Import and process manually collected data for European cities
rentalunits_renters <- read_excel("data/rentalunits_renters_new.xlsx") %>%
  mutate(
    Name = ifelse(str_detect(NAME, "city,"),
      str_extract(NAME, ".*?(?=city,)"), NAME
    ),
    Name = ifelse(Name == "Wien", "Vienna", Name)
  ) %>%
  filter(str_detect(Name, regex(paste(c(
    "Amsterdam", "New York", "San Francisco", "Berlin", "London", "Paris",
    "Los Angeles", "Chicago", "Houston", "Philadelphia", "Washington",
    "Boston", "Vienna", "Seattle"
  ), collapse = "|"), ignore_case = TRUE))) %>%
  select(-NAME)

# API setup for Numbeo historical rent prices
api_key <- "qk9iaz7bvg3yjp"
api_url_historical_city_prices <- "https://www.numbeo.com/api/historical_city_prices?api_key="

# Define relevant cities and their IDs
city_ids <- c(
  "London" = 6512, "Amsterdam" = 5931, "Berlin" = 5439, "Paris" = 5426, "Vienna" = 5048,
  "New York, NY" = 3455, "Los Angeles, CA" = 3442, "Chicago, IL" = 3405, "Boston, MA" = 3400,
  "Houston, TX" = 3428, "Washington, DC" = 3497, "Seattle, WA" = 3487, "San Francisco, CA" = 3483,
  "Philadelphia, PA" = 3465
)

# Fetch and process historical price data for all cities from Numbeo
historical_data <- lapply(names(city_ids), function(city) {
  response <- GET(paste0(api_url_historical_city_prices, api_key, "&city_id=", city_ids[[city]]))
  if (http_status(response)$category == "Success") {
    data <- jsonlite::fromJSON(content(response, "text"))
    data.frame(data) %>%
      filter(entry.item_id %in% c(26, 27, 28, 29, 100, 101), entry.year %in% 2010:2022) %>%
      mutate(cityName = city)
  }
}) %>%
  bind_rows()

# Reshape data and calculate rent growth for 2011-2021
item27 <- historical_data %>%
  pivot_wider(names_from = entry.year, values_from = entry.amount) %>%
  filter(entry.item_id == 27) %>%
  mutate(
    rentgrowth20112021 = (`2021` / `2011` - 1) * 100,
    cityName = str_trim(str_extract(cityName, "^[^,]+"))
  )

# Prepare data for joining
rentalunits_renters <- rentalunits_renters %>%
  mutate(Name = str_trim(Name))

Datenpart1 <- item27 %>%
  select(cityName, rentgrowth20112021)

Datenpart2 <- rentalunits_renters %>%
  select(Name, percentage_rentalunits)

# Join rent growth and rental units data
cities_for_graph_27 <- left_join(Datenpart1, Datenpart2, by = c("cityName" = "Name"))

## PLOT CREATION

# Create Figure 8: Rent growth vs. percentage of rental units
figure8 <- ggplot(cities_for_graph_27, aes(x = percentage_rentalunits, y = rentgrowth20112021)) +
  geom_point() +
  geom_rect(
    data = subset(cities_for_graph_27, cityName == "Berlin"),
    aes(
      xmin = percentage_rentalunits - 2.5, xmax = percentage_rentalunits + 2.5,
      ymin = rentgrowth20112021 - 2.5, ymax = rentgrowth20112021 + 2.5
    ),
    fill = "transparent", color = "grey60", linewidth = 1, inherit.aes = FALSE, linetype = "solid"
  ) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text_repel(aes(label = cityName),
    box.padding = 0.3, point.padding = 0.3,
    size = 3.5, color = "gray40",
    bg.color = "white",
    bg.r = 0.25
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.caption = element_text(size = 5)
  ) +
  labs(
    x = "Percentage of rental units among all units",
    y = "Rental growth 2011-2021 (in %)"
  )

# Display the plot
print(figure8)
