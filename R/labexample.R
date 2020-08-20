## Project: Lab 03
## Script purpose: Week 3
## Date: August 19

library(tidyverse)
library(sf)

region = data.frame(region = state.region,
                    state_name = state.name)

south = USAboundaries::us_states() %>%
  left_join(region) %>%
  filter(region == "South")

plot(conus['aland'])

cities = readr::read_csv("data/uscities.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_filter(south, .predicate = st_intersects)

plot(south$geometry)
plot(cities$geometry, pch = 16, cex  = .1, add = T)

south_c = st_combine(south) %>%
  st_cast("MULTILINESTRING")

south_c = st_transform(south_c, 5070)
cities = st_transform(cities, 5070)

cities = cities %>%
  mutate(dist_to_state = st_distance(cities, south_c),
         dist_to_state = units::set_units(dist_to_state, "km"),
         dist_to_state = units::drop_units(dist_to_state))

ggplot() +
  geom_sf(data = south_c) +
  geom_sf(data = cities, aes(color = dist_to_state), size = .1) +
  geom_sf(data = cities_big, color = "navy") +
  scale_color_gradient(low = "grey", high = "darkred") +
  ggthemes::theme_map() +
  ggrepel::geom_label_repel(data = cities_big,
                            aes(label = city, geometry = geometry),
                            stat = "sf_coordinates",
                            size = 3)

cities_big = cities %>%
  group_by(state_name) %>%
  slice_max(population, n = 2)

library(ggrepel)
library(gghighlight)

ggplot() +
  geom_sf(data = south_c) +
  geom_sf(data = cities, aes(color = dist_to_state), size = .1) +
  gghighlight::gghighlight(population > 1e4) +
  geom_sf(data = cities_big, color = "navy") +
  scale_color_gradient(low = "grey", high = "darkred")

