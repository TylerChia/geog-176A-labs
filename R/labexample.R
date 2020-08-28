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

## Week 4
states = USAboundaries::us_states()
tmp = states %>%
  filter(grepl("New", name))
plot(tmp$geometry, col = "red")

#####

nearest_state_plot = function(name) {
state.of.interest = "name"
states = USAboundaries::us_states() %>%
  st_transform(5070)

soi = states %>%
  filter(state_name == state.of.interest)

adjoining = st_filter(states, soi, .predicate = st_touches)

sample = st_make_grid(soi, n = 70) %>%
  st_sf() %>%
  st_centroid()

closest = st_join(sample, adjoining, join = st_nearest_feature)

voroni = closest %>%
  st_union() %>%
  st_voronoi() %>%
  st_cast() %>%
  st_sf()

v_state = st_join(voroni, closest) %>%
  st_cast()

combined = v_state %>%
  group_by(state_name) %>%
  summarize() %>%
  st_intersection(soi)

ggplot() +
  geom_sf(data = adjoining, aes(fill = state_name)) +
  geom_sf(data = combined, aes(fill = state_name), col = NA) +
  geom_sf(data = soi, col = "black", fill = "white", alpha = .5) +
  theme_minimal() +
  labs(fill = "")
}
nearest_state_plot("Alabama")
