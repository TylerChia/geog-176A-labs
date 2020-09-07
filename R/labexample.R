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


library(leaflet)
leaflet() %>%
  addTiles() %>%
  addMarkers(lng=-119.8489, lat=34.4140,popup = "UCSB")

leaflet() %>%
  setView(lng=-119.8489, lat=34.4140,zoom = 12) %>%
  addProviderTiles(providers$CartoDB)

leaflet() %>%
  setView(lng=-119.8489, lat=34.4140,zoom = 12) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addProviderTiles(providers$Stamen.TonerLines, options = providerTileOptions(opacity = .5))

leaflet(data = ) %>%
  addProviderTiles(providers$CartoDB) %>%
  addCircleMarkers(label = -address, popup = pop,
                   clusterOptions = markerClusterOptions())

pal <- colorFactor(c("darkgreen", "navy"), domain = c("Goleta","Santa Barbara"))

leaflet(data = ) %>%
  addProviderTiles(providers$CartoDB) %>%
  addCircleMarkers(color = -pal(city), fillOpacity = .5, stroke = FALSE) %>%
  addPolylines(data = , fillColor = "transparent", color = "black")

## Week 5
library(tidyverse)
library(sf)
library(raster)
library(getlandsat)
library(mapview)
library(osmdata )

bb = read_csv("data/uscities.csv") %>%
  filter(city  == "Palo") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(5070) %>%
  st_buffer(5000) %>%
  st_bbox() %>%
  st_as_sfc

mapview(bb)

#####

bwgs = st_transform(bb, 4326)

osm = osmdata::opq(bwgs) %>%
  osmdata::add_osm_feature("building") %>%
  osmdata::osmdata_sf()

mapview(osm$osm_polygons)

#####

bbwgs = st_bbox(bwgs)
scenes = lsat_scenes()

down = scenes %>%
  filter(min_lat <= bbwgs$ymin, max_lat >= bbwgs$ymax,
         min_lon <= bbwgs$xmin, max_lon >= bbwgs$xmax,
         as.Date(acquisitionDate) == as.Date("2016-09-26"))

write.csv(down, file = "data/palo-flood.csv", row.names = F)

## IN RMD ------------------------------------------------------------

meta = read_csv("data/palo-flood.csv")

files = lsat_scene_files(meta$download_url) %>%
  filter(grepl(paste0("B",1:6,".TIF$", collapse = "|"), file)) %>%
  arrange(file) %>%
  pull(file)

st = sapply(files, lsat_image)

s = stack(st) %>%
  setNames(paste0("band", 1:6))

cropper = bb %>%
  st_as_sf() %>%
  st_transform(crs(s))

r = crop(s, cropper)

par(mfrow = c(1,2))

plotRGB(r, r = 4, g = 3, b = 2)

plotRGB(r, r = 5, g = 4, b = 3, stretch = "hist")

ndvi = (r$band5 - r$band4) / (r$band5 + r$band4)

palette = colorRampPalette(c("blue","white","red"))

plot(ndvi, col = palette(256))

thresholding = function(x){ifelse(x <= 0,1, NA)}

flood = calc(ndvi, thresholding)
plot(flood)

mapview(flood)

