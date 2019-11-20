library(tidyverse)
library(rvest)
library(opencage) # Opencage API key saved as environment variable OPENCAGE_KEY
library(sf)
library(rnaturalearth)
library(transformr)
library(gganimate)
library(hrbrthemes)
library(geosphere)

# function to calculate distance between two coordinate points

get_geo_distance = function(long1, lat1, long2, lat2, units = "miles") {
  longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
  longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
  distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
  distance_m = purrr::pluck(distance_list, 1)
  if (units == "km") {
    distance = distance_m / 1000.0;
  }
  else if (units == "miles") {
    distance = distance_m / 1609.344
  }
  else {
    distance = distance_m
    # This will return in meter as same way as distHaversine function. 
  }
  distance
}

# get anita travel data

travel_stops <- read.csv("travel.csv")

# map world cloropleths and join to anita file

world_map <- rnaturalearth::ne_download(category = "cultural", returnclass = "sf", type = "map_units") %>% 
  st_transform(crs = 4326) %>% 
  filter(!NAME_EN %in% c("Fr. S. Antarctic Lands", "Antarctica"))

travel_map <- inner_join(world_map, travel_stops, by = c("NAME_EN" = "Country"))

# pinpoint cities

world <- unique(as.character(travel_stops$City)) %>% 
  map_df(~ {
    geoc <- opencage_forward(placename = .x)
    lat <- geoc$results$geometry.lat[1]
    long <- geoc$results$geometry.lng[1]
    tibble(Location = .x, lat = lat, lon = long)
  }) %>% 
  dplyr::right_join(travel_map, by = c("Location" = "City")) %>% 
  dplyr::arrange(Stop) 

# calculate distances

world$Dist <- rep(0, nrow(world))

for (i in 2:nrow(world)) {
  world$Dist[i] <- get_geo_distance(world$lon[i], world$lat[i], world$lon[i - 1], world$lat[i - 1]) %>% 
    round()
}

world <- world %>% 
  dplyr::mutate(dist_total = cumsum(Dist))


# calculate countries visited

world$countries_visited <- rep(0, nrow(world))

for (i in 1:nrow(world)) {
  world$countries_visited[i] <- length(unique(world$NAME_EN[1:i])) %>% as.integer()
}




# animate

p <- ggplot() +
  geom_sf(data = world_map, colour = "#ffffff20", fill = "#2d2d2d60", size = .5) +
  geom_sf(data = world, aes(fill = NAME_EN, geometry = geometry), colour = "white", show.legend = FALSE) +
  geom_point(data = world, aes(lon, lat), colour = "white") +
  geom_text(data = world, aes(lon, lat, label = Location), colour = "white", nudge_y = -5, size = 8) + 
  coord_sf(datum = NA) +
  transition_states(Stop, 1, 1) +
  labs(title = "My Travel Adventures", caption="Plot by @dr_keithmcnulty", x = NULL, y = NULL) +
  geom_text(data = world, aes(x = -180, y = -15, label = "Countries visited:"), size = 10, hjust = "left", family = 'Roboto Condensed', color = 'white') +
  geom_text(data = world, aes(x = -180, y = -25, label = format(countries_visited, nsmall = 0)), size = 15, hjust = "left", family = 'Roboto Condensed', color = 'white') +
  geom_text(data = world, aes(x = -180, y = -35, label = "Miles travelled:"), size = 10, hjust = "left", family = 'Roboto Condensed', color = 'white') +
  geom_text(data = world, aes(x = -180, y = -45, label = prettyNum(dist_total, big.mark = ",")), size = 15, hjust = "left", family = 'Roboto Condensed', color = 'white') +
  theme_ft_rc() +
  theme(axis.text=element_blank(), plot.caption = element_text(size = 9), plot.title = element_text(size = 40)) 

p_anim <- gganimate::animate(p, 200, fps = 5, duration = 200, width = 2000, height = 1200, renderer = ffmpeg_renderer())

save_animation(p_anim, "travel.mp4")

## to add audio, use ffmpeg in terminal:
##
## trim original audio track to desired length from desired start time to suit video: (example)
## ffmpeg -ss 00:00:25 -t 00:00:40 -i "Willie Nelson - On The Road Again (Official Audio).mp3" on_the_road_again.mp3
##
## add audio to mp4 (example):
## ffmpeg -i "travel.mp4" -i "on_the_road_again.mp3" -shortest travel_w_audio.mp4
##
## fade out audio near end based on start time and no of seconds to fade (example):
## ffmpeg -i travel_w_audio.mp4 -af 'afade=out:st=36:d=3' -c:v libx264 -crf 22 -preset fast travel_with_audio.mp4









