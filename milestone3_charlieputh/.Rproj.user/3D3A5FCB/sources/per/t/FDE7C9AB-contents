# Import and attach libraries/packages

pkgs <- c('vosonSML', 'rtweet', 'stringr', 'tm', 'SnowballC', 'ggplot2', 'igraph',
          'slam', 'Rmpfr', 'rjson', 'leaflet', 'leaflet', 'httpuv', 'devtools', 'dplyr')
lapply(pkgs, library, character.only = TRUE)

# Twitter Search API

View(trends_available())
current_trends <- get_trends(23424748)
View(current_trends)


# Geolocation 

lat1 <- 47.608013
long1 <- -122.335167
lat2 <- 29.424349
long2 <- -98.491142


# Draw Map

map <- leaflet() %>%
  addTiles() %>%
  fitBounds(long1, lat1, long2, lat2)
map


# Find tweets from this area and process them.

geo_tweet <- search_tweets("charlieputh", lang="en", n=1000)
geo_tweet_df <- lat_lng(geo_tweet)
geo_tweet_loc <- subset(geo_tweet_df, lat!="NA")
geo_tweet_loc <- geo_tweet_loc[c("screen_name", "created_at", "text", "lat", "lng")]
geo_tweet_loc <- geo_tweet_loc %>%
  group_by(lat, lng) %>%
  mutate(concat_text = paste0(text, collapse = "<br/> <br/>")) %>%
  select(-text)
geo_tweet_loc <- unique(geo_tweet_loc)
View(geo_tweet_loc)
write.csv(geo_tweet_loc,"charlieputh.csv")


# Project Tweet Co-ordinates to map

geo_tweet_loc$longitude = as.numeric(as.character(geo_tweet_loc$lng))
geo_tweet_loc$latitude = as.numeric(as.character(geo_tweet_loc$lat))
map <- leaflet() %>%
  addTiles() %>%
  fitBounds(long1, lat1, long2, lat2) %>%
  addMarkers(geo_tweet_loc$longitude, geo_tweet_loc$latitude, popup=geo_tweet_loc$concat_text)
map


