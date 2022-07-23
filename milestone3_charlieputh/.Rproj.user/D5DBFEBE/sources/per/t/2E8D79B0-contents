# Import and attach libraries/packages

pkgs <- c('vosonSML', 'rtweet', 'stringr', 'tm', 'SnowballC', 'ggplot2', 'igraph',
          'slam', 'Rmpfr', 'rjson', 'leaflet', 'leaflet', 'httpuv', 'devtools', 'dplyr')
lapply(pkgs, library, character.only = TRUE)


# Set up authentication variables

appname <- "APP NAME"
my_api_key <- "aOqLz9bD1ZO8sSsMSRFqdm9wG"
my_api_secret <- "gneL08mAWZBb4upNBpfoq3I0F0vJwkYHOpW13mQX6cEHAmWeSC"
my_access_token <- "1040713787592998912-mYmj6XerRPh11hBVDL2KY2oDNBKV0E"
my_access_token_secret <- "REmQJhL3QLOKXLdO1uLhEKLbOKQjqbXh8CsrbQDFsozGc"


# Authenticate and get data

myTwitterData <- Authenticate("twitter",
                              appName= appname,
                              apiKey=my_api_key,
                              apiSecret=my_api_secret,
                              accessToken=my_access_token,
                              accessTokenSecret=my_access_token_secret,
                              useCachedToken = F) %>%
  Collect(searchTerm="iPhoneX", language="en", numTweets=2000, writeToFile=TRUE)


# Text Cleaning

# remove search term
myTwitterData$text = gsub("#iphonex|iphonex|iPhoneX|iphoneX", "", myTwitterData$text)
# remove RT
myTwitterData$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", myTwitterData$text)
# remove @ people
myTwitterData$text = gsub("@\\w+", "", myTwitterData$text)
# remove punctuation
myTwitterData$text = gsub("[[:punct:]]", "", myTwitterData$text)
# remove numbers
myTwitterData$text = gsub("[[:digit:]]", "", myTwitterData$text)
# remove html links
myTwitterData$text = gsub("http\\w+", "", myTwitterData$text)
# remove unnecessary tabs and newlines
myTwitterData$text = gsub("\\t", "", myTwitterData$text)
myTwitterData$text = gsub("\\n", "", myTwitterData$text)
# remove unnecessary spaces
myTwitterData$text = gsub("^\\s+|\\s+$", "", myTwitterData$text)
# to lowercase
myTwitterData$text = tolower(myTwitterData$text)


# Create Bimodal

g_twitter_bimodal <- myTwitterData %>% Create("bimodal")
g_twitter_bimodal <- g_twitter_bimodal$graph
V(g_twitter_bimodal)$name <- V(g_twitter_bimodal)$display_name

# View Graph Object
g_twitter_bimodal
length(V(g_twitter_bimodal))
V(g_twitter_bimodal)$name


# Get connected components

cc <- clusters(g_twitter_bimodal)
cc$no


# Get subnetwork with most members

max(cc$csize)
g_twitter_bimodal_sub <- induced_subgraph(g_twitter_bimodal,
                                           which(cc$membership == which.max(cc$csize)))

# Get in-degrees for each node (hashtag)

ind <- strength(g_twitter_bimodal_sub, mode="in")


# Get out-degrees for each node (user)

oud <- strength(g_twitter_bimodal_sub, mode="out")


# Display top 20 items from the subgraph ordered by in-degree and out-degree

V(g_twitter_bimodal_sub)[order(ind, decreasing=TRUE)[1:20]]
V(g_twitter_bimodal_sub)[order(oud, decreasing=TRUE)[1:20]]


# Centrality Measurement based on user-hashtag relationship (degree)

sort(degree(g_twitter_bimodal_sub, mode="in"), decreasing=TRUE)[1:30]
sort(degree(g_twitter_bimodal_sub, mode="out"), decreasing=TRUE)[1:30]


# Centrality Measurement based on user-hashtag relationship (closeness)

sort(closeness(g_twitter_bimodal_sub), decreasing=FALSE)[1:30]
sort(closeness(g_twitter_bimodal_sub), decreasing=TRUE)[1:30]


# Centrality Measurement based on user-hashtag relationship (betweenness)

sort(betweenness(g_twitter_bimodal_sub, directed = FALSE), decreasing=TRUE)[1:30]


# Create Actor Network

g_twitter_actor <- myTwitterData %>% Create("actor")
V(g_twitter_actor$graph)$name <- V(g_twitter_actor$graph)$screen_name


# Find Communities of Users

imc <- infomap.community(g_twitter_actor$graph, nb.trials = 3)
communityMembership <- membership(imc)
commDistribution <- summary(as.factor(communityMembership))


# Find community with the maximum number (will bottom entry)

tail(sort(commDistribution), n=3)


# Get all Communities and analyse

communities_list <- communities(imc)
length(communities_list)
communities_list[names(tail(sort(commDistribution), n=3))]
communities_list[names(head(sort(commDistribution), n=3))]


# Twitter Search API

View(trends_available())
current_trends <- get_trends(23424748)
View(current_trends)


# Geolocation 

lat1 <- -28.09469789418109
long1 <- 153.3255386358539
lat2 <- -27.93345069419669
long2 <- 153.46424102783203


# Draw Map

map <- leaflet() %>%
  addTiles() %>%
  fitBounds(long1, lat1, long2, lat2)
map


# Find tweets from this area and process them.

geo_tweet <- search_tweets("instagram", lang="en", 
                           geocode = "-27.9950,153.404,100km", n=1000)
geo_tweet_df <- lat_lng(geo_tweet)
geo_tweet_loc <- subset(geo_tweet_df, lat!="NA")
geo_tweet_loc <- geo_tweet_loc[c("text", "lat", "lng")]
geo_tweet_loc <- geo_tweet_loc %>%
  group_by(lat, lng) %>%
  mutate(concat_text = paste0(text, collapse = "<br/> <br/>")) %>%
  select(-text)
geo_tweet_loc <- unique(geo_tweet_loc)
View(geo_tweet_loc)


# Project Tweet Co-ordinates to map

geo_tweet_loc$longitude = as.numeric(as.character(geo_tweet_loc$lng))
geo_tweet_loc$latitude = as.numeric(as.character(geo_tweet_loc$lat))

map <- leaflet() %>%
  addTiles() %>%
  fitBounds(long1, lat1, long2, lat2) %>%
  addMarkers(geo_tweet_loc$longitude, geo_tweet_loc$latitude, popup=geo_tweet_loc$concat_text)
map


