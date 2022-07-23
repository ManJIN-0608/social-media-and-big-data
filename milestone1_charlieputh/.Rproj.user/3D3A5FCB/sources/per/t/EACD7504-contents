pkgs <- c("Rspotify", "spotifyr", "httpuv", "ggridges", "highcharter", "knitr", 
          "tm", "tidyverse", "igraph", "ggplot2", "stringr", "scales")

lapply(pkgs, library, character.only = TRUE)

options(httr_oauth_cache = TRUE)

app_id <- "8fee083e5ddc41948dc9836b9444c0fb"
app_secret <- "3c36b666d22349ceb33660ae50cf06a6"
token <- "1"

keys <- spotifyOAuth(token, app_id, app_secret)

Sys.setenv(SPOTIFY_CLIENT_ID = app_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()

######1.3 Data retrieval
# Charlie Puth
keyword <- "Charlie+Puth"

# Retrieve artist by keyword
findArtist <- searchArtist(keyword, token=keys)

# Retrieve information about artist
charlie_puth <- getArtistinfo(findArtist$id[1], token=keys)

# Retrieve albums of that artist
charlie_puth.album <- getAlbums(findArtist$id[1], token=keys)

# Retrieve information on songs from that ablum
album.songs <- getAlbum(charlie_puth.album$id[1], token=keys)
album.songs2 <- getAlbum(charlie_puth.album$id[2], token=keys)
album.songs3 <- getAlbum(charlie_puth.album$id[3], token=keys)

# Retrieve information on a specific song on that album
song <- getFeatures(album.songs$id[1], token=keys)


# Charlie Puth Example
charlieputh <- get_artist_audio_features('charlie puth')
view(charlieputh)

# Get happiest songs from the Charlie Puth (If the '-' symbol does not preceed valence, we sort tracks in ascending rather than descending order)
charlieputh %>%
  arrange(-valence) %>%
  select(track_name, artist_name, valence) %>%
  head(10) %>%
  kable()

# Plot Valence Scores for Every Album
ggplot(charlieputh, aes(x = valence, y= album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("Plot of Charlie Puth' Joy Distributions",
          subtitle = "Based on valence from Spotify's Web API")

######1.3 Data retrieval
# Charlie Puth Relationship Example 
related <- getRelated('charlie puth', token=keys)
view(related)


######1.7 Spotify related artists
# Construct Edges
edges <- c()
for (artist in related$name){
  related <- getRelated(artist, token=keys)
  for (relatedartist in related$name){
    edges <- append(edges, artist)
    edges <- append(edges, relatedartist)
  }
}

# Create Graph and Save to External File
g_spotify_charlie <- graph(edges)
write.graph(g_spotify_charlie, "g_spotify_charlie.graphml", format="graphml")