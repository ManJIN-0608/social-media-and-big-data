pkgs <- c('devtools','vosonSML','magrittr','tm','igraph','stringr', 'SnowballC', 'ggplot2', 'rtweet',
'slam', 'Rmpfr', 'rjson', 'leaflet', 'httpuv', 'dplyr')
lapply(pkgs, library, character.only = TRUE)

appname <- "twitter"
my_api_key <- "aOqLz9bD1ZO8sSsMSRFqdm9wG"
my_api_secret <- "gneL08mAWZBb4upNBpfoq3I0F0vJwkYHOpW13mQX6cEHAmWeSC"
my_access_token <- "1040713787592998912-mYmj6XerRPh11hBVDL2KY2oDNBKV0E"
my_access_token_secret <- "REmQJhL3QLOKXLdO1uLhEKLbOKQjqbXh8CsrbQDFsozGc"

zaynTwitterData <- Authenticate("twitter",
                              appName= appname,
                              apiKey=my_api_key,
                              apiSecret=my_api_secret,
                              accessToken=my_access_token,
                              accessTokenSecret=my_access_token_secret,
                              useCachedToken = F) %>%

# Data retrieval
Collect(searchTerm="ZAYN", lang="en", numTweets=1500, writeToFile=TRUE)

View(zaynTwitterData)

zaynTwitterData$text <- iconv(zaynTwitterData$text, to="utf-8")
zaynTwitterData$text <- iconv(zaynTwitterData$text, "latin1", "ASCII", sub="")

# Text Cleaning
# remove RT
tweetText = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", zaynTwitterData$text)
# remove @ people
tweetText = gsub("@\\w+", "", tweetText)
# remove punctuation
tweetText = gsub("[[:punct:]]", "", tweetText)
# remove numbers
tweetText = gsub("[[:digit:]]", "", tweetText)
# remove html links
tweetText = gsub("http\\w+", "", tweetText)
# remove unnecessary tabs and newlines
tweetText = gsub("\\t", "", tweetText)
tweetText = gsub("\\n", "", tweetText)
# remove unnecessary spaces
tweetText = gsub("^\\s+|\\s+$", "", tweetText)
# to lowercase
tweetText = tolower(tweetText)

# Convert tweetText Vector to a collection of text documents
tweetCorpus <- Corpus(VectorSource(tweetText))
tweetCorpus[[1]]$content
tweetCorpus[[5]]$content

# Preform Stemming 
doctext <- tweetCorpus %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stemDocument)

# Transform doctext into a document matrix
tdm <- as.matrix(TermDocumentMatrix((doctext)))

# Create Bimodal

zayn_twitter_bimodal <- zaynTwitterData %>% Create("bimodal")
zayn_twitter_bimodal <- zayn_twitter_bimodal$graph
V(zayn_twitter_bimodal)$name <- V(zayn_twitter_bimodal)$display_name

# View Graph Object
zayn_twitter_bimodal
length(V(zayn_twitter_bimodal))
V(zayn_twitter_bimodal)$name


# Get connected components

cc <- clusters(zayn_twitter_bimodal)
cc$no


# Get subnetwork with most members

max(cc$csize)
zayn_twitter_bimodal_sub <- induced_subgraph(zayn_twitter_bimodal,
                                          which(cc$membership == which.max(cc$csize)))

# Get in-degrees for each node (hashtag)

ind <- strength(zayn_twitter_bimodal_sub, mode="in")


# Get out-degrees for each node (user)

oud <- strength(zayn_twitter_bimodal_sub, mode="out")


# Display top 15 items from the subgraph ordered by in-degree and out-degree

V(zayn_twitter_bimodal_sub)[order(ind, decreasing=TRUE)[1:15]]
V(zayn_twitter_bimodal_sub)[order(oud, decreasing=TRUE)[1:15]]


# Centrality Measurement based on user-hashtag relationship (degree)

sort(degree(zayn_twitter_bimodal_sub, mode="in"), decreasing=TRUE)[1:20]
sort(degree(zayn_twitter_bimodal_sub, mode="out"), decreasing=TRUE)[1:20]


# Centrality Measurement based on user-hashtag relationship (closeness)

sort(closeness(zayn_twitter_bimodal_sub), decreasing=FALSE)[1:20]
sort(closeness(zayn_twitter_bimodal_sub), decreasing=TRUE)[1:20]


# Centrality Measurement based on user-hashtag relationship (betweenness)

sort(betweenness(zayn_twitter_bimodal_sub, directed = FALSE), decreasing=TRUE)[1:20]
