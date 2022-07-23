pkgs <- c('devtools','vosonSML','magrittr','tm','igraph','stringr', 'SnowballC', 'ggplot2', 'rtweet',
'slam', 'Rmpfr', 'rjson', 'leaflet', 'httpuv', 'dplyr')
lapply(pkgs, library, character.only = TRUE)

appname <- "twitter"
my_api_key <- "aOqLz9bD1ZO8sSsMSRFqdm9wG"
my_api_secret <- "gneL08mAWZBb4upNBpfoq3I0F0vJwkYHOpW13mQX6cEHAmWeSC"
my_access_token <- "1040713787592998912-mYmj6XerRPh11hBVDL2KY2oDNBKV0E"
my_access_token_secret <- "REmQJhL3QLOKXLdO1uLhEKLbOKQjqbXh8CsrbQDFsozGc"

charlieTwitterData <- Authenticate("twitter",
                              appName= appname,
                              apiKey=my_api_key,
                              apiSecret=my_api_secret,
                              accessToken=my_access_token,
                              accessTokenSecret=my_access_token_secret,
                              useCachedToken = F) %>%

# Data retrieval
Collect(searchTerm="charlieputh", lang="en", numTweets=1500, writeToFile=TRUE)

View(charlieTwitterData)

charlieTwitterData$text <- iconv(charlieTwitterData$text, to="utf-8")
charlieTwitterData$text <- iconv(charlieTwitterData$text, "latin1", "ASCII", sub="")

# Text Cleaning
# remove RT
tweetText = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", charlieTwitterData$text)
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

# Count co-occurance from large to small
v <- sort(rowSums(tdm), decreasing = TRUE)
head(v, n=10)
df <- data.frame(word = names(v), freq = v)

ggplot(subset(df, freq>81), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  ggtitle('Words Frequency') + 
  xlab('Words') + 
  ylab('Frequency')

# Create Bimodal

charlie_twitter_bimodal <- charlieTwitterData %>% Create("bimodal")
charlie_twitter_bimodal <- charlie_twitter_bimodal$graph
V(charlie_twitter_bimodal)$name <- V(charlie_twitter_bimodal)$display_name

# View Graph Object
charlie_twitter_bimodal
length(V(charlie_twitter_bimodal))
V(charlie_twitter_bimodal)$name


# Get connected components

cc <- clusters(charlie_twitter_bimodal)
cc$no


# Get subnetwork with most members

max(cc$csize)
charlie_twitter_bimodal_sub <- induced_subgraph(charlie_twitter_bimodal,
                                          which(cc$membership == which.max(cc$csize)))

# Get in-degrees for each node (hashtag)

ind <- strength(charlie_twitter_bimodal_sub, mode="in")


# Get out-degrees for each node (user)

oud <- strength(charlie_twitter_bimodal_sub, mode="out")


# Display top 15 items from the subgraph ordered by in-degree and out-degree

V(charlie_twitter_bimodal_sub)[order(ind, decreasing=TRUE)[1:15]]
V(charlie_twitter_bimodal_sub)[order(oud, decreasing=TRUE)[1:15]]


# Centrality Measurement based on user-hashtag relationship (degree)

sort(degree(charlie_twitter_bimodal_sub, mode="in"), decreasing=TRUE)[1:20]
sort(degree(charlie_twitter_bimodal_sub, mode="out"), decreasing=TRUE)[1:20]


# Centrality Measurement based on user-hashtag relationship (closeness)

sort(closeness(charlie_twitter_bimodal_sub), decreasing=FALSE)[1:20]
sort(closeness(charlie_twitter_bimodal_sub), decreasing=TRUE)[1:20]


# Centrality Measurement based on user-hashtag relationship (betweenness)

sort(betweenness(charlie_twitter_bimodal_sub, directed = FALSE), decreasing=TRUE)[1:20]
