pkgs <- c("devtools","vosonSML","magrittr","tm","igraph","stringr")
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

######1.3 Data retrieval
Collect(searchTerm="#charlieputh", language="en", numTweets=1000, writeToFile=TRUE)

View(charlieTwitterData)

######1.4 Top 5 users
g_twitter_actor <- charlieTwitterData %>% Create("actor")
V(g_twitter_actor$graph)$name <- V(g_twitter_actor$graph)$screen_name
write.graph(g_twitter_actor$graph, "CharlieTwitterActor.graphml", format="graphml")

pageRank_charlie_actor <- sort(page.rank(g_twitter_actor$graph)$vector, decreasing=TRUE)
head(pageRank_charlie_actor, n=5)

######1.5 Top 10 terms
g_twitter_semantic <- charlieTwitterData %>% Create("semantic", stopwordsEnglish = T)
write.graph(g_twitter_semantic$graph, "CharlieTwitterSemantic.graphml", format="graphml")

pageRank_charlie_semantic <- sort(page_rank(g_twitter_semantic$graph)$vector, decreasing=TRUE)
head(pageRank_charlie_semantic, n=10)

######1.6 Retweets/accounts
count(charlieTwitterData[which(charlieTwitterData$is_retweet == FALSE),])