# Load packages
pkgs <- c("rtweet","ggplot2", "tm", 'Rspotify', "spotifyr", "dplyr", "plyr", "caret")
lapply(pkgs, library, character.only=TRUE)


####### 3.1) k-means

# Use twitter API to get the followers of Charlie Puth and information about each.
CP_followers <- get_followers("charlieputh", n = 10000)
user_data.df <- lookup_users(CP_followers$user_id)
View(user_data.df)

# Plot scatter of Friends Count vs Followers Count.
ggplot(data=user_data.df, aes(x=followers_count, y=friends_count)) + 
  geom_point(stat = "identity") + 
  xlab("Followers Count") + 
  ylab("Friends Count") + 
  ggtitle("Friends vs Followers")

# Plot scatter of Friends Count vs Followers Count (using logarithmic scales).
ggplot(data=user_data.df, aes(x=log(followers_count), y=log(friends_count))) + 
  geom_point(stat = "identity") + 
  xlab("Log Followers Count") + 
  ylab("Log Friends Count") + 
  ggtitle("Friends vs Followers - Log 10 Scale")

# Get Friends Count and Followers Count under logarithmic functions.
logFriendsCount <- log(user_data.df$friends_count)
logFollowersCount <- log(user_data.df$followers_count)

# Create data frame with log data derived above
kObject.log <- data.frame(user_data.df$name, logFollowersCount, logFriendsCount)
View(kObject.log)

# Remove invalid records from the data frame.
kObject.log <- subset(kObject.log, kObject.log$logFriendsCount!="-Inf")
kObject.log <- subset(kObject.log, kObject.log$logFollowersCount!="-Inf")

# Run KMeans Clustering Algorithm.
user2Means.log <- kmeans(kObject.log[,2:ncol(kObject.log)], centers=5, iter.max=10, nstart=10)
kObject.log$cluster=factor(user2Means.log$cluster)
View(kObject.log)

# Plot cluster data.
ggplot(data=kObject.log,
       aes(x=logFollowersCount, y=logFriendsCount, colour=cluster)) +
  geom_point(stat = "identity") + 
  xlab("Log Followers Count") + 
  ylab("Log Friends Count") + 
  ggtitle("Friends vs Followers - Log 10 Scale")

user2Means.log$centers

## Repeat for other Users! ##

# Use twitter API to get the followers of Derik Fein and information about each.
DF_followers <- get_followers("DerikFein", n = 10000)
user_data.df <- lookup_users(DF_followers$user_id)
View(user_data.df)

# Plot scatter of Friends Count vs Followers Count.
ggplot(data=user_data.df, aes(x=followers_count, y=friends_count)) + 
  geom_point(stat = "identity") + 
  xlab("Followers Count") + 
  ylab("Friends Count") + 
  ggtitle("Friends vs Followers")

# Plot scatter of Friends Count vs Followers Count (using logarithmic scales).
ggplot(data=user_data.df, aes(x=log(followers_count), y=log(friends_count))) + 
  geom_point(stat = "identity") + 
  xlab("Log Followers Count") + 
  ylab("Log Friends Count") + 
  ggtitle("Friends vs Followers - Log 10 Scale")

# Get Friends Count and Followers Count under logarithmic functions.
logFriendsCount <- log(user_data.df$friends_count)
logFollowersCount <- log(user_data.df$followers_count)

# Create data frame with log data derived above
kObject.log <- data.frame(user_data.df$name, logFollowersCount, logFriendsCount)
View(kObject.log)

# Remove invalid records from the data frame.
kObject.log <- subset(kObject.log, kObject.log$logFriendsCount!="-Inf")
kObject.log <- subset(kObject.log, kObject.log$logFollowersCount!="-Inf")

# Run KMeans Clustering Algorithm.
user2Means.log <- kmeans(kObject.log[,2:ncol(kObject.log)], centers=5, iter.max=10, nstart=10)
kObject.log$cluster=factor(user2Means.log$cluster)
View(kObject.log)

# Plot cluster data.
ggplot(data=kObject.log,
       aes(x=logFollowersCount, y=logFriendsCount, colour=cluster)) +
  geom_point(stat = "identity") + 
  xlab("Log Followers Count") + 
  ylab("Log Friends Count") + 
  ggtitle("Friends vs Followers - Log 10 Scale")

user2Means.log$centers

####### 3.2) Decision tree

# Spotify Authentication

app_id <- "8fee083e5ddc41948dc9836b9444c0fb"
app_secret <- "3c36b666d22349ceb33660ae50cf06a6"
token <- "1"

keys <- spotifyOAuth(token, app_id, app_secret)

Sys.setenv(SPOTIFY_CLIENT_ID = app_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()

# Get Songs from Charlie Puth (and their audio features)
charliesongs <- get_artist_audio_features('Charlie Puth')
View(charliesongs)

# Get Drake Songs that have a full record associated with them
getScore <- subset(charliesongs, track_name!="NA")
median(getScore$valence)
mean(getScore$valence)


# Plot Charlie Song Valence Data.
ggplot(data=getScore,
       aes(x=getScore$valence, y=getScore$track_name)) +
  geom_point() +
  theme(axis.text.y = element_text(size = 5)) +
  xlab("Valence Score") +
  ylab("Album") + 
  ggtitle("Charlie Puth's Valence Score")

# Get songs and features from an artist (DNCE) - same way done for Charlie
dncesongs <- get_artist_audio_features('DNCE')
getScoreDnce <- subset(dncesongs, track_name!="NA")

# Get songs and features from ZAYN - same way done for Charlie
zaynsongs <- get_artist_audio_features('ZAYN')
getScoreZayn <- subset(zaynsongs, track_name!="NA")

# Get songs and features from Nick Jonas - same way done for Charlie
njsongs <- get_artist_audio_features('Nick Jonas')
getScoreNj <- subset(njsongs, track_name!="NA")

# Add the isCharlie column to each score array (to indicate which songs are Charlie and which are not)
getScoreDnce["isCharlie"] <- 0
getScore["isCharlie"] <- 1
getScoreZayn["isCharlie"] <- 0
getScoreNj["isCharlie"] <- 0

# Combine the score arrays from the two artists
dataSet <- rbind.fill(getScoreDnce, getScore, getScoreZayn, getScoreNj)
dataSet <- dataSet[!duplicated(dataSet),]

# Select only the feature columns and isCharlie column.
cols <- c(9:19, 40)
dataSet <- dataSet[,cols]
View(dataSet[1,])

# Change the isCharlie column into a factor (for both testing and training sets)
dataSet$isCharlie <- factor(dataSet$isCharlie)

# Randomize data set
dataSet <- dataSet[sample(1:nrow(dataSet)),]

# Split the data set into training and testing sets (80% training set, 20% testing)
splitPoint <- as.integer(nrow(dataSet)*0.8)
trainingSet <- dataSet[1:splitPoint,]
testingSet <- dataSet[(splitPoint+1):nrow(dataSet),]

# Train the CART Model
prediction.r <- train(isCharlie~ ., data=trainingSet, method="rpart")
prediction.r
plot(prediction.r)

# Sample a single prediction (can repeat)
successRate = 0
successPercentage = 0
for (pRow in 1:nrow(testingSet)){
  print(paste("testing row:", pRow))
  prediction_row = pRow # MUST be smaller or equal to than training data set size
  if (predict(prediction.r, testingSet[prediction_row,]) == 
      testingSet[prediction_row, 12]){
    print("Correct!")
    successRate = successRate + 1
  } else{
    ("Wrong.")
  }
}
successPercentage = floor(successRate / nrow(testingSet) * 100)
a = paste("Rate of Success Predictions:", successPercentage, "%")
print(a)

# Analyse the model accuracy using the Confusion Matrix
confusionMatrix(prediction.r, reference = testingSet$isCharlie)


