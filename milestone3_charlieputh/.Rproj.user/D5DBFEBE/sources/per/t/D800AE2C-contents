pkgs <- c("vosonSML", "magrittr", "sentiment", "ggplot2", "tm", "slam", "topicmodels", "Rmpfr", 
          'igraph', 'tuber')
lapply(pkgs, library, character.only = TRUE)

appname <- "twitter"
my_api_key <- "aOqLz9bD1ZO8sSsMSRFqdm9wG"
my_api_secret <- "gneL08mAWZBb4upNBpfoq3I0F0vJwkYHOpW13mQX6cEHAmWeSC"
my_access_token <- "1040713787592998912-mYmj6XerRPh11hBVDL2KY2oDNBKV0E"
my_access_token_secret <- "REmQJhL3QLOKXLdO1uLhEKLbOKQjqbXh8CsrbQDFsozGc"

myTwitterData <- Authenticate("twitter",
                              appName= appname,
                              apiKey=my_api_key,
                              apiSecret=my_api_secret,
                              accessToken=my_access_token,
                              accessTokenSecret=my_access_token_secret,
                              useCachedToken = F) %>%
  Collect(searchTerm="charlie puth", includeRetweets = FALSE, lang="en",
          numTweets=1500, writeToFile = TRUE)


# Text Preprocessing/Cleaning

tweetUTF <- iconv(myTwitterData$text, to="utf-8")
some_txt <- iconv(tweetUTF, "latin1", "ASCII", sub="") # Remove none utf-8 characters

tweetText = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt) # remove RT
tweetText = gsub("@\\w+", "", tweetText)# remove @ people
tweetText = gsub("[[:punct:]]", "", tweetText) # remove punctuation
tweetText = gsub("[[:digit:]]", "", tweetText) # remove numbers
tweetText = gsub("http\\w+", "", tweetText) # remove html links
#tweetText = gsub("\\t", "", tweetText) # remove tabs
#tweetText = gsub("\\n", "", tweetText) # remove newlines
tweetText = gsub("[ \t]{2,}", "", tweetText) # remove unnecessary spaces
tweetText = gsub("^\\s+|\\s+$", "", tweetText) # remove unnecessary spaces
tweetText = tolower(tweetText) #to lowercase

# Sentiment Analysis

# classify emotion
class_emo = classify_emotion(tweetText, algorithm="bayes", prior=1.0)

# get emotion best fit
emotion = class_emo[,7]

# classify polarity
class_pol = classify_polarity(tweetText, algorithm="bayes")

# get polarity best fit
polarity = class_pol[,4]

# data frame containing results
sent_df = data.frame(text=tweetText, emotion=emotion, 
                     polarity=polarity, stringsAsFactors = FALSE)

# sort data frame
sent_df = within(sent_df, emotion <- factor(
  emotion, levels=names(sort(table(emotion), decreasing = TRUE))))
View(sent_df)

# discard emotions classified as 'unknown
final_plot <- subset(sent_df, sent_df$emotion!="unknown")

# visualize the data for emotion
ggplot(final_plot, aes(x=emotion)) +
  geom_bar(aes(fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets") +
  ggtitle("Sentiment Analysis of Tweets \n(Classification by Emotion)")

# visualize the data for polarity
ggplot(final_plot, aes(x=polarity)) +
  geom_bar(aes(fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets") +
  ggtitle("Sentiment Analysis of Tweets \n(Classification by Polarity)")


#LDA Topic Modelling

# transform tweet into vector corpus (and remove stopwords)
tweetCorpus <- Corpus(VectorSource(tweetText)) %>%
  tm_map(removeWords, stopwords(kind = 'en'))

# build document term matrix
dtmTopicModeling <- DocumentTermMatrix(tweetCorpus)

# adjust sparsity level (optional)
dtmTopicModeling <- removeSparseTerms(dtmTopicModeling, sparse=0.98)

# build TF-IDF matrix to remove unimportant terms based on co-occurrence characteristics
term_tfidf <- tapply(dtmTopicModeling$v/row_sums(dtmTopicModeling)
                     [dtmTopicModeling$i], dtmTopicModeling$j, mean) *
                      log2(nDocs(dtmTopicModeling)/
                             col_sums(dtmTopicModeling > 0 ))

# get median of term_tfidf and ommit frequent terms based on it.
median_tfidf <- summary(term_tfidf)[3]
dtmTopicModeling <- dtmTopicModeling[, term_tfidf >= median_tfidf]

# remove rows with zero entries
toRemove <- which(row_sums(dtmTopicModeling) == 0)
dtmTopicModeling <- dtmTopicModeling[row_sums(dtmTopicModeling) > 0,]

#define harmonic mean function
harmonicMean <- function(logLikelihoods, precision=2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods, prec = precision) + llMed))))
}

# LDA parameter definition
burnin = 1000
iter = 1000
keep = 50
sequ <- seq(2, 100, 10)

# run LDA model for each value of K
fitted_many <- lapply(sequ, function(k) LDA(dtmTopicModeling, k = k,
                                            method = "Gibbs",
                                            control = list(burnin = burnin, iter = iter, keep = keep)))

# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L) L@logLiks[
  -c(1:(burnin/keep))])

# compute the harmonic mean of log-likelihood values
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

# plot result (setting number of topics based on maximum harmonic mean)
plot(sequ, hm_many, type = "l")

# save number of topics to variable 'k'
k <- sequ[which.max(hm_many)]

# run final LDA model
seedNum <- 42
lda <- LDA(dtmTopicModeling, k = k, method = "Gibbs", control = list(
  burnin = burnin, iter = iter, keep = keep, seed = seedNum))

# look at top 10 terms in each topic
topTenTermsEachTopic <- terms(lda, 10)
View(topTenTermsEachTopic)

# take tweets from topic 1 (change number/s for other topics)
topicsProb <- topics(lda, 1)
topTenTermsEachTopic[,1]
topic1tweets <- which(topicsProb == 1)

# delete removed rows from tweet corpus
tweetCorpus_LDA <- tweetCorpus[-toRemove]

# sample of five tweetes from topic 1
topics1TweetsText <- as.list(tweetCorpus_LDA[topic1tweets])
sampleTweets <- sample(topics1TweetsText, 5)
View(sampleTweets)

##### different data source

# Youtube API Credentials and Authenticate

apiKey <- "AIzaSyC7X31Uj8_xPnE8whKL-MoPMVqg14ap_Jg"
appID <- "597183295495-770rn2a1n9i7f3nt4kdtkbt2i47ivr96.apps.googleusercontent.com"
appSecret <- "C2jeBjDQKZ5CChavf4LFSiC6"

yt_oauth(appID, appSecret)


# Start search and collect some YT data

searchResult <- yt_search("zayn")
View(searchResult)

getcomment <- get_comment_threads(filter = c(video_id = 'nfs8NYg7yQM'), max_results = 100)
View(getcomment)

# Text Preprocessing/Cleaning

ytbUTF <- iconv(getcomment$textOriginal, to="utf-8")
some_txt <- iconv(ytbUTF, "latin1", "ASCII", sub="") # Remove none utf-8 characters

ytbText = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt) # remove RT
ytbText = gsub("@\\w+", "", ytbText)# remove @ people
ytbText = gsub("[[:punct:]]", "", ytbText) # remove punctuation
ytbText = gsub("[[:digit:]]", "", ytbText) # remove numbers
ytbText = gsub("http\\w+", "", ytbText) # remove html links
#ytbText = gsub("\\t", "", ytbText) # remove tabs
#ytbText = gsub("\\n", "", ytbText) # remove newlines
ytbText = gsub("[ \t]{2,}", "", ytbText) # remove unnecessary spaces
ytbText = gsub("^\\s+|\\s+$", "", ytbText) # remove unnecessary spaces
ytbText = tolower(ytbText) #to lowercase

# Sentiment Analysis

# classify emotion
class_emo = classify_emotion(ytbText, algorithm="bayes", prior=1.0)

# get emotion best fit
emotion = class_emo[,7]

# classify polarity
class_pol = classify_polarity(ytbText, algorithm="bayes")

# get polarity best fit
polarity = class_pol[,4]

# data frame containing results
sent_df = data.frame(text=ytbText, emotion=emotion, 
                     polarity=polarity, stringsAsFactors = FALSE)

# sort data frame
sent_df = within(sent_df, emotion <- factor(
  emotion, levels=names(sort(table(emotion), decreasing = TRUE))))
View(sent_df)

# discard emotions classified as 'unknown
final_plot <- subset(sent_df, sent_df$emotion!="unknown")

# visualize the data for emotion
ggplot(final_plot, aes(x=emotion)) +
  geom_bar(aes(fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of comments") +
  ggtitle("Sentiment Analysis of comments \n(Classification by Emotion)")

# visualize the data for polarity
ggplot(final_plot, aes(x=polarity)) +
  geom_bar(aes(fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of comments") +
  ggtitle("Sentiment Analysis of comments \n(Classification by Polarity)")


#LDA Topic Modelling

# transform tweet into vector corpus (and remove stopwords)
ytbCorpus <- Corpus(VectorSource(ytbText)) %>%
  tm_map(removeWords, stopwords(kind = 'en'))

# build document term matrix
dtmTopicModeling <- DocumentTermMatrix(ytbCorpus)

# adjust sparsity level (optional)
dtmTopicModeling <- removeSparseTerms(dtmTopicModeling, sparse=0.98)

# build TF-IDF matrix to remove unimportant terms based on co-occurrence characteristics
term_tfidf <- tapply(dtmTopicModeling$v/row_sums(dtmTopicModeling)
                     [dtmTopicModeling$i], dtmTopicModeling$j, mean) *
  log2(nDocs(dtmTopicModeling)/
         col_sums(dtmTopicModeling > 0 ))

# get median of term_tfidf and ommit frequent terms based on it.
median_tfidf <- summary(term_tfidf)[3]
dtmTopicModeling <- dtmTopicModeling[, term_tfidf >= median_tfidf]

# remove rows with zero entries
toRemove <- which(row_sums(dtmTopicModeling) == 0)
dtmTopicModeling <- dtmTopicModeling[row_sums(dtmTopicModeling) > 0,]

#define harmonic mean function
harmonicMean <- function(logLikelihoods, precision=2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods, prec = precision) + llMed))))
}

# LDA parameter definition
burnin = 1000
iter = 1000
keep = 50
sequ <- seq(2, 100, 10)

# run LDA model for each value of K
fitted_many <- lapply(sequ, function(k) LDA(dtmTopicModeling, k = k,
                                            method = "Gibbs",
                                            control = list(burnin = burnin, iter = iter, keep = keep)))

# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L) L@logLiks[
  -c(1:(burnin/keep))])

# compute the harmonic mean of log-likelihood values
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

# plot result (setting number of topics based on maximum harmonic mean)
plot(sequ, hm_many, type = "l")

# save number of topics to variable 'k'
k <- sequ[which.max(hm_many)]

# run final LDA model
seedNum <- 42
lda <- LDA(dtmTopicModeling, k = k, method = "Gibbs", control = list(
  burnin = burnin, iter = iter, keep = keep, seed = seedNum))

# look at top 10 terms in each topic
topTenTermsEachTopic <- terms(lda, 10)
View(topTenTermsEachTopic)

# take tweets from topic 1 (change number/s for other topics)
topicsProb <- topics(lda, 1)
topTenTermsEachTopic[,1]
topic1comments <- which(topicsProb == 1)

# delete removed rows from tweet corpus
tweetCorpus_LDA <- tweetCorpus[-toRemove]

# sample of five tweetes from topic 1
topics1YtbText <- as.list(tweetCorpus_LDA[topic1comments])
sampleComments <- sample(topics1YtbText, 5)
View(sampleComments)
