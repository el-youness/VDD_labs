library(twitteR)

#-----------------------------------------------------------------------
# Connexion to Twitter
#-----------------------------------------------------------------------

api_key <- "YOUR API KEY"
api_secret <- "YOUR API SECRET"
access_token <- "YOUR ACCESS TOKEN"
access_token_secret <- "YOUR ACCESS TOKEN SECRET"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret) # authentification

#-----------------------------------------------------------------------
# Retrieve the tweets and save into a csv file
#-----------------------------------------------------------------------
processedWord="#paris"
nbTweets=100
filename="tweetParis.csv"
tweets <-searchTwitter(processedWord,n=nbTweets) # retrieve tweets
tweets.df <- twListToDF(tweets) # transform tweets in a dataframe
write.csv(tweets.df,file=filename,row.names=F) # write the dataframe in a csv file
mydata <- read.csv(filename) # read the csv file