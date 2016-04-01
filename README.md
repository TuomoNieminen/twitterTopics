# twitterTopics
Example on how to retrieve data from twitter and do topic model analysis on it.

analyze_twitter_user shows an example on how to fit some topic models on data retrieved from twitter. The data collection is  based on the twitteR package in R. The get_max_tweets script is able to retrieve more data and shows an example o how to download the maxixmum amoun of tweets from a user timeline. It is based on the ["https://github.com/SMAPPNYU/smappR/"](smappR) package.

In order to 'do it yourself' you need access to the twitter API and for that you need to create an twitter app (with your twitter account). 
For that go to https://apps.twitter.com/ 

You might also find this tutorial useful: http://thinktostart.com/twitter-authentification-with-r/

The fi_stopwords.Rda file includes finnish stopwords that are retrieved from http://www.nettiapina.fi/finnish-stopword-list/ 
with some manual additions. It is used in cleaning the data to create before fitting the topic models.
