# twitterTopics
Example on how to retrieve data from twitter and do topic model analysis on it.

The `analyze_twitter_user` R script shows an example on how to fit some topic models on data retrieved from twitter. The data collection is based on the twitteR package. The `get_max_tweets` script however shows a better method for the data collection and it contains an example on how to download the maximum amount of tweets (~3200) from a user timeline. It is based on the [smappR]("https://github.com/SMAPPNYU/smappR/") package.

In order to 'do it yourself' you need access to the twitter API and for that you need to create a twitter app (with your twitter account). For that go [here]("https://apps.twitter.com/"). You might also find [this tutorial]("http://thinktostart.com/twitter-authentification-with-r/") useful.

The `fi_stopwords` file includes finnish stopwords that are retrieved from [her]("http://www.nettiapina.fi/finnish-stopword-list/") with some manual additions. It is used in cleaning the vocabulary before fitting the topic models.
