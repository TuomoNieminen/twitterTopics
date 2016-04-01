# Get max tweets (3200) from user

source("twitter_functions.R")

tweeter <- "alexstubb"

filepath <- paste0(tweeter,"_tweets.Rda")
oauth_path <- "credentials/my_oauth"


# access API

consumerKey <- ""
consumerSecret <- ""

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

my_oauth <- OAuthFactory$new(consumerKey=consumerKey, consumerSecret=consumerSecret, 
                             requestURL=requestURL, accessURL=accessURL, authURL=authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(file=oauth_path,my_oauth)

# get tweets and save them to a JSON file

jsonpath <- paste0(tweeter,".json")
getTimeline(filename=jsonpath,
                    screen_name=tweeter,
                    oauth_folder="credentials")

# make a data frame using the JSON file
# you have to first change file encoding (using notepad++ for example, convert to "utf8 not BOM")

tw <- parseTweets(jsonpath)
tw <- tw[!duplicated(tw$id_str),]

# how many did we get?
nrow(tw)
