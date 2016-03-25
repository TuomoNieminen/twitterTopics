# get data from a twitter user and fit topic models on it.
# 26.3.2016
# tuomo.a.nieminen@gmail.com

source("twitter_functions.R")

tweeter <- "alexstubb"
timezone <- 2
tweeter_languages <- c("english","finnish","swedish")

filepath <- paste0(tweeter,"_tweets.Rda")

# oauth session
twitter_oauth()

# use a wrapper for twitteR library to get some data
tw = super_search(search = "userTimeline",
                  args=list(user=tweeter, n = 20))
tw = twListToDF(tw)

# remove duplicate tweets
tw$id <- as.numeric(tw$id)
tw <- tw[!duplicated(tw$id),]

# change timezone
tw$created <- tw$created + timezone*60*60
tw$tunti <- as.numeric(format(tw$created,"%H"))

# define popularity as a sum of favorites and retweets
tw$suosio <- tw$favoriteCount + tw$retweetCount

# clean the tweets and add language variable
tw$tweet <- clean_text(tw$text)
tw <- tw[tw$tweet !="",]
tw$lang <- get_languages(tw$tweet, tweeter_languages)
tw <- tw[!is.na(tw$lang),]

#how many did we get?
ntweets <- nrow(tw)
ntweets

# # save to file
# save(file=filepath,tw)
# 
# #load data
# tw <- get(load(filepath))

# some basic stats
mindate <- format(min(tw$created),"%d.%m.%Y")
maxdate <- format(max(tw$created),"%d.%m.%Y")
ntweets <- nrow(tw)
sink(file=paste0(tweeter,"basic_stats.txt"))
ntweets
mindate
maxdate
sink()

  # prepare data for topic modelling

# separate by language
TW <- lapply(tweeter_languages, function(l) {
  tw[tw$lang==l,]})

names(TW) <- tweeter_languages

# get document term matrix by language
DTM <- lapply(TW, function(tw) {
  get_documentTermMatrix(tw$tweet)})
save(file=paste0(tweeter,"_DTM.Rda"),DTM)

# get tfidf and remove most used words
TFIDF <- lapply(DTM, function(dtm) {
  tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
    log2(nDocs(dtm)/col_sums(dtm > 0))
})

cat("term frequencies before cuts: \n")
lapply(DTM,function(dtm) summary(col_sums(dtm)))

#use 0.1 quantile as tfidf cutoff
DTM2 <- list()
temp <- mapply(function(dtm,tfidf,lang) {
  cutoff <- quantile(tfidf, 0.1)
  dtm <- dtm[,tfidf >= cutoff]
  dtm <- dtm[,col_sums(dtm)>1]
  DTM2[[lang]] <<- dtm[row_sums(dtm) > 0,]
  return()
},DTM,TFIDF,tweeter_languages)

cat("term frequencies after cuts: \n")
lapply(DTM2,function(dtm) summary(col_sums(dtm)))


# fit some topic models (separately for each language)
k <- 4
TM <- lapply(DTM2, function(dtm) {
  list(
    VEM = LDA(dtm, k = k),
    Gibbs = LDA(dtm, k = k, method = "Gibbs",
                control = list(burnin = 1000,thin = 10, iter = 10000)),
    CTM = CTM(dtm, k = k)
  )
})

# for each language, get the model with lowest entropy
ENTR <- lapply(TM, function(tm) {
  sapply(tm, function(x) {
    mean(apply(posterior(x)$topics,
               1, function(z) - sum(z * log(z))))
  })})
model_choice <- sapply(ENTR, function(entr) {
  names(entr[entr==min(entr)])
})

# keep only the models with lowest entropy
M <- list()
for(i in 1:length(TM)) {
  M[i] <- TM[[i]][[model_choice[i]]]
}
names(M) <- tweeter_languages

cat("model choices: \n")
model_choice

# get the topic distributions
TOPICS <- lapply(M,topics)

cat("document topic distributions")
lapply(TOPICS, table)

# get the most frequent terms by topic
for(i in 1:length(M)) {
  trms <- terms(M[[i]],40)
  write.csv(file=paste0(tweeter,"_top_terms_by_topics_",
                        names(M[i]),".csv"),
            row.names=F,
            trms)
}


# label the tweets and add frequencies of most frequent topic (posteriors)
for(i in 1:length(TW)) {
  tpcs <- TOPICS[[i]]
  havelabels <- as.numeric(names(tpcs))
  TW[[i]]$topic <- NA
  TW[[i]][havelabels,]$topic <- tpcs
  posteriors <- posterior(M[[i]])$topics
  TW[[i]]["P_max"] <- NA
  TW[[i]][havelabels,]["P_max"] <- apply(posteriors,1,max)
  
  for(j in 1:k) {
    colname <- paste0("P_topic",j)
    TW[[i]][colname] <- NA
    TW[[i]][havelabels,][colname] <- posteriors[,j]
  }
}

# get 10 'most representative' tweets from each topic
for(lang in tweeter_languages) {
  tw <- TW[[lang]]
  tw <- tw[!is.na(tw$topic),]
  path <- paste0(tweeter,"_topicsample_",lang,"_test.csv")
  write(file=path,paste("Top 10",lang,"tweets by topic \n"))
  write(file=path,"Topic distributions: \n",append=T)
  write.table(file=path,table(TOPICS[[lang]]),
              row.names=F,col.names=c("Topic","Freq"),append=T)
  for(t in sort(unique(tw$topic))) {
    tw_sub <- tw[tw$topic==t,]
    tw_sub <- tw_sub[with(tw_sub,order(P_max,decreasing=T)),]
    s <- tw_sub$text[1:min(10,nrow(tw_sub))]
    write(file=path,paste("\n ### TOPIC",t,"### \n"),append=T)
    j <- 1
    for(tweet in s) {
      max_post <- round(tw_sub$P_max[j],2)
      write(file=path,paste0("P(T",t,")=",max_post," ",tweet," \n "),append=T)
      j <- j + 1
    }
  }
}

# save all data frames as a list
save(file=paste0(tweeter,"_topic_data.Rda"),TW)



