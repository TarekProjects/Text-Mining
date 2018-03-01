## Tarek Frahi 2016


library(tm)
library(ggplot2)
library(wordcloud)
library(GrapheR)
library(igraph)
library(topicmodels)
library(devtools)
library(curl)
library(stringr)

## Data Collection

id = c(consumer_key,consumer_secret,access_token,access_secret)

Tweets = function(id){
  require(twitteR)
  require(ROAuth)
  setup_twitter_oauth(id[1], id[2], id[3], id[4])
  tweets <- userTimeline('banquedefrance', n=3200)
  tweets.df <- twListToDF(tweets)
  write.csv2(tweets.df, 'tweets.csv', row.names = FALSE)
}

## Tweets Corpus

Corpus_Tweets <- function(x){
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
  removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
  replaceWord <- function(corpus, oldword, newword){tm_map(corpus, content_transformer(gsub),pattern=oldword, replacement=newword)}
  myStopwords <- c(stopwords('french'), stopwords("en"),"mensuelles","assez","ème","plus","non","faut","dt","l","d","encore","the","of","a","noyer","septembre","octobre","novembre","décembre","janvier","février","mars","avril","mai","juin","juillet","août","false","true","href","follow","relnofollowhootsuite","relnofollowtwitter")
  myCorpus <- Corpus(VectorSource(x))
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
  myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
  myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
  myCorpus <- replaceWord(myCorpus, "villeroydegalhau", "Galhau")
  myCorpus <- replaceWord(myCorpus, "banquedefrance", "BDF")
  return(myCorpus)
}

## Key words

Keys = function(x){
  tdm <- TermDocumentMatrix(x, control = list(wordLengths = c(2, Inf)))
  term.freq <- rowSums(as.matrix(tdm))
  term.freq <- sort(subset(term.freq, term.freq >= 50), decreasing = T)
  df <- data.frame(term = names(term.freq), freq = term.freq)
  h <- head(df,10)
  g = ggplot(h, aes(x=term, y=freq)) + geom_bar(stat="identity") + xlab("Termes") + ylab("Fréquences")+ theme(axis.text=element_text(size=7))
  w = wordcloud(words = df$term, freq = df$freq, scale = c(3,0.2), min.freq = 50, rot.per = 0, random.order = F, colors = brewer.pal(9, "BuGn")[-(1:4)])
  print(g)
  print(w)
  return(list(df,h,g,w))
}

## correlations

Topics = function(x){
  y <- Corpus_Tweets(x)
  dtm <- DocumentTermMatrix(y)
  rowTotals <- apply(dtm , 1, sum)
  dtm <- dtm[rowTotals> 0, ]
  lda <- LDA(dtm, k = 3)
  term <- terms(lda, 3)
  term <- apply(term, MARGIN = 1, paste, collapse = ", ")
  topics <- topics(lda)
  topics <- data.frame(date=as.Date(x$created), topic=topics)
  f = qplot(date, ..count.., data=topics, geom="density", fill=term[topic])
  print(f)
  return(list(topics,f))
}

## hashtags

Hashtags = function(x){
  hashtags = str_extract_all(x, "#\\w+")
  hashtags = unlist(hashtags)
  hashtags = gsub("#VilleroydeGalhau", "#Galhau", hashtags)
  hashtags = gsub("#StatInfo", "#Stinfo", hashtags)
  tags = table(hashtags)
  w = wordcloud(names(tags), tags, random.order=FALSE, colors=brewer.pal(9, "BuGn")[-(1:4)])
  hashtags = as.data.frame(tags)
  hashtags = hashtags[order(hashtags[,2],decreasing=TRUE),]
  colnames(hashtags) = c("hashtags","freq")
  rownames(hashtags) = NULL
  hashtags = head(hashtags,5)
  g = ggplot(hashtags, aes(x=hashtags, y=freq)) + geom_bar(stat="identity") + xlab("Tags") + ylab("Fréquences")+ theme(axis.text=element_text(size=7))
  return(list(hashtags,g,w))
}

## retweets

RT = function(x){
  rt = subset(x, x$retweetCount >= 10)
  dates <- strptime(x$created, format="%Y-%m-%d")
  par(bg="white", mar=c(1,1,1,1))
  g = ggplot(x, aes(x=strptime(x$created, format="%Y-%m-%d"), y=x$retweetCount)) + geom_bar(stat="identity") +xlab("") + ylab("Nombre de RT") + theme(axis.text=element_text(size=7))
  return(list(rt,g))
}

## likes

Fav = function(x){
  fav = subset(x, x$favoriteCount >= 10)
  dates <- strptime(x$created, format="%Y-%m-%d")
  par(bg="white", mar=c(1,1,1,1))
  g = ggplot(x, aes(x=strptime(x$created, format="%Y-%m-%d"), y=x$favoriteCount)) + geom_bar(stat="identity") +xlab("") + ylab("Nombre de likes") + theme(axis.text=element_text(size=7))
  return(list(fav,g))
}

## maping

InterRt = function(x){
  txt = sapply(x, function(x) x$getText())
  rt = grep("(RT|via)((?:\\b\\W*@\\w+)+)", txt, ignore.case=TRUE)
  who_retweet = as.list(1:length(rt))
  who_post = as.list(1:length(rt))
  for (i in 1:length(rt)){ 
    twit = tweets1[[rt[i]]]
    poster = str_extract_all(twit$getText(),"(RT|via)((?:\\b\\W*@\\w+)+)") 
    poster = gsub(":", "", unlist(poster)) 
    who_post[[i]] = gsub("(RT @|via @)", "", poster, ignore.case=TRUE) 
    who_retweet[[i]] = rep(twit$getScreenName(), length(poster)) 
  }
  who_post = unlist(who_post)
  who_retweet = unlist(who_retweet)
  rt_graph = graph.edgelist(retweeter_poster)
  ver_labs = get.vertex.attribute(rt_graph, "name", index=V(rt_graph))
  glay = layout.fruchterman.reingold(rt_graph)
  par(bg="white", mar=c(1,1,1,1))
  plot(rt_graph,
       layout=glay,
       vertex.color= "gray",
       vertex.frame.color= "gray",
       vertex.size=5,
       vertex.label=ver_labs,
       vertex.label.family="mono",
       vertex.label.color= "black",
       vertex.label.cex=0.6,
       edge.arrow.size=0.5,
       edge.arrow.width=0.5,
       edge.width=3,
       edge.color= brewer.pal(9, "BuGn")[-(1:2)])
  title("\n Retweets", cex.main=1, col.main="black", family="mono")
}
              