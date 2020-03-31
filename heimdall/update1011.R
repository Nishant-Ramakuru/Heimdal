library(gtrendsR)

youtubetrd<-function(kw,loc="IN"){
  ygall<<-gtrends(keyword="kashmir", geo ="IN", time ="2017-08-01 2017-10-01", category = 0,gprop = "youtube")
  gi<-ygall$interest_over_time
  n<-mean(gi$hits)+(sd(gi$hits))
  outs <- subset(gi,gi$hits>n)
  dateso<-data.frame(as.Date(outs$date))
  gc<-ygall$interest_by_region
  nr<-mean(gc$hits)+(sd(gc$hits))
  outsr <- subset(gc,gc$hits>nr)
  outs$date<-as.Date(as.IDate(outs$date), origin="1970-01-01")
  outs$date <- as.character(outs$date) 
  yddte<<-data.frame(outs$date,outs$hits,outs$geo)
  ydrc<<-data.frame(outsr$location,outsr$hits,outsr$geo)
}
youtubedt<-function(){
  fin<-as.data.frame(yddte)
  fin
}
youtubeloc<-function(){
  ydrc
}
youtubefr<-function(){
  plot(ygall)
}

youtubeqwr<-function(){
  rel<-as.data.frame(ygall$related_queries)
  rel<-rel[1:7,1:3]
  rel
}

youtubetop<-function(){
  rel<-as.data.frame(gall$related_topics)
  rel<-rel[1:7,1:3]
  rel
}



closest<-function(lat,long){
  df<-data.frame(closestTrendLocations(lat,long))
  df
}

availtrends<-function(place){
  acun<-data.frame(availableTrendLocations())
  filt<<-acun[acun$country==place,]
  filt
}

trends<-function(placen){
  woe<-filt$woeid[filt$name==placen]
  altrends<-data.frame(getTrends(woe))
  altrends
}





###tpoic


mytrends<-function(num=1000 ,lat,long,rad="500km"){
  gc<-paste(lat,long,rad,sep=",")
  my<-searchTwitteR("-RT",resultType = "recent",n=num,geocode = gc)
  mytweets<-twListToDF(my)
  myCorpus <- Corpus(VectorSource(mytweets$text))
  #remove url
  removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
  myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
  # remove anything other than English letters or space
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
  myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
  # remove stopwords
  myStopwords <- c(setdiff(stopwords('english'), c("rt","big","i","u")))
  myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
  # remove extra whitespace
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  # keep a copy for stem completion later
  myCorpusCopy <- myCorpus
  
  myCorpus <- tm_map(myCorpus, stemDocument) # stem words
  #writeLines(strwrap(myCorpus[[6]]$content, 60))
  
  ## r refer card data mine now provid link packag cran packag
  ## mapreduc hadoop ad
  #stemCompletion2 <- function(x, dictionary) {
  # x <- unlist(strsplit(as.character(x), " "))
  # x <- x[x != ""]
  # x <- stemCompletion(x, dictionary=dictionary)
  # x <- paste(x, sep="", collapse=" ")
  # PlainTextDocument(stripWhitespace(x))
  #}
  #myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
  #myCorpus <- Corpus(VectorSource(myCorpus))
  #writeLines(strwrap(myCorpus$content[[1]], 60))
  
  
  ####              Building term document matrix      ########################
  tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))
  #(freq.terms <- findFreqTerms(tdm, lowfreq = 40))
  term.freq <- rowSums(as.matrix(tdm))
  term.freq <- subset(term.freq, term.freq >= 20)
  mytwtdf <- data.frame(term = names(term.freq), freq = term.freq)
  
  dtm <- as.DocumentTermMatrix(tdm)
  rowTotals <- apply(dtm , 1, sum)
  #dtm=dtm[raw.sum!=0,]
  dtm.new   <- dtm[rowTotals> 0, ] 
  lda <- LDA(dtm.new, k = 5) # find 8 topics
  term <- terms(lda, 5)
  topic <- topics(lda, 1)
  #term <- apply(term, MARGIN = 2, paste, collapse = ", ")
  #topics <- data.frame(date=as.IDate(mytweets$created), topic)
  #topics <- data.frame(date=(mytweets$created), topic)
  #qplot(date, ..count.., data=topics, geom="density",
  #      fill=term[topic], position="stack")
  topdf<-data.frame(terms(lda,5))
  topdf
}




##news


newstrd<-function(kw,loc="IN"){
  ngall<<-gtrends(keyword="kashmir", geo ="IN", time ="2017-08-01 2017-10-01", category = 0,gprop = "news")
  gi<-ngall$interest_over_time
  n<-mean(gi$hits)+(sd(gi$hits))
  outs <- subset(gi,gi$hits>n)
  dateso<-data.frame(as.Date(outs$date))
  gc<-ngall$interest_by_region
  nr<-mean(gc$hits)+(sd(gc$hits))
  outsr <- subset(gc,gc$hits>nr)
  outs$date<-as.Date(as.IDate(outs$date), origin="1970-01-01")
  outs$date <- as.character(outs$date) 
  nddte<<-data.frame(outs$date,outs$hits,outs$geo)
  ndrc<<-data.frame(outsr$location,outsr$hits,outsr$geo)
}
newsdt<-function(){
  fin<-as.data.frame(nddte)
  fin
}
newsloc<-function(){
  ndrc
}
newsfr<-function(){
  plot(ngall)
}
#webtpc<-function(){

# x<-c(as.String(drc$outsr.location[1]))
#  for(i in 2:length(drc$outsr.location))oz  
# {
#  x<-append(x,as.String(drc$outsr.location[i]))
#}
#barplot(drc$outsr.hits,names.arg = x,col = rainbow(11))
#}
newsqwr<-function(){
  rel<-as.data.frame(gall$related_queries)
  rel<-rel[1:7,1:3]
  rel
}

newstop<-function(){
  rel<-as.data.frame(gall$related_topics)
  rel<-rel[1:7,1:3]
  rel
}


