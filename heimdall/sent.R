#################FINDING WORD CORELATIONS######################################

corf<-function(){
  plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T)
}

#!!! TOO SMALL GOD DAMN IT ###


######################### CLUSTERING ##########################################
clg<-function(){
  tdm2 <- removeSparseTerms(tdm, sparse = 0.9999)
  m2 <- as.matrix(tdm2)
  # cluster terms
  distMatrix <- dist(scale(m2))
  fit <- hclust(distMatrix, method = "ward.D2")
  plot(fit)
  rect.hclust(fit, k = 6)
}
clt<-function(){
  tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
  m2 <- as.matrix(tdm2)
  # cluster terms
  distMatrix <- dist(scale(m2))
  fit <- hclust(distMatrix, method = "ward")
  m3 <- t(m2) # transpose the matrix to cluster documents (tweets)
  set.seed(122) # set a fixed random seed
  k <- 6 # number of clusters
  kmeansResult <- kmeans(m3, k)
  round(kmeansResult$centers, digits = 3) # cluster centers
  for (i in 1:k) {
    cat(paste("cluster ", i, ": ", sep = ""))
    s <- sort(kmeansResult$centers[i, ], decreasing = T)
    cat(names(s)[1:5], "\n")
    # print the tweets of every cluster
    # print(tweets[which(kmeansResult$cluster==i)])
  }
}

##############################################################

################## TOPIC MODELLING ############################

tm<-function(){
  dtm <- as.DocumentTermMatrix(tdm)
  lda <- LDA(dtm, k = 2) # find 8 topics
  (term <- terms(lda, 3))
  topic <- topics(lda, 1)
  topics <- data.frame(date=as.IDate(tf.df$created), topic)
  qplot(date, ..count.., data=topics, geom="density",
        fill=term[topic], position="stack")
}

############################################################3

##############  PLOT TOP FRQUENCY WORDS ##################### 
tfq<-function(){
  ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +xlab("Terms") + ylab("Count") + coord_flip() +theme(axis.text=element_text(size=7))
} 

############################################################

####ONLY FOR SINGLE USER #####

##################### USER SENTIMENT OVER TIME ###################################
usv<-function(user1){
  #sentiments <- sentiment(utf.df$text)
  #table(sentiments$polarity)
  #sentiments$score <- 0
  #sentiments$score[sentiments$polarity == "positive"] <- 1
  #sentiments$score[sentiments$polarity == "negative"] <- -1
  #sentiments$date <- as.IDate(utf.df$created)
  utf(user1,100)
  utf.df$text <- sapply(utf.df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
  mysent<-get_nrc_sentiment(utf.df$text)
  tw2 <<- cbind(utf.df, mysent)
  tw2$negative<-tw2$negative*-1
  tw2$avg<-((tw2$negative+tw2$positive)/2)
  tw2$date<-as.IDate(tw2$created)
  result <- aggregate(avg ~ date, data = tw2, sum)
  plot(result, type = "l")
}

##################################################################################


############# USING THE NRC SENTIMENT PACKAGE #################################
gnrc<-function(){
  
  tf$Text <- sapply(tf.df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))  
  mysent<-get_nrc_sentiment(tf$Text)
  tweets2 <<- cbind(tf$Text, mysent)
  sentimentTotals <- data.frame(colSums(tweets2[,c(2:11)]))
  names(sentimentTotals) <- "count"
  sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
  rownames(sentimentTotals) <- NULL
 sentimentTotals
   # ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  
  #  geom_bar(aes(fill = sentiment), stat = "identity") +
   # theme(legend.position = "none") +
  #  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")
  
}
###############################################################################################

######################## SORTING OUT THE USEFUL TWEETS ########################################
avgt<-function(){
  #### ERROR HERE PLEASE CHECK #######
  #sentiments <- sentiment(utf.df$text)
  #tf<-cbind(tf,sentiments$polarity)
  #tf<-as.data.frame(tf[tf$`sentiments$polarity` == "negative",])
  tf$Post <- sapply(tf$Post,function(row) iconv(row, "latin1", "ASCII", sub=""))
  mysent<-get_nrc_sentiment(tf$Post)
  tw3 <- cbind(tf, mysent)
  tw3$negative<-tw3$negative*-1
  tw3$avg<-((tw3$negative+tw3$positive)/2)
  ftdf<-as.data.frame(cbind(tf,tw3$avg))
  #ftdf<<-ftdf[ftdf$`tw2$avg` < 0,]
  ftdf
}

addcolumn<-function(df){
  df$Post <- sapply(df$Post,function(row) iconv(row, "latin1", "ASCII", sub=""))
  print(df$Post)
  mysent<-get_nrc_sentiment(df$Post)
  tw3 <- cbind(df, mysent)
  tw3$negative<-tw3$negative*-1
  tw3$avg<-((tw3$negative+tw3$positive)/2)
  ytdf<-as.data.frame(cbind(df,tw3$avg))
  
  #ytdf<-ftdf[ftdf$`tw2$avg` < 0,]
  ytdf
}
###############################################################################################
avgtmap<-function(){
  #### ERROR HERE PLEASE CHECK #######
  #sentiments <- sentiment(utf.df$text)
  #tf<-cbind(tf,sentiments$polarity)
  #tf<-as.data.frame(tf[tf$`sentiments$polarity` == "negative",])
  tf$Post <- sapply(tf$Post,function(row) iconv(row, "latin1", "ASCII", sub=""))
  mysent<-get_nrc_sentiment(tf$Post)
  tw2 <<- cbind(tf, mysent)
  tw2$negative<-tw2$negative*-1
  tw2$avg<-((tw2$negative+tw2$positive)/2)
  ftdf<<-as.data.frame(cbind(tf,tw2$avg))
}


