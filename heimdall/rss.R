library(feedeR)
library(stringr)
library(dplyr)
urls<-c("http://rss.cnn.com/rss/edition.rss",
        "http://rss.cnn.com/rss/edition.rss")

get_feed<-function(key=""){
  d<-feed.extract(urls[1])
  it<-d$items
  if(key=="")
  {
    it$title
  }
  else {
    print(key)
    l<-length(it$title)
    for(i in 1:l){
    pos = regexpr(key, it$title[i])
    if(pos <= -1){
     it$title[i]
      
     }
    }
    
  }
}

