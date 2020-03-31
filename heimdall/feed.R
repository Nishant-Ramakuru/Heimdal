library(feedeR)
get_feed<-function(ur,key="no"){
  d<-feed.extract("https://feeds.feedburner.com/RBloggers")
  it<-d$items
  feed<-data.frame(date,title)
  if(key=="no")
  {
    feed$title
  }
  else {
    l<-length(feed$title)
    for(i in 1:l){
      pos = regexpr(key, feed$title[i])
      if(pos != -1){
        print(feed$title[i])
      }
    }
    
  }
}

