#######      ALL THE REQUIRED PACKAGES   ############
require(devtools)
library(stringr)
library(ROAuth)
library(streamR)
#library(sentiment)
library(RJSONIO)
library(topicmodels)
library(twitteR)
library(tm)
library(ggplot2)
library(RCurl)
library(tm)
library(XML)
library(syuzhet)
library("ggplot2", lib.loc="~/R/win-library/3.4")
library("data.table", lib.loc="~/R/win-library/3.4")
# Declare Twitter API Credentials
auth<-function(num){
  a<-read.csv("data/tokens.csv",stringsAsFactors = F)
  setup_twitter_oauth(a$apikey[num],a$apisecret[num],a$apitoken[num],a$apitokensecret[num])
  
  }
#to get cco-ordinates
#Returns longitute and lattitutegetgeo<-function(addrs){
longlat <- function(addrs) {
  longlat1 <- function(addr) {
    # Attempts to retrieve longitude and latitude from place name
    url = paste0("http://maps.google.com/maps/api/geocode/xml?address=", 
                 addr, "&sensor=false")
    doc <- NA
    r <- c(NA, NA)
    try(doc <- xmlTreeParse(url), silent = T)
    if (!is.na(doc[1])) {
      root = xmlRoot(doc)
      long <<- xmlValue(root[["result"]][["geometry"]][["location"]][["lng"]])
      lat<<-xmlValue(root[["result"]][["geometry"]][["location"]][["lat"]])
      r <- c(long, lat)
      #st<<-cat(lat, ',',long,sep='')
    } else {
      print(paste("Error: Could not find", addr))
    }
    return(as.numeric(r))
  }
  lato <<- longlat1(addrs[1])
  if (length(addrs) > 1) {
    for (i in 2:length(addrs)) {
      lato <<- rbind(l, longlat1(addrs[i]))
    }
  }
  latt<<-paste(lat)
  longg<<-paste(long)
  
  return(lato)
  }

#return lato
#tweets search - insert NULL if no argument , do not leave blank
srch<-function(text,num=300,geotext="text2",sinc=NULL,unt=NULL){
  longlat(geotext)
  goc<-paste(lat,",",long,",500mi",sep="")
  tweets_geolocated <- searchTwitter(text, n=num, lang="en", geocode=goc, since=sinc,until=unt)
  tf.df <<- twListToDF(tweets_geolocated)
  tf.df$sss<-"Twitter"
  tf<-cbind(tf.df$text,tf.df$sss,tf.df$screenName,tf.df$created,geotext)
  colnames(tf)<-c("Post","Source","Postedby","Time","Location")
  tf<<-as.data.frame(tf)
  tf
}

#tweets on a users timeline
utf<-function(name,num){
  tf<-userTimeline(name,n=num)
  utf.df<<-twListToDF(tf)
  getorg()
  userdisp()
}
getorg<-function(){
  l<-length(utf.df$text)
  for(i in 1:l){
    pos=regexpr('//',utf.df$statusSource[i])
    pos2=regexpr('rel',utf.df$statusSource[i])
    pos3=regexpr("android",utf.df$statusSource[i])
    pos4=regexpr("iphone",utf.df$statusSource[i])
    pos5=regexpr("ipad",utf.df$statusSource[i])
    if(pos3!=-1)
    {
      utf.df$ss[i]<<-"twitter for android"
    }
    else if(pos4!=-1)
    {
      utf.df$ss[i]<<-"twitter for Iphone"
    }
    else if(pos5!=-1)
    {
      utf.df$ss[i]<<-"twitter for Ipad"
    }
    else
    {
      utf.df$ss[i]<<-substr(utf.df$statusSource,pos+2,pos2-3)
    }
  }
} 
userdisp<-function(){
  utfr<-data.frame(utf.df$text,utf.df$created,utf.df$ss)
  colnames(utfr)<-c("text","Date","source")
  utfr<-data.frame(utfr)
  utfr
}



