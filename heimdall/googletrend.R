library(gtrendsR)
##ONLY IF REQUIRED
#GOOGLE_USER<-"gaikwad.akshata79@gmail.com"
#GOOGLE_PASSWORD<-"8888293856akshata"

#devtools::install_github('diplodata/gtrendsR')
#GET 
#webt<-function(kw,loc){
#gconnect(usr=GOOGLE_USER,psw=GOOGLE_PASSWORD)
tempfun<-function(kw,loc="IN"){
  gall<<-gtrends(keyword=kw, geo =c("IN","PK"), time = "today 3-m", category = 0)
  gi<-gall$interest_over_time
  #n<-mean(gi$hits)+(sd(gi$hits))
  outs <- gi[order(-gi$hits),]
  outs <<- outs[1:6,]
  dateso<-data.frame(as.Date(outs$date))
  gc<-gall$interest_by_region
  #nr<-mean(gc$hits)+(sd(gc$hits))
  outsr <<- gc[1:5,1:4]
  outs$date<-as.Date(as.IDate(outs$date), origin="1970-01-01")
  outs$date <- as.character(outs$date) 
}
ret1<-function(){
  outs
}
ret2<-function(){
  outsr
}
webtp<-function(){
  plot(gall)
}
#webtpc<-function(){

# x<-c(as.String(drc$outsr.location[1]))
#  for(i in 2:length(drc$outsr.location))
# {
#  x<-append(x,as.String(drc$outsr.location[i]))
#}
#barplot(drc$outsr.hits,names.arg = x,col = rainbow(11))
#}
webtpc<-function(){
  rel<-as.data.frame(gall$related_queries)
  if(is.null(rel)){
    ret <- "No topics Found"
    ret
  }
  else{
  rel<-rel[1:7,1:3]
  rel
  }
}