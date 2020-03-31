library(gtrendsR)
##ONLY IF REQUIRED
#GOOGLE_USER<-"gaikwad.akshata79@gmail.com"
#GOOGLE_PASSWORD<-"8888293856akshata"


#GET 
#webt<-function(kw,loc){
#gconnect(usr=GOOGLE_USER,psw=GOOGLE_PASSWORD)
tempfun<-function(kw,loc="IN"){
  gall<<-gtrends(keyword=kw, geo =loc, time = "today 1-m", category = 0)
  gi<-gall$interest_over_time
  n<-mean(gi$hits)+(sd(gi$hits))
  outs <- subset(gi,gi$hits>n)
  dateso<-data.frame(as.Date(outs$date))
  gc<-gall$interest_by_region
  nr<-mean(gc$hits)+(sd(gc$hits))
  outsr <- subset(gc,gc$hits>nr)
  outs$date<-as.Date(as.IDate(outs$date), origin="1970-01-01")
  outs$date <- as.character(outs$date) 
  ddte<<-data.frame(outs$date,outs$hits,outs$geo)
  drc<<-data.frame(outsr$location,outsr$hits,outsr$geo)
}
ret1<-function(){
  fin<-as.data.frame(ddte)
  fin}
ret2<-function(){
  drc
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
  rel<-rel[1:7,1:3]
  rel
}