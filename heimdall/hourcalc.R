hrcount<-function(x,y){
a<-searchTwitter(paste("from:",x,sep=''),n=y)
b<-twListToDF(a)
c<-b$created
d<-c()
e<-c()
for(i in (1:length(c))){
  .POSIXct(c[i])
  d[i]<-as.String(c[i])}
for(i in (1:length(d))){e[i]<-as.integer(substr(d[i],nchar(d[i])-7,nchar(d[i])-6))}
j<-c()
for(i in 1:24){j[i]<-0}
for(i in 1:length(e)){j[e[i]+1]<-j[e[i]+1]+1}
#for(i in 1:length(e)){j[(e[i]+1+5)%%24+1]<-j[(e[i]+1+5)%%24+1]+1}
plot(1:24,j,type='l',lwd=2,col='red',xlab="Time(IST)",ylab="Activity",main="User Activity Plot")
}