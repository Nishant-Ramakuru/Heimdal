library("rworldmap")
fas<-function(text){
  findAssocs(tdm,text,0.2)
}


insu<-function(text){
  friends <- text$getFriends() 
  followers <- text$getFollowers()
  }
mspg<-function(names,num)
{
  user<-getUser(names)
  c<-retweeters(tf.df$id[num])
  l<-length(c)
  f<<-user$getFollowers()
  #sum<-0
  i<<- 1
  d<-length(f)
  #for(i in 1:c){
   # c<-getUser(f[i])
  #  sum<-sum+length(c$getFollowers())
  #}
  #c<-retweeters(tf.df$id[num])
  #l<-length(c)
  ls<-l*44*d
  paste("This message effectively reached ",ls," people")
}


############ TO GET THE LOCATION OF NEGATIVE TWEETS ##################

negma<-function(){
  n<-length(ftdf$Postedby)
  c<-n-1
  i<-1
  for(i in 1:c){
    u<-getUser(ftdf$Postedby[i])
    loc<-u$location
    if(loc==""){
      ftdf$lng[i]<-361
      ftdf$ltt[i]<-361
    }
    else{
      longlat(loc)
      ftdf$lng[i]<-longg
      ftdf$ltt[i]<-latt
    }
  }
  
  #newmap <- getMap(resolution = "low")
  #dfm <- tf.df[complete.cases(tf.df[,15:16]),]
ftdf
    #plot(newmap, xlim = c(80,85), ylim = c(7,35), asp = 1)
  #points(ftdf$lng, ftdf$ltt, col = "red", cex = 1)
}


##################################################################
getorg<-function(){
  l<-length(utf.df$text)
  for(i in 1:11){
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
  