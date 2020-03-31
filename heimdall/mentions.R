 sidesearch<-function(uname,session){
 mnm<-paste(uname,"-fr",sep="")
 unm<-paste("@",uname,sep="")
 upun<-paste(unm,"-RT")
 m<-searchTwitter(searchString=upun,n=100,resultType = "recent")
 if(length(m)==0)
 {
   print("ret")
   return()}
 
 
 m<-twListToDF(m)
 len<-length(m$text)
 
 
 check<-c("meet","met","with","at","see","saw","saath","dekh","dekha")
 m$responsive<-0
 for(i in 1:len){
   ckr<-str_detect(m$text[i],check)
   if(TRUE %in% ckr == TRUE)
   {m$responsive[i]<-1}
 }
 print(m$responsive)
 m<-m[m$responsive==1,]
 m$lat<-0
 m$long<-0
 m$radius<-0
 m[,2:4]<-NULL
 m[,3:7]<-NULL
 m[,4:9]<-NULL
 m$exec<-0
 m<-m[m$screenName!=uname,]
 m<-m[!duplicated(m$screenName),]
 len<-length(m$text)
 for(i in 1:len){
   check<-c(" meet "," met "," with "," at "," see "," saw "," saath "," dekh "," dekha ")
   ckr<-str_detect(m$text[i],check)
   m$exec[i][TRUE %in% ckr == TRUE]<-1
 }
 m<-m[m$exec==1,]
 
 prog1<-Progress$new(session, min = 0, max = length(m$text))

 prog2<-Progress$new(session, min = 0, max = 2100)
for( i in 1:length(m$text)){
  m$text=gsub(",","",m$text[i])
  frlat<<-0
  frlong<<-0
  frradi<<-0
  prog1$inc(amount = 1, message = "Current Search", detail = m$screenName[i])
  prog2$set(value = 0, message = "Searching ", detail = m$screenName[i])
  locgenfr(m$screenName[i],prog2)
  urn<-m$screenName[i]
  m$lat[m$screenName == urn]<-frlat
  m$long[m$screenName == urn]<-frlong
  m$radius[m$screenName == urn]<-frradi
 
  m$created[i]<-.POSIXct(m$created[i])
  
}
 prog2$close()
 prog1$close()
   df <- data.frame(m)
   m<-m[m$lat==0,]
   m<-m[m$long==0,]
   m<-m[m$radius==0,]
      namec<-paste("data/",mnm,".csv",sep = "")
   write.csv(df,file=namec,row.names = F)

 }