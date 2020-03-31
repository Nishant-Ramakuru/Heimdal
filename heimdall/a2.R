flat<-0
flong<-0
fradi<-0

locgen<-function(uname,progbar){ 
  urn<<-uname
  un<-paste("from:",uname,sep='')
  progbar$set(value = 0, message = paste("Searching",uname), detail = NULL)
  frame<-newSearchTwitter(un,1,"recent","22.5391,80.1172,1250mi")
  if(length(frame)!=0){
    searchatthis("22.5391","80.1172","2100km",un,progbar)
  }
  else{
  progbar$inc(amount = 0, message = paste("Searching",uname), detail = "Not Found")
    Sys.sleep(1)
  }
  }
searchatthis<-function(lat,long,radius,un,progbar)
{
  flat<<- lat
  flong<<-long
  radi<-sub("km","",radius)
  radi<-as.integer(radi)
  fradi<<-radi
  print(paste(flat,flong,fradi))
 
  progbar$set(value = 2100-radi, message = paste("Optimising ",urn), detail = paste(radi,"km"))
  
  if(radi>10)
  {
    if(radi>150)
    {
      system(paste("java -jar CircleDivide.jar ",lat," ",long," ",radi," new.csv 4"))
    }
    else
      {
        system(paste("java -jar CircleDivide.jar ",lat," ",long," ",radi," new.csv 9")) 
    }
    iter1<-read.csv("new.csv")
    for(i in 1:length(iter1$long))
    {
      geol<-paste(iter1$lat[i],iter1$long[i],iter1$rad[i],sep=",")
      frame1<<-newSearchTwitter( un,1,"recent",geol)
      if(length(frame1)!=0){
        searchatthis(iter1$lat[i],iter1$long[i],iter1$rad[i],un,progbar)
      return()
      }
      #Sys.sleep(22)
    }
  }

  tempfile <- do.call("rbind", lapply(frame1, as.data.frame))
    #$ Save image
  tweeet<<-gsub(","," ",tempfile$text)
  tweeet<<-paste("'",tweeet,"'")
  tiiime<<-as.String(.POSIXct(tempfile$created))
  u<-getUser(urn)
  naaame<<-as.character(u$name)
  system(paste("java -jar VBJDataScraper.jar scrap_twitter_img ",u$profileImageUrl,paste(" ",urn),sep=""))
  
  
    if(!file.exists(paste("data/",urn,".csv",sep = ""))){
      df <- data.frame(Lat=double(),
                       Long=double(),
                       Radius=double(),
                       Tweet=character(),
                       SName=character(),
                       Time=character(),stringsAsFactors = FALSE)
      newrow<-c(flat,flong,fradi,tweeet,naaame,tiiime)
      df<-rbind(df,newrow)
      colnames(df)<-c("Lat","Long","Radius", "Tweet", "SName","Time")
      namec<-paste("data/",urn,".csv",sep = "")
      write.csv(df,file=namec,row.names = F)
      unlink("new.csv")
      return()
    }
    else
      {
      namec<-paste("data/",urn,".csv",sep = "")
      tdf<<-read.csv(file=namec,stringsAsFactors = FALSE)
      ind<-nrow(tdf)+1
      tdf[nrow(tdf)+1,] <- c(flat,flong,fradi,tweeet,naaame,tiiime)
      write.csv(tdf,file=namec,row.names = F)
      unlink("new.csv")
      return()
      }
}
  
  

