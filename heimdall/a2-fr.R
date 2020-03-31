source("automateapi.R")
frlat<<-0
frlong<<-0
frradi<<-0


locgenfr<-function(uname,progbar){ 
  urn<<-uname
  un<-paste("from:",uname,sep='')
  progbar$set(value = 0, message = paste("Searching",uname), detail = NULL)
  frame<-newSearchTwitter(un,1,"recent","22.5391,80.1172,1250mi")
  if(length(frame)!=0){
    searchatthisf("22.5391","80.1172","2100km",un,progbar)
  }
  else{
    progbar$inc(amount = 0, message = paste("Searching",uname), detail = "Not Found")
    Sys.sleep(1)
  }
}
searchatthisf<-function(lat,long,radius,un,progbar)
{
  frlat<<- lat
  frlong<<-long
  radi<-sub("km","",radius)
  radi<-as.integer(radi)
  frradi<<-radi
  print(paste(frlat,frlong,frradi))
  
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
        searchatthisf(iter1$lat[i],iter1$long[i],iter1$rad[i],un,progbar)
        return()
      }
      #Sys.sleep(22)
    }
  }
  
 
}



