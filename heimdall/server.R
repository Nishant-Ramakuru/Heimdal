library(shiny)
library(shinydashboard)
library(rpivotTable)
library(flexdashboard)
source("auth.R")
source("sent.R")
source("solo.R")
source("feed.R")
source("googletrend.R")
source("cryptrec.R")
source("rss.R")
source("bot.R")
source("user.R")
source("a2.R")
source("a2-fr.R")
source("mentions.R")
source("popgen.R")
source("networks.R")
source("hourcalc.R")
source("update1011.R")
library(shinyjs)

my_username <- c("test","admin")
my_password <- c("test","123")



shinyServer(function(input, output,session) {
  jsResetCode<-"shinyjs.reset=function(){history.go(0)}"
  observe({print(paste("Connected ",session$clientData$url_hostname,":",session$clientData$url_port,sep=''))})
  USER <- reactiveValues(Logged = F)
  GLOBAL <- reactiveValues(tpflag1=F,tpflag2=F,mmflag=F,tpflag3=F,tpflag4=F,tpflag5=F,tpflag6=F,mmflag=F)
  ui1 <- function(){
    tagList(
      div(id = "login",
          wellPanel(
            tags$a(img(src = 'logo.png',height = "290px",style = "display: block;width:100%;margin-left: auto;margin-right: auto;")),
              textInput(label="",placeholder =  "Username","userName"),
              passwordInput("passwd",label=NULL,placeholder =  "Password"),
              actionButton("Login", "Log in",style="color:#FFFFFF;background:#33B3D2;width:100%;")))
              ,tags$style(type="text/css", "#login { 
                                    display: block;margin-left: auto;margin-right: auto;
                                    width: 25%;
                                    box-shadow: 7px 7px 4px #888888;            
                                    margin-top:5%
                                    border: 1px solid #808080;}")
    )}
  trackkermap<<-leaflet() %>%addTiles() %>%setView(80, 22.5, zoom = 4)
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(my_username == Username)
          Id.password <- which(my_password == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
              print(paste("Connection ",session$clientData$url_hostname,"...Logged in as ",Username,sep=''))
              
              
            }
            else
            {
              observe({session$sendCustomMessage(type = 'msg',message=list(text='Incorrect Credentials...'))})
            }
          }
          else
          {
            observe({session$sendCustomMessage(type = 'msg',message=list(text='Incorrect Credentials...'))})
          }
        }
      }
    }
  })
  observe({
    if (USER$Logged == FALSE) {
      output$page <- renderUI({
        do.call(bootstrapPage,c("",ui1()))
      })
     
    }
    if (USER$Logged == TRUE)    {
      output$mymap3 <- renderLeaflet({trackkermap})
      output$page <- renderUI({mybody})
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      shinyjs::show(selector = "body > div > header")
      output$side <- renderUI({myside})
      ### Reqirements
      auth(1)
      filePathVBJ<<-paste("C:\\")
      ###logout
      
      output$mymap<-renderLeaflet({leaflet() %>% addProviderTiles(providers$Stamen.TonerLite,options = providerTileOptions(noWrap = TRUE)) 
      }) 
      output$angerbox<-flexdashboard::renderGauge({gaugemaker(100,"Anger")})
      output$anticipationbox<-flexdashboard::renderGauge({gaugemaker(100,"Ancipation")})
      output$disgustbox<-flexdashboard::renderGauge({gaugemaker(100,"Disgust")})
      output$fearbox<-flexdashboard::renderGauge({gaugemaker(100,"Fear")})
      output$posbox<- flexdashboard::renderGauge({gaugemaker(100,"Positive")})
      output$negbox<- flexdashboard::renderGauge({gaugemaker(100,"Negative")})
      output$trstbox<- flexdashboard::renderGauge({gaugemaker(100,"Trust")})
      output$sadbox<- flexdashboard::renderGauge({gaugemaker(100,"Sad")})
      output$surbox<- flexdashboard::renderGauge({gaugemaker(100,"Surprise")})
      output$joybox<- flexdashboard::renderGauge({gaugemaker(100,"Joy")})
      
      output$viewbt1<-renderUI(if(GLOBAL$tpflag1==F){witheye(1)}else{withslashedeye(1)})
      output$viewbt2<-renderUI(if(GLOBAL$tpflag2==F){witheye(2)}else{withslashedeye(2)})
      output$viewbt3<-renderUI(if(GLOBAL$tpflag3==F){witheye(3)}else{withslashedeye(3)})
      output$viewbt4<-renderUI(if(GLOBAL$tpflag4==F){witheye(4)}else{withslashedeye(4)})
      output$viewbt5<-renderUI(if(GLOBAL$tpflag5==F){witheye(5)}else{withslashedeye(5)})
      output$viewbt6<-renderUI(if(GLOBAL$tpflag6==F){witheye(6)}else{withslashedeye(6)})
      ### Analyze
          
      observeEvent(input$attbutton1,{
        anuser(paste("@",gsub("@","",input$attinput1),sep=""),input$attinput2)
      })
      observeEvent(input$attbutton2,{
        if(input$attselect=="Actor Network")
        {
          output$attout<-renderPlot(actor(input$attinputa1))
        }
        else   if(input$attselect=="Bimodal Network")
        {
          output$attout<-renderPlot(bimodal(input$attinputb1))
        }
        else   if(input$attselect=="Activity Plot")
        {
          output$attout<-renderPlot(hrcount(input$attinput1,input$attinput2))
        }
        else
        {
          output$attout<-renderPlot(semantic(input$attinputc3,input$attinputc2,input$attinputc1))
        }
        
      })
      
      ###OSA
      observeEvent(input$oabutton,{
        temp<-gsub("@","",input$oainput)
        system(paste("java -jar findOSA.jar ",temp," oaop_",temp,".csv",sep = ""))
        a<-read.csv(paste("data\\oaop_",temp,".csv",sep=""),stringsAsFactors = FALSE)
        a<-data.frame(a)
        output$oatable<-renderDataTable({a})
      })
      
      ###botpost
      fbUploaddf<<-data.frame("name"=c(1),"size"=c(1),"type"=c(1),"datapath"=c(1))
      fbUploaddf<<-fbUploaddf[0,]
      
      observeEvent(input$postfb,{
        a<-input$fbin
      write(a,"data/topostfb.txt")
      filelist<-list.files("fbupload")
      for(i in 0:length(filelist))
        filelist[i]<-paste(gsub("/","\\\\",getwd()),"\\fbupload\\",filelist[i],sep="")
      
      fileConn<-file("data/fbupload.txt")
      writeLines(filelist, fileConn)
      close(fileConn)
      
      system(paste("java -jar FbBotPoster.jar fbuserpass.csv 5 topostfb.txt fbupload.txt"))
      do.call(file.remove, list(list.files("fbupload", full.names = TRUE)))  
      fbUploaddf<<-fbUploaddf[0,]
      })
      
      
      observeEvent(input$fbpostfilein,{
        if(NROW(fbUploaddf)==0)
          fbUploaddf<<-input$fbpostfilein
        else
          fbUploaddf<<-rbind(fbUploaddf,input$fbpostfilein)
        df <- input$fbpostfilein
        path <- df$datapath 
        file.copy(path, paste("fbupload/",df$name,sep=""))
        filelist<-list.files("fbupload")
        for(i in 0:length(filelist))
          filelist[i]<-paste(gsub("/","\\\\",getwd()),"\\fbupload\\",filelist[i],sep="")
        
        fileConn<-file("data/fbupload.txt")
        writeLines(filelist, fileConn)
        close(fileConn)
        tempdf<-fbUploaddf
        tempdf$datapath<-NULL
        output$fbpostfileinout <- renderTable(tempdf,width = "100%")
      })
      
      twUploaddf<<-data.frame("name"=c(1),"size"=c(1),"type"=c(1),"datapath"=c(1))
      twUploaddf<<-twUploaddf[0,]
      
      observeEvent(input$posttwi,{
        a<-input$twin
        write(a,"data/toposttw.txt")
        filelist<-list.files("twupload")
        for(i in 0:length(filelist))
          filelist[i]<-paste(gsub("/","\\\\",getwd()),"\\twupload\\",filelist[i],sep="")
        
        fileConn<-file("data/twupload.txt")
        writeLines(filelist, fileConn)
        close(fileConn)
        
        system(paste("java -jar TwBotPoster.jar twuserpass.csv 5 toposttw.txt twupload.txt"))
        do.call(file.remove, list(list.files("twupload", full.names = TRUE)))  
        twUploaddf<<-twUploaddf[0,]
      })
      
      
      observeEvent(input$twpostfilein,{
        if(NROW(twUploaddf)==0)
          twUploaddf<<-input$twpostfilein
        else
          twUploaddf<<-rbind(twUploaddf,input$twpostfilein)
        df <- input$twpostfilein
        path <- df$datapath 
        file.copy(path, paste("twupload/",df$name,sep=""))
        filelist<-list.files("twupload")
        for(i in 0:length(filelist))
          filelist[i]<-paste(gsub("/","\\\\",getwd()),"\\twupload\\",filelist[i],sep="")
        
        fileConn<-file("data/twupload.txt")
        writeLines(filelist, fileConn)
        close(fileConn)
        tempdf<-twUploaddf
        tempdf$datapath<-NULL
        output$twpostfileinout <- renderTable(tempdf,width = "100%")
      })
      
      ### Options FB TW
      observe( output$fbuserpass<-renderTable(read.csv("data/fbuserpass.csv")))
      observe( output$twuserpass<-renderTable(read.csv("data/twuserpass.csv")))
    
      observeEvent(input$opbptabbutton3,{
        a<-read.csv("data/twuserpass.csv",stringsAsFactors=FALSE)
        flagtw<<-F
        if(input$opbptabinput4 != "" && input$opbptabinput5 != "" )
        {
          if(!(length(a$user)==0))
          {for(i in (1:length(a$user)))
            if(a$user[i]==input$opbptabinput4)
              flagtw<<-T
          }
          if(!flagtw)
            a[nrow(a)+1,] <- c(isolate(input$opbptabinput4),isolate(input$opbptabinput5))
          else
          {
            observe({session$sendCustomMessage(type = 'msg',message=list(text='Duplicate Entries...'))})
            flagtw<<-F
          }
        }
        else
        {
          observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields..'))})
        }
        write.csv(a,"data/twuserpass.csv",row.names = F)
        observe( output$twuserpass<-renderTable(read.csv("data/twuserpass.csv")))
      })
      observeEvent(input$opbptabbutton4,{
        a<-read.csv("data/twuserpass.csv",stringsAsFactors=FALSE)
        if(input$opbptabinput6 != "")
        {
          a<-a[!(a$user==input$opbptabinput6),]
        }
        else
        {
          observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields..'))})
        }
        write.csv(a,"data/twuserpass.csv",row.names = F)
        observe( output$twuserpass<-renderTable(read.csv("data/twuserpass.csv")))
      })  

      observeEvent(input$opbptabbutton1,{
        a<-read.csv("data/fbuserpass.csv",stringsAsFactors=FALSE)
        flagfb<<-F
        if(input$opbptabinput1 != "" && input$opbptabinput2 != "" )
        {
          if(!(length(a$user)==0))
          {for(i in (1:length(a$user)))
            if(a$user[i]==input$opbptabinput1)
              flagfb<<-T
          }
          if(!flagfb)
            a[nrow(a)+1,] <- c(isolate(input$opbptabinput1),isolate(input$opbptabinput2))
          else
          {
            observe({session$sendCustomMessage(type = 'msg',message=list(text='Duplicate Entries...'))})
            flagfb<<-F
          }
        }
        else
        {
          observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields..'))})
        }
        write.csv(a,"data/fbuserpass.csv",row.names = F)
        observe( output$fbuserpass<-renderTable(read.csv("data/fbuserpass.csv")))
        })
      observeEvent(input$opbptabbutton2,{
        a<-read.csv("data/fbuserpass.csv",stringsAsFactors=FALSE)
        if(input$opbptabinput3 != "")
        {
         a<-a[!(a$user==input$opbptabinput3),]
        }
        else
        {
          observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields..'))})
        }
        write.csv(a,"data/fbuserpass.csv",row.names = F)
        observe( output$fbuserpass<-renderTable(read.csv("data/fbuserpass.csv")))
      })
            
               
      
      ###Options (Change api)
      observe( output$apitable<-renderTable(subset(read.csv("data/tokens.csv"),select=c("id","name","apikey"))))
      observeEvent(input$addnewapibutton,{
  a<-read.csv("data/tokens.csv",stringsAsFactors=FALSE)
  flag<<-F
  if(input$newapiapiname != "" && input$newapiapikey != "" && input$newapiapitoken != "" && input$newapiapisecret != "" && input$newapiapitokensecret != "" )
  {
    if(!(length(a$name)==0))
      {for(i in (1:length(a$name)))
        if(a$name[i]==input$newapiapiname||a$apikey[i]==input$newapiapikey||a$apitoken[i]==input$newapiapitoken||a$apisecret[i]==input$newapiapisecret||a$apitokensecret[i]==input$newapiapitokensecret)
          flag<<-T
    }
      if(!flag)
        a[nrow(a)+1,] <- c((nrow(a)+1),isolate(input$newapiapiname),isolate(input$newapiapikey),isolate(input$newapiapitoken),isolate(input$newapiapisecret),isolate(input$newapiapitokensecret))
  else
  {
    observe({session$sendCustomMessage(type = 'msg',message=list(text='Duplicate Entries...'))})
    flag<<-F
  }
  }
  else
  {
    observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields..'))})
  }
  write.csv(a,"data/tokens.csv",row.names = F)
  output$apitable<-renderTable(subset(read.csv("data/tokens.csv"),select=c("id","name","apikey")))})
      observeEvent(input$deleteapibutton,{
        a<-read.csv("data/tokens.csv",stringsAsFactors=FALSE)
        if(input$deleteapiname != "")
        {
          if(length(a$name)==1)
          {
            observe({session$sendCustomMessage(type = 'msg',message=list(text='Cannot Delete Default API'))})
          }
          else
          {a<-a[!(a$name==input$deleteapiname),]
          for(i in (1:length(a$name)))
          a$id[i]<-i
          }
        }
        else
        {
          observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields..'))})
        }
        write.csv(a,"data/tokens.csv",row.names = F)
        output$apitable<-renderTable(subset(read.csv("data/tokens.csv"),select=c("id","name","apikey")))})
      observeEvent(input$selectapibutton,{
        a<-read.csv("data/tokens.csv",stringsAsFactors = F)
        j=as.integer(isolate(input$selectapiname))
        if(j>0&&j<=length(a$name))
        {auth(j)
          observe({session$sendCustomMessage(type = 'msg',message=list(text='API Changed Successfully !'))})
        }
        else
        {
          observe({session$sendCustomMessage(type = 'msg',message=list(text='Invalid API ID'))})
        }
      })

      ### Twitter Tracker
      
      plotfr<-function(nameis,iter)
      {
        a<-read.csv(paste("data/",nameis,"-fr.csv",sep=''))
        
        latss<-jitter(a$lat,0.001)
        Longss<-jitter(a$long,0.001)
        prog5<-Progress$new(session, min = 0, max = length(latss))
        trackkermap<<-trackkermap%>%setView(80, 22.5, zoom = 4)
        trackkermap<<-trackkermap%>%addCircles(lng=Longss,lat=latss,radius=a$Radius*1000,group = clpl[iter] , color = clpl[iter], weight = 2,  opacity =1, fill = TRUE, fillOpacity = 0.1)
        trackkermap<<-trackkermap%>%addPolylines(lng=Longss,lat=latss,group = clpl[iter] , color = clpl[iter], weight = 2,  opacity = 1,dashArray="5,5")
        for (i in (1:length(latss))){
          trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[i],lat=latss[i],group = clpl[iter] ,popup=genpopup(getUser(a$screenName[i])$name,paste(nameis,"[RF-@",a$screenName[i],"]",sep=''),a$created[i],a$text[i],a$long[i],a$lat[i],a$radius[i],clpl[iter]),
                                                        popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="users",library="fa",iconColor="#FFFFFF",markerColor="red"),options=markerOptions(riseOnHover = T))
        
          prog5$inc(amount=1,message="Mentions",detail=a$screenName[i])
          }
        prog5$close()
      }
      
      
         GLOBAL$tpflag1<-F
        observeEvent(input$ttclrbtn1,{
          if(!(isolate(input$ttinpt1)=="")){
        #    shinyjs::disable(selector = "button[type='button']")
        nameis<-sub("@","",paste(isolate(input$ttinpt1)))
        prog1<-Progress$new(session, min = 0, max = 2100)
        locgen(uname=nameis,prog1)
        prog1$close()
          output$mymap3 <- renderLeaflet({
          if(file.exists(paste("data/",nameis,".csv",sep='')))
          {
           GLOBAL$tpflag1<-T
          a<-read.csv(paste("data/",nameis,".csv",sep=''))
          latss<-jitter(a$Lat,0.001)
          Longss<-jitter(a$Long,0.001)
          trackkermap<<-trackkermap%>%clearGroup(clpl[1]) %>%setView(80, 22.5, zoom = 4)
          trackkermap<<-trackkermap%>%addCircles(lng=Longss,lat=latss,radius=a$Radius*1000,group = clpl[1] , color = clpl[1], weight = 2,  opacity =1, fill = TRUE, fillOpacity = 0.1)
          trackkermap<<-trackkermap%>%addPolylines(lng=Longss,lat=latss,group = clpl[1] , color = clpl[1], weight = 2,  opacity = 1)
          if(length(a$Long)>1){
            trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[1:length(a$Long)-1],lat=latss[1:length(a$Long)-1],group = clpl[1],popup=genpopup(a$SName[1:length(a$Long)-1],nameis,a$Time[1:length(a$Long)-1],a$Tweet[1:length(a$Long)-1],a$Long[1:length(a$Long)-1],a$Lat[1:length(a$Long)-1],a$Radius[1:length(a$Long)-1],clpl[1])
                                                          ,popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="street-view",library="fa",iconColor="#FFFFFF",markerColor="red"),options=markerOptions(riseOnHover = T))
          }
          trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[length(a$Long)],lat=latss[length(a$Long)],group = clpl[1] ,popup=genpopup(a$SName[length(a$Long)],nameis,a$Time[length(a$Long)],a$Tweet[length(a$Long)],round(a$Long[length(a$Long)],digits = 8),round(a$Lat[length(a$Long)],digits = 8),a$Radius[length(a$Long)],clpl[1]),
                                                        popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="location-arrow",library="fa",iconColor="#FFFFFF",markerColor="red",spin=T),options=markerOptions(riseOnHover = T))
          }
          else
          {
            observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('File not Present',nameis)))})
            trackkermap<<-trackkermap%>%clearGroup(clpl[1]) %>%setView(80, 22.5, zoom = 4)
            
          }
            trackkermap
          })
          
          }
          else{observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields'))})}})
        observeEvent(input$ttbtton1,{
          if(!(isolate(input$ttinpt1)=="")){
        
            nameis<-sub("@","",paste(isolate(input$ttinpt1)))
          if(GLOBAL$tpflag1==F){
          output$mymap3 <- renderLeaflet({
            
          if(file.exists(paste("data/",nameis,".csv",sep='')))
          {
          a<-read.csv(paste("data/",nameis,".csv",sep=''))
           GLOBAL$tpflag1<-T
          latss<-jitter(a$Lat,0.001)
          Longss<-jitter(a$Long,0.001)
          trackkermap<<-trackkermap%>%addCircles(lng=Longss,lat=latss,radius=a$Radius*1000,group = clpl[1] , color = clpl[1], weight = 2,  opacity =1, fill = TRUE, fillOpacity = 0.1)
          trackkermap<<-trackkermap%>%addPolylines(lng=Longss,lat=latss,group = clpl[1] , color = clpl[1], weight = 2,  opacity = 1)
          if(length(a$Long)>1){
          trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[1:length(a$Long)-1],lat=latss[1:length(a$Long)-1],group = clpl[1],popup=genpopup(a$SName[1:length(a$Long)-1],nameis,a$Time[1:length(a$Long)-1],a$Tweet[1:length(a$Long)-1],a$Long[1:length(a$Long)-1],a$Lat[1:length(a$Long)-1],a$Radius[1:length(a$Long)-1],clpl[1])
,popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="street-view",library="fa",iconColor="#FFFFFF",markerColor="red"),options=markerOptions(riseOnHover = T))
          }
          trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[length(a$Long)],lat=latss[length(a$Long)],group = clpl[1] ,popup=genpopup(a$SName[length(a$Long)],nameis,a$Time[length(a$Long)],a$Tweet[length(a$Long)],round(a$Long[length(a$Long)],digits = 8),round(a$Lat[length(a$Long)],digits = 8),a$Radius[length(a$Long)],clpl[1]),
            popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="location-arrow",library="fa",iconColor="#FFFFFF",markerColor="red",spin=T),options=markerOptions(riseOnHover = T))
          }
            
            
            else
            {
              observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('Cached Data Unavailable..',nameis)))})
            }
            if(file.exists(paste("data/",nameis,"-fr.csv",sep='')))
            {
            plotfr(nameis,1)
            }
            
            
            trackkermap<<-trackkermap%>%setView(80, 22.5, zoom = 4)
            trackkermap
            })}
        else
        {
          trackkermap<<-trackkermap%>%clearGroup(clpl[1]) %>%setView(80, 22.5, zoom = 4)
           GLOBAL$tpflag1<-F
          output$mymap3 <- renderLeaflet({trackkermap})
        }
          
          }
          else{observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields'))})}})
        observeEvent(input$ttfrrbtn1,{
          if(!(isolate(input$ttinpt1)=="")){
            #    shinyjs::disable(selector = "button[type='button']")
            nameis<-sub("@","",paste(isolate(input$ttinpt1)))
           sidesearch(uname=nameis,session)
            output$mymap3 <- renderLeaflet({
              trackkermap<<-trackkermap%>%clearGroup(clpl[1]) %>%setView(80, 22.5, zoom = 4)
              
              if(file.exists(paste("data/",nameis,"-fr.csv",sep='')))
              {
                GLOBAL$tpflag1<-T
                plotfr(nameis,1)
              }                                                                                               
              else
              {
                observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('File not Present',nameis)))})
                
              }
              trackkermap
            })
            
          }
          else{observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields'))})}})
         GLOBAL$tpflag2<-F
        observeEvent(input$ttclrbtn2,{
          if(!(isolate(input$ttinpt2)=="")){
            nameis<-sub("@","",paste(isolate(input$ttinpt2)))
            prog2<-Progress$new(session, min = 0, max = 2100)
            locgen(uname=nameis,prog2)
            prog2$close()
            output$mymap3 <- renderLeaflet({
              if(file.exists(paste("data/",nameis,".csv",sep='')))
              {
                 GLOBAL$tpflag2<-T
                a<-read.csv(paste("data/",nameis,".csv",sep=''))
                latss<-jitter(a$Lat,0.001)
                Longss<-jitter(a$Long,0.001)
                trackkermap<<-trackkermap%>%clearGroup(clpl[2]) %>%setView(80, 22.5, zoom = 4)
                trackkermap<<-trackkermap%>%addCircles(lng=Longss,lat=latss,radius=a$Radius*1000,group = clpl[2] , color = clpl[2], weight = 2,  opacity =1, fill = TRUE, fillOpacity = 0.1)
                trackkermap<<-trackkermap%>%addPolylines(lng=Longss,lat=latss,group = clpl[2] , color = clpl[2], weight = 2,  opacity = 1)
                if(length(a$Long)>1){
                  trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[1:length(a$Long)-1],lat=latss[1:length(a$Long)-1],group = clpl[2],popup=genpopup(a$SName[1:length(a$Long)-1],nameis,a$Time[1:length(a$Long)-1],a$Tweet[1:length(a$Long)-1],a$Long[1:length(a$Long)-1],a$Lat[1:length(a$Long)-1],a$Radius[1:length(a$Long)-1],clpl[2])
                                                                ,popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="street-view",library="fa",iconColor="#FFFFFF",markerColor=mlpl[2]),options=markerOptions(riseOnHover = T))
                }
                trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[length(a$Long)],lat=latss[length(a$Long)],group = clpl[2] ,popup=genpopup(a$SName[length(a$Long)],nameis,a$Time[length(a$Long)],a$Tweet[length(a$Long)],round(a$Long[length(a$Long)],digits = 8),round(a$Lat[length(a$Long)],digits = 8),a$Radius[length(a$Long)],clpl[2]),
                                                              popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="location-arrow",library="fa",iconColor="#FFFFFF",markerColor=mlpl[2],spin=T),options=markerOptions(riseOnHover = T))
              }
              else
              {
                observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('File not Present',nameis)))})
                trackkermap<<-trackkermap%>%clearGroup(clpl[2]) %>%setView(80, 22.5, zoom = 4)
                
              }
              trackkermap
            })
            
          }
          else{observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields'))})}})
        observeEvent(input$ttbtton2,{
          if(!(isolate(input$ttinpt2)=="")){
            
            nameis<-sub("@","",paste(isolate(input$ttinpt2)))
            if(GLOBAL$tpflag2==F){
              output$mymap3 <- renderLeaflet({
                
                if(file.exists(paste("data/",nameis,".csv",sep='')))
                {
                  a<-read.csv(paste("data/",nameis,".csv",sep=''))
                   GLOBAL$tpflag2<-T
                  latss<-jitter(a$Lat,0.001)
                  Longss<-jitter(a$Long,0.001)
                  trackkermap<<-trackkermap%>%addCircles(lng=Longss,lat=latss,radius=a$Radius*1000,group = clpl[2] , color = clpl[2], weight = 2,  opacity =1, fill = TRUE, fillOpacity = 0.1)
                  trackkermap<<-trackkermap%>%addPolylines(lng=Longss,lat=latss,group = clpl[2] , color = clpl[2], weight = 2,  opacity = 1)
                  if(length(a$Long)>1){
                    trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[1:length(a$Long)-1],lat=latss[1:length(a$Long)-1],group = clpl[2],popup=genpopup(a$SName[1:length(a$Long)-1],nameis,a$Time[1:length(a$Long)-1],a$Tweet[1:length(a$Long)-1],a$Long[1:length(a$Long)-1],a$Lat[1:length(a$Long)-1],a$Radius[1:length(a$Long)-1],clpl[2])
                                                                  ,popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="street-view",library="fa",iconColor="#FFFFFF",markerColor=mlpl[2]),options=markerOptions(riseOnHover = T))
                  }
                  trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[length(a$Long)],lat=latss[length(a$Long)],group = clpl[2] ,popup=genpopup(a$SName[length(a$Long)],nameis,a$Time[length(a$Long)],a$Tweet[length(a$Long)],round(a$Long[length(a$Long)],digits = 8),round(a$Lat[length(a$Long)],digits = 8),a$Radius[length(a$Long)],clpl[2]),
                                                                popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="location-arrow",library="fa",iconColor="#FFFFFF",markerColor=mlpl[2],spin=T),options=markerOptions(riseOnHover = T))
                }
                else
                {
                  observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('Cached Data Unavailable..',nameis)))})
                }
                trackkermap<<-trackkermap%>%setView(80, 22.5, zoom = 4)
                trackkermap
              })}
            else
            {
              trackkermap<<-trackkermap%>%clearGroup(clpl[2]) %>%setView(80, 22.5, zoom = 4)
               GLOBAL$tpflag2<-F
              output$mymap3 <- renderLeaflet({trackkermap})
            }
            
          }
          else{observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields'))})}})
        
        observeEvent(input$ttfrrbtn2,{
          if(!(isolate(input$ttinpt2)=="")){
            #    shinyjs::disable(selector = "button[type='button']")
            nameis<-sub("@","",paste(isolate(input$ttinpt2)))
            sidesearch(uname=nameis,session)
            output$mymap3 <- renderLeaflet({
              trackkermap<<-trackkermap%>%clearGroup(clpl[2]) %>%setView(80, 22.5, zoom = 4)
              
              if(file.exists(paste("data/",nameis,"-fr.csv",sep='')))
              {
                GLOBAL$tpflag2<-T
                plotfr(nameis,2)
              }                                                                                               
              else
              {
                observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('File not Present',nameis)))})
                
              }
              trackkermap
            })
            
          }
          else{observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields'))})}})
        
         GLOBAL$tpflag3<-F
        observeEvent(input$ttclrbtn3,{
          if(!(isolate(input$ttinpt3)=="")){
            nameis<-sub("@","",paste(isolate(input$ttinpt3)))
            prog3<-Progress$new(session, min = 0, max = 2100)
            locgen(uname=nameis,prog3)
            prog3$close()
            output$mymap3 <- renderLeaflet({
              if(file.exists(paste("data/",nameis,".csv",sep='')))
              {
                 GLOBAL$tpflag3<-T
                a<-read.csv(paste("data/",nameis,".csv",sep=''))
                latss<-jitter(a$Lat,0.001)
                Longss<-jitter(a$Long,0.001)
                trackkermap<<-trackkermap%>%clearGroup(clpl[3]) %>%setView(80, 22.5, zoom = 4)
                trackkermap<<-trackkermap%>%addCircles(lng=Longss,lat=latss,radius=a$Radius*1000,group = clpl[3] , color = clpl[3], weight = 2,  opacity =1, fill = TRUE, fillOpacity = 0.1)
                trackkermap<<-trackkermap%>%addPolylines(lng=Longss,lat=latss,group = clpl[3] , color = clpl[3], weight = 2,  opacity = 1)
                if(length(a$Long)>1){
                  trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[1:length(a$Long)-1],lat=latss[1:length(a$Long)-1],group = clpl[3],popup=genpopup(a$SName[1:length(a$Long)-1],nameis,a$Time[1:length(a$Long)-1],a$Tweet[1:length(a$Long)-1],a$Long[1:length(a$Long)-1],a$Lat[1:length(a$Long)-1],a$Radius[1:length(a$Long)-1],clpl[3])
                                                                ,popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="street-view",library="fa",iconColor="#FFFFFF",markerColor=mlpl[3]),options=markerOptions(riseOnHover = T))
                }
                trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[length(a$Long)],lat=latss[length(a$Long)],group = clpl[3] ,popup=genpopup(a$SName[length(a$Long)],nameis,a$Time[length(a$Long)],a$Tweet[length(a$Long)],round(a$Long[length(a$Long)],digits = 8),round(a$Lat[length(a$Long)],digits = 8),a$Radius[length(a$Long)],clpl[3]),
                                                              popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="location-arrow",library="fa",iconColor="#FFFFFF",markerColor=mlpl[3],spin=T),options=markerOptions(riseOnHover = T))
              }
              else
              {
                observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('File not Present',nameis)))})
                trackkermap<<-trackkermap%>%clearGroup(clpl[3]) %>%setView(80, 22.5, zoom = 4)
                
              }
              trackkermap
            })
            
          }
          else{observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields'))})}})
        observeEvent(input$ttbtton3,{
          if(!(isolate(input$ttinpt3)=="")){
            
            nameis<-sub("@","",paste(isolate(input$ttinpt3)))
            if(GLOBAL$tpflag3==F){
              output$mymap3 <- renderLeaflet({
                
                if(file.exists(paste("data/",nameis,".csv",sep='')))
                {
                  a<-read.csv(paste("data/",nameis,".csv",sep=''))
                   GLOBAL$tpflag3<-T
                  latss<-jitter(a$Lat,0.001)
                  Longss<-jitter(a$Long,0.001)
                  trackkermap<<-trackkermap%>%addCircles(lng=Longss,lat=latss,radius=a$Radius*1000,group = clpl[3] , color = clpl[3], weight = 2,  opacity =1, fill = TRUE, fillOpacity = 0.1)
                  trackkermap<<-trackkermap%>%addPolylines(lng=Longss,lat=latss,group = clpl[3] , color = clpl[3], weight = 2,  opacity = 1)
                  if(length(a$Long)>1){
                    trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[1:length(a$Long)-1],lat=latss[1:length(a$Long)-1],group = clpl[3],popup=genpopup(a$SName[1:length(a$Long)-1],nameis,a$Time[1:length(a$Long)-1],a$Tweet[1:length(a$Long)-1],a$Long[1:length(a$Long)-1],a$Lat[1:length(a$Long)-1],a$Radius[1:length(a$Long)-1],clpl[3])
                                                                  ,popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="street-view",library="fa",iconColor="#FFFFFF",markerColor=mlpl[3]),options=markerOptions(riseOnHover = T))
                  }
                  trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[length(a$Long)],lat=latss[length(a$Long)],group = clpl[3] ,popup=genpopup(a$SName[length(a$Long)],nameis,a$Time[length(a$Long)],a$Tweet[length(a$Long)],round(a$Long[length(a$Long)],digits = 8),round(a$Lat[length(a$Long)],digits = 8),a$Radius[length(a$Long)],clpl[3]),
                                                                popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="location-arrow",library="fa",iconColor="#FFFFFF",markerColor=mlpl[3],spin=T),options=markerOptions(riseOnHover = T))
                }
                else
                {
                  observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('Cached Data Unavailable..',nameis)))})
                }
                trackkermap<<-trackkermap%>%setView(80, 22.5, zoom = 4)
                trackkermap
              })}
            else
            {
              trackkermap<<-trackkermap%>%clearGroup(clpl[3]) %>%setView(80, 22.5, zoom = 4)
               GLOBAL$tpflag3<-F
              output$mymap3 <- renderLeaflet({trackkermap})
            }
            
          }
          else{observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields'))})}})
        observeEvent(input$ttfrrbtn3,{
          if(!(isolate(input$ttinpt3)=="")){
            #    shinyjs::disable(selector = "button[type='button']")
            nameis<-sub("@","",paste(isolate(input$ttinpt3)))
            sidesearch(uname=nameis,session)
            output$mymap3 <- renderLeaflet({
              trackkermap<<-trackkermap%>%clearGroup(clpl[3]) %>%setView(80, 22.5, zoom = 4)
              
              if(file.exists(paste("data/",nameis,"-fr.csv",sep='')))
              {
                GLOBAL$tpflag3<-T
                plotfr(nameis,3)
              }                                                                                               
              else
              {
                observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('File not Present',nameis)))})
                
              }
              trackkermap
            })
            
          }
          else{observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields'))})}})
         GLOBAL$tpflag4<-F
        observeEvent(input$ttclrbtn4,{
          if(!(isolate(input$ttinpt4)=="")){
            nameis<-sub("@","",paste(isolate(input$ttinpt4)))
            prog4<-Progress$new(session, min = 0, max = 2100)
            locgen(uname=nameis,prog4)
            prog4$close()
            output$mymap3 <- renderLeaflet({
              if(file.exists(paste("data/",nameis,".csv",sep='')))
              {
                 GLOBAL$tpflag4<-T
                a<-read.csv(paste("data/",nameis,".csv",sep=''))
                latss<-jitter(a$Lat,0.001)
                Longss<-jitter(a$Long,0.001)
                trackkermap<<-trackkermap%>%clearGroup(clpl[4]) %>%setView(80, 22.5, zoom = 4)
                trackkermap<<-trackkermap%>%addCircles(lng=Longss,lat=latss,radius=a$Radius*1000,group = clpl[4] , color = clpl[4], weight = 2,  opacity =1, fill = TRUE, fillOpacity = 0.1)
                trackkermap<<-trackkermap%>%addPolylines(lng=Longss,lat=latss,group = clpl[4] , color = clpl[4], weight = 2,  opacity = 1)
                if(length(a$Long)>1){
                  trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[1:length(a$Long)-1],lat=latss[1:length(a$Long)-1],group = clpl[4],popup=genpopup(a$SName[1:length(a$Long)-1],nameis,a$Time[1:length(a$Long)-1],a$Tweet[1:length(a$Long)-1],a$Long[1:length(a$Long)-1],a$Lat[1:length(a$Long)-1],a$Radius[1:length(a$Long)-1],clpl[4])
                                                                ,popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="street-view",library="fa",iconColor="#FFFFFF",markerColor=mlpl[4]),options=markerOptions(riseOnHover = T))
                }
                trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[length(a$Long)],lat=latss[length(a$Long)],group = clpl[4] ,popup=genpopup(a$SName[length(a$Long)],nameis,a$Time[length(a$Long)],a$Tweet[length(a$Long)],round(a$Long[length(a$Long)],digits = 8),round(a$Lat[length(a$Long)],digits = 8),a$Radius[length(a$Long)],clpl[4]),
                                                              popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="location-arrow",library="fa",iconColor="#FFFFFF",markerColor=mlpl[4],spin=T),options=markerOptions(riseOnHover = T))
              }
              else
              {
                observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('File not Present',nameis)))})
                trackkermap<<-trackkermap%>%clearGroup(clpl[4]) %>%setView(80, 22.5, zoom = 4)
                
              }
              trackkermap
            })
            
          }
          else{observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields'))})}})
        observeEvent(input$ttbtton4,{
          if(!(isolate(input$ttinpt4)=="")){
            
            nameis<-sub("@","",paste(isolate(input$ttinpt4)))
            if(GLOBAL$tpflag4==F){
              output$mymap3 <- renderLeaflet({
                
                if(file.exists(paste("data/",nameis,".csv",sep='')))
                {
                  a<-read.csv(paste("data/",nameis,".csv",sep=''))
                   GLOBAL$tpflag4<-T
                  latss<-jitter(a$Lat,0.001)
                  Longss<-jitter(a$Long,0.001)
                  trackkermap<<-trackkermap%>%addCircles(lng=Longss,lat=latss,radius=a$Radius*1000,group = clpl[4] , color = clpl[4], weight = 2,  opacity =1, fill = TRUE, fillOpacity = 0.1)
                  trackkermap<<-trackkermap%>%addPolylines(lng=Longss,lat=latss,group = clpl[4] , color = clpl[4], weight = 2,  opacity = 1)
                  if(length(a$Long)>1){
                    trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[1:length(a$Long)-1],lat=latss[1:length(a$Long)-1],group = clpl[4],popup=genpopup(a$SName[1:length(a$Long)-1],nameis,a$Time[1:length(a$Long)-1],a$Tweet[1:length(a$Long)-1],a$Long[1:length(a$Long)-1],a$Lat[1:length(a$Long)-1],a$Radius[1:length(a$Long)-1],clpl[4])
                                                                  ,popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="street-view",library="fa",iconColor="#FFFFFF",markerColor=mlpl[4]),options=markerOptions(riseOnHover = T))
                  }
                  trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[length(a$Long)],lat=latss[length(a$Long)],group = clpl[4] ,popup=genpopup(a$SName[length(a$Long)],nameis,a$Time[length(a$Long)],a$Tweet[length(a$Long)],round(a$Long[length(a$Long)],digits = 8),round(a$Lat[length(a$Long)],digits = 8),a$Radius[length(a$Long)],clpl[4]),
                                                                popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="location-arrow",library="fa",iconColor="#FFFFFF",markerColor=mlpl[4],spin=T),options=markerOptions(riseOnHover = T))
                }
                else
                {
                  observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('Cached Data Unavailable..',nameis)))})
                }
                trackkermap<<-trackkermap%>%setView(80, 22.5, zoom = 4)
                trackkermap
              })}
            else
            {
              trackkermap<<-trackkermap%>%clearGroup(clpl[4]) %>%setView(80, 22.5, zoom = 4)
               GLOBAL$tpflag4<-F
              output$mymap3 <- renderLeaflet({trackkermap})
            }
            
          }
          else{observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields'))})}})
        observeEvent(input$ttfrrbtn4,{
          if(!(isolate(input$ttinpt4)=="")){
            #    shinyjs::disable(selector = "button[type='button']")
            nameis<-sub("@","",paste(isolate(input$ttinpt4)))
            sidesearch(uname=nameis,session)
            output$mymap3 <- renderLeaflet({
              trackkermap<<-trackkermap%>%clearGroup(clpl[4]) %>%setView(80, 22.5, zoom = 4)
              
              if(file.exists(paste("data/",nameis,"-fr.csv",sep='')))
              {
                GLOBAL$tpflag4<-T
                plotfr(nameis,4)
              }                                                                                               
              else
              {
                observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('File not Present',nameis)))})
                
              }
              trackkermap
            })
            
          }
          else{observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields'))})}})
         GLOBAL$tpflag5<-F
        observeEvent(input$ttclrbtn5,{
          if(!(isolate(input$ttinpt5)=="")){
            nameis<-sub("@","",paste(isolate(input$ttinpt5)))
            prog5<-Progress$new(session, min = 0, max = 2100)
            locgen(uname=nameis,prog5)
            prog5$close()
            output$mymap3 <- renderLeaflet({
              if(file.exists(paste("data/",nameis,".csv",sep='')))
              {
                 GLOBAL$tpflag5<-T
                a<-read.csv(paste("data/",nameis,".csv",sep=''))
                latss<-jitter(a$Lat,0.001)
                Longss<-jitter(a$Long,0.001)
                trackkermap<<-trackkermap%>%clearGroup(clpl[5]) %>%setView(80, 22.5, zoom = 4)
                trackkermap<<-trackkermap%>%addCircles(lng=Longss,lat=latss,radius=a$Radius*1000,group = clpl[5] , color = clpl[5], weight = 2,  opacity =1, fill = TRUE, fillOpacity = 0.1)
                trackkermap<<-trackkermap%>%addPolylines(lng=Longss,lat=latss,group = clpl[5] , color = clpl[5], weight = 2,  opacity = 1)
                if(length(a$Long)>1){
                  trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[1:length(a$Long)-1],lat=latss[1:length(a$Long)-1],group = clpl[5],popup=genpopup(a$SName[1:length(a$Long)-1],nameis,a$Time[1:length(a$Long)-1],a$Tweet[1:length(a$Long)-1],a$Long[1:length(a$Long)-1],a$Lat[1:length(a$Long)-1],a$Radius[1:length(a$Long)-1],clpl[5])
                                                                ,popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="street-view",library="fa",iconColor="#FFFFFF",markerColor=mlpl[5]),options=markerOptions(riseOnHover = T))
                }
                trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[length(a$Long)],lat=latss[length(a$Long)],group = clpl[5] ,popup=genpopup(a$SName[length(a$Long)],nameis,a$Time[length(a$Long)],a$Tweet[length(a$Long)],round(a$Long[length(a$Long)],digits = 8),round(a$Lat[length(a$Long)],digits = 8),a$Radius[length(a$Long)],clpl[5]),
                                                              popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="location-arrow",library="fa",iconColor="#FFFFFF",markerColor=mlpl[5],spin=T),options=markerOptions(riseOnHover = T))
              }
              else
              {
                observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('File not Present',nameis)))})
                trackkermap<<-trackkermap%>%clearGroup(clpl[5]) %>%setView(80, 22.5, zoom = 4)
                
              }
              trackkermap
            })
            
          }
          else{observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields'))})}})
        observeEvent(input$ttbtton5,{
          if(!(isolate(input$ttinpt5)=="")){
            
            nameis<-sub("@","",paste(isolate(input$ttinpt5)))
            if(GLOBAL$tpflag5==F){
              output$mymap3 <- renderLeaflet({
                
                if(file.exists(paste("data/",nameis,".csv",sep='')))
                {
                  a<-read.csv(paste("data/",nameis,".csv",sep=''))
                   GLOBAL$tpflag5<-T
                  latss<-jitter(a$Lat,0.001)
                  Longss<-jitter(a$Long,0.001)
                  trackkermap<<-trackkermap%>%addCircles(lng=Longss,lat=latss,radius=a$Radius*1000,group = clpl[5] , color = clpl[5], weight = 2,  opacity =1, fill = TRUE, fillOpacity = 0.1)
                  trackkermap<<-trackkermap%>%addPolylines(lng=Longss,lat=latss,group = clpl[5] , color = clpl[5], weight = 2,  opacity = 1)
                  if(length(a$Long)>1){
                    trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[1:length(a$Long)-1],lat=latss[1:length(a$Long)-1],group = clpl[5],popup=genpopup(a$SName[1:length(a$Long)-1],nameis,a$Time[1:length(a$Long)-1],a$Tweet[1:length(a$Long)-1],a$Long[1:length(a$Long)-1],a$Lat[1:length(a$Long)-1],a$Radius[1:length(a$Long)-1],clpl[5])
                                                                  ,popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="street-view",library="fa",iconColor="#FFFFFF",markerColor=mlpl[5]),options=markerOptions(riseOnHover = T))
                  }
                  trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[length(a$Long)],lat=latss[length(a$Long)],group = clpl[5] ,popup=genpopup(a$SName[length(a$Long)],nameis,a$Time[length(a$Long)],a$Tweet[length(a$Long)],round(a$Long[length(a$Long)],digits = 8),round(a$Lat[length(a$Long)],digits = 8),a$Radius[length(a$Long)],clpl[5]),
                                                                popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="location-arrow",library="fa",iconColor="#FFFFFF",markerColor=mlpl[5],spin=T),options=markerOptions(riseOnHover = T))
                }
                else
                {
                  observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('Cached Data Unavailable..',nameis)))})
                }
                trackkermap<<-trackkermap%>%setView(80, 22.5, zoom = 4)
                trackkermap
              })}
            else
            {
              trackkermap<<-trackkermap%>%clearGroup(clpl[5]) %>%setView(80, 22.5, zoom = 4)
               GLOBAL$tpflag5<-F
              output$mymap3 <- renderLeaflet({trackkermap})
            }
            
          }
          else{observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields'))})}})
        observeEvent(input$ttfrrbtn5,{
          if(!(isolate(input$ttinpt5)=="")){
            #    shinyjs::disable(selector = "button[type='button']")
            nameis<-sub("@","",paste(isolate(input$ttinpt5)))
            sidesearch(uname=nameis,session)
            output$mymap3 <- renderLeaflet({
              trackkermap<<-trackkermap%>%clearGroup(clpl[5]) %>%setView(80, 22.5, zoom = 4)
              
              if(file.exists(paste("data/",nameis,"-fr.csv",sep='')))
              {
                GLOBAL$tpflag5<-T
                plotfr(nameis,5)
              }                                                                                               
              else
              {
                observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('File not Present',nameis)))})
                
              }
              trackkermap
            })
            
          }
          else{observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields'))})}})
         GLOBAL$tpflag6<-F
        observeEvent(input$ttclrbtn6,{
          if(!(isolate(input$ttinpt6)=="")){
            nameis<-sub("@","",paste(isolate(input$ttinpt6)))
            prog6<-Progress$new(session, min = 0, max = 2100)
            locgen(uname=nameis,prog6)
            prog6$close()
            output$mymap3 <- renderLeaflet({
              if(file.exists(paste("data/",nameis,".csv",sep='')))
              {
                 GLOBAL$tpflag6<-T
                a<-read.csv(paste("data/",nameis,".csv",sep=''))
                latss<-jitter(a$Lat,0.001)
                Longss<-jitter(a$Long,0.001)
                trackkermap<<-trackkermap%>%clearGroup(clpl[6]) %>%setView(80, 22.5, zoom = 4)
                trackkermap<<-trackkermap%>%addCircles(lng=Longss,lat=latss,radius=a$Radius*1000,group = clpl[6] , color = clpl[6], weight = 2,  opacity =1, fill = TRUE, fillOpacity = 0.1)
                trackkermap<<-trackkermap%>%addPolylines(lng=Longss,lat=latss,group = clpl[6] , color = clpl[6], weight = 2,  opacity = 1)
                if(length(a$Long)>1){
                  trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[1:length(a$Long)-1],lat=latss[1:length(a$Long)-1],group = clpl[6],popup=genpopup(a$SName[1:length(a$Long)-1],nameis,a$Time[1:length(a$Long)-1],a$Tweet[1:length(a$Long)-1],a$Long[1:length(a$Long)-1],a$Lat[1:length(a$Long)-1],a$Radius[1:length(a$Long)-1],clpl[6])
                                                                ,popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="street-view",library="fa",iconColor="#FFFFFF",markerColor=mlpl[6]),options=markerOptions(riseOnHover = T))
                }
                trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[length(a$Long)],lat=latss[length(a$Long)],group = clpl[6] ,popup=genpopup(a$SName[length(a$Long)],nameis,a$Time[length(a$Long)],a$Tweet[length(a$Long)],round(a$Long[length(a$Long)],digits = 8),round(a$Lat[length(a$Long)],digits = 8),a$Radius[length(a$Long)],clpl[6]),
                                                              popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="location-arrow",library="fa",iconColor="#FFFFFF",markerColor=mlpl[6],spin=T),options=markerOptions(riseOnHover = T))
              }
              else
              {
                observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('File not Present',nameis)))})
                trackkermap<<-trackkermap%>%clearGroup(clpl[6]) %>%setView(80, 22.5, zoom = 4)
                
              }
              trackkermap
            })
            
          }
          else{observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields'))})}})
        observeEvent(input$ttbtton6,{
          if(!(isolate(input$ttinpt6)=="")){
            
            nameis<-sub("@","",paste(isolate(input$ttinpt6)))
            if(GLOBAL$tpflag6==F){
              output$mymap3 <- renderLeaflet({
                
                if(file.exists(paste("data/",nameis,".csv",sep='')))
                {
                  a<-read.csv(paste("data/",nameis,".csv",sep=''))
                   GLOBAL$tpflag6<-T
                  latss<-jitter(a$Lat,0.001)
                  Longss<-jitter(a$Long,0.001)
                  trackkermap<<-trackkermap%>%addCircles(lng=Longss,lat=latss,radius=a$Radius*1000,group = clpl[6] , color = clpl[6], weight = 2,  opacity =1, fill = TRUE, fillOpacity = 0.1)
                  trackkermap<<-trackkermap%>%addPolylines(lng=Longss,lat=latss,group = clpl[6] , color = clpl[6], weight = 2,  opacity = 1)
                  if(length(a$Long)>1){
                    trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[1:length(a$Long)-1],lat=latss[1:length(a$Long)-1],group = clpl[6],popup=genpopup(a$SName[1:length(a$Long)-1],nameis,a$Time[1:length(a$Long)-1],a$Tweet[1:length(a$Long)-1],a$Long[1:length(a$Long)-1],a$Lat[1:length(a$Long)-1],a$Radius[1:length(a$Long)-1],clpl[6])
                                                                  ,popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="street-view",library="fa",iconColor="#FFFFFF",markerColor=mlpl[6]),options=markerOptions(riseOnHover = T))
                  }
                  trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[length(a$Long)],lat=latss[length(a$Long)],group = clpl[6] ,popup=genpopup(a$SName[length(a$Long)],nameis,a$Time[length(a$Long)],a$Tweet[length(a$Long)],round(a$Long[length(a$Long)],digits = 8),round(a$Lat[length(a$Long)],digits = 8),a$Radius[length(a$Long)],clpl[6]),
                                                                popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="location-arrow",library="fa",iconColor="#FFFFFF",markerColor=mlpl[6],spin=T),options=markerOptions(riseOnHover = T))
                }
                else
                {
                  observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('Cached Data Unavailable..',nameis)))})
                }
                trackkermap<<-trackkermap%>%setView(80, 22.5, zoom = 4)
                trackkermap
              })}
            else
            {
              trackkermap<<-trackkermap%>%clearGroup(clpl[6]) %>%setView(80, 22.5, zoom = 4)
               GLOBAL$tpflag6<-F
              output$mymap3 <- renderLeaflet({trackkermap})
            }
            
          }
          else{observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields'))})}})
        observeEvent(input$ttfrrbtn6,{
          if(!(isolate(input$ttinpt6)=="")){
            #    shinyjs::disable(selector = "button[type='button']")
            nameis<-sub("@","",paste(isolate(input$ttinpt6)))
            sidesearch(uname=nameis,session)
            output$mymap3 <- renderLeaflet({
              trackkermap<<-trackkermap%>%clearGroup(clpl[6]) %>%setView(80, 22.5, zoom = 4)
              
              if(file.exists(paste("data/",nameis,"-fr.csv",sep='')))
              {
                GLOBAL$tpflag6<-T
                plotfr(nameis,6)
              }                                                                                               
              else
              {
                observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('File not Present',nameis)))})
                
              }
              trackkermap
            })
            
          }
          else{observe({session$sendCustomMessage(type = 'msg',message=list(text='Empty Fields'))})}})
        tpflagall<<-F
        observeEvent(input$ttallbtton,{
          
            
          if(tpflagall==T)
          {
            prog<-Progress$new(session,min=0,max=6)
            for (i in 1:6){
              trackkermap<<-trackkermap %>% clearGroup(clpl[i])
              prog$inc(amount=1,message="Removing",detail=paste(mlpl[i]))
              Sys.sleep(0.3)
            }
            prog$close()
            tpflagall<<-F
             GLOBAL$tpflag1<-F
             GLOBAL$tpflag2<-F
             GLOBAL$tpflag3<-F
             GLOBAL$tpflag4<-F
             GLOBAL$tpflag5<-F
             GLOBAL$tpflag6<-F}
          else
          {
            trackkermap<<-trackkermap %>% clearGroup(clpl[1]) %>% clearGroup(clpl[2]) %>% clearGroup(clpl[3]) %>% clearGroup(clpl[4]) %>% clearGroup(clpl[5]) %>% clearGroup(clpl[6])
            namess=vector()
            namess[1]<-sub("@","",paste(isolate(input$ttinpt1)))
            namess[2]<-sub("@","",paste(isolate(input$ttinpt2)))
            namess[3]<-sub("@","",paste(isolate(input$ttinpt3)))
            namess[4]<-sub("@","",paste(isolate(input$ttinpt4)))
            namess[5]<-sub("@","",paste(isolate(input$ttinpt5)))
            namess[6]<-sub("@","",paste(isolate(input$ttinpt6)))
            
            prog<-Progress$new(session,min=0,max=length(namess[namess!=""]))
            for (i in 1:6){
            if(namess[i]!="")
            {
            
              
              if(file.exists(paste("data/",namess[i],".csv",sep='')))
            {
              a<-read.csv(paste("data/",namess[i],".csv",sep=''))
              prog$inc(amount=1,message=paste("Plotting ",namess[i]),detail="Cache Found")
              latss<-jitter(a$Lat,0.001)
              Longss<-jitter(a$Long,0.001)
              trackkermap<<-trackkermap%>%addCircles(lng=Longss,lat=latss,radius=a$Radius*1000,group = clpl[i] , color = clpl[i], weight = 2,  opacity =1, fill = TRUE, fillOpacity = 0.1)
              trackkermap<<-trackkermap%>%addPolylines(lng=Longss,lat=latss,group = clpl[i] , color = clpl[i], weight = 2,  opacity = 1)
              if(length(a$Long)>1){
                trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[1:length(a$Long)-1],lat=latss[1:length(a$Long)-1],group = clpl[i],popup=genpopup(a$SName[1:length(a$Long)-1],namess[i],a$Time[1:length(a$Long)-1],a$Tweet[1:length(a$Long)-1],a$Long[1:length(a$Long)-1],a$Lat[1:length(a$Long)-1],a$Radius[1:length(a$Long)-1],clpl[i])
                                                              ,popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="street-view",library="fa",iconColor="#FFFFFF",markerColor=mlpl[i]),options=markerOptions(riseOnHover = T))
              
                }
              trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[length(a$Long)],lat=latss[length(a$Long)],group = clpl[i] ,popup=genpopup(a$SName[length(a$Long)],namess[i],a$Time[length(a$Long)],a$Tweet[length(a$Long)],round(a$Long[length(a$Long)],digits = 8),round(a$Lat[length(a$Long)],digits = 8),a$Radius[length(a$Long)],clpl[i]),
                                                            popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="location-arrow",library="fa",iconColor="#FFFFFF",markerColor=mlpl[i],spin=T),options=markerOptions(riseOnHover = T))
            }
            else
            {
              prog$inc(amount=1,message=paste("Plotting ",namess[i]),detail="Cache Not Found")
              
             # observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('Cached Data Unavailable..')))})
            }}
              Sys.sleep(1)
            }
            prog$close()
            tpflagall<<-T
            GLOBAL$tpflag1<-T
             GLOBAL$tpflag2<-T
             GLOBAL$tpflag3<-T
             GLOBAL$tpflag4<-T
             GLOBAL$tpflag5<-T
             GLOBAL$tpflag6<-T
            }
            
          
          trackkermap<<-trackkermap%>%setView(80, 22.5, zoom = 4)
          
          output$mymap3<-renderLeaflet(trackkermap )
          
        })
        observeEvent(input$ttclrallbtn,{
          
          
          if(tpflagall==T)
          {
            prog3<-Progress$new(session,min=0,max=6)
            for (i in 1:6){
              trackkermap<<-trackkermap %>% clearGroup(clpl[i])
              prog3$inc(amount=1,message="Removing",detail=paste(mlpl[i]))
              Sys.sleep(0.3)
            }
            prog3$close()
           }
            namess=vector()
            namess[1]<-sub("@","",paste(isolate(input$ttinpt1)))
            namess[2]<-sub("@","",paste(isolate(input$ttinpt2)))
            namess[3]<-sub("@","",paste(isolate(input$ttinpt3)))
            namess[4]<-sub("@","",paste(isolate(input$ttinpt4)))
            namess[5]<-sub("@","",paste(isolate(input$ttinpt5)))
            namess[6]<-sub("@","",paste(isolate(input$ttinpt6)))
            
            prog<-Progress$new(session,min=0,max=length(namess[namess!=""]))
            prog$inc(amount=0,message=paste("Plotting ",namess[1]))
            
            for (i in 1:6){
              if(namess[i]!="")
              {
                prog1<-Progress$new(session,min=0,max=2100)
                locgen(uname=namess[i],progbar = prog1)
                prog1$close()
                if(file.exists(paste("data/",namess[i],".csv",sep='')))
                {
                  a<-read.csv(paste("data/",namess[i],".csv",sep=''))
                  prog$inc(amount=1,message=paste("Plotting ",namess[i]),detail="Cache Found")
                  latss<-jitter(a$Lat,0.001)
                  Longss<-jitter(a$Long,0.001)
                  trackkermap<<-trackkermap%>%addCircles(lng=Longss,lat=latss,radius=a$Radius*1000,group = clpl[i] , color = clpl[i], weight = 2,  opacity =1, fill = TRUE, fillOpacity = 0.1)
                  trackkermap<<-trackkermap%>%addPolylines(lng=Longss,lat=latss,group = clpl[i] , color = clpl[6], weight = 2,  opacity = 1)
                  if(length(a$Long)>1){
                    trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[1:length(a$Long)-1],lat=latss[1:length(a$Long)-1],group = clpl[i],popup=genpopup(a$SName[1:length(a$Long)-1],namess[i],a$Time[1:length(a$Long)-1],a$Tweet[1:length(a$Long)-1],a$Long[1:length(a$Long)-1],a$Lat[1:length(a$Long)-1],a$Radius[1:length(a$Long)-1],clpl[i])
                                                                  ,popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="street-view",library="fa",iconColor="#FFFFFF",markerColor=mlpl[i]),options=markerOptions(riseOnHover = T))
                    
                  }
                  trackkermap<<-trackkermap%>%addAwesomeMarkers(lng=Longss[length(a$Long)],lat=latss[length(a$Long)],group = clpl[i] ,popup=genpopup(a$SName[length(a$Long)],namess[i],a$Time[length(a$Long)],a$Tweet[length(a$Long)],round(a$Long[length(a$Long)],digits = 8),round(a$Lat[length(a$Long)],digits = 8),a$Radius[length(a$Long)],clpl[i]),
                                                                popupOptions=popupOptions( maxWidth = 350,minWidth=330),icon=makeAwesomeIcon(icon="location-arrow",library="fa",iconColor="#FFFFFF",markerColor=mlpl[i],spin=T),options=markerOptions(riseOnHover = T))
                }
                else
                {
                  prog$inc(amount=1,message=paste("Plotting ",namess[i]),detail="Cache Not Found")
                  
                  # observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('Cached Data Unavailable..')))})
                }}
              Sys.sleep(1)
            }
            prog$close()
            tpflagall<<-T
            GLOBAL$tpflag1<-T
            GLOBAL$tpflag2<-T
            GLOBAL$tpflag3<-T
            GLOBAL$tpflag4<-T
            GLOBAL$tpflag5<-T
            GLOBAL$tpflag6<-T
          
          
          trackkermap<<-trackkermap%>%setView(80, 22.5, zoom = 4)
          
          output$mymap3<-renderLeaflet(trackkermap )
          
        })
        observeEvent(input$ttfrrallbtn,{
          
          
          if(tpflagall==T)
          {
            prog3<-Progress$new(session,min=0,max=6)
            for (i in 1:6){
              trackkermap<<-trackkermap %>% clearGroup(clpl[i])
              prog3$inc(amount=1,message="Removing",detail=paste(mlpl[i]))
              Sys.sleep(0.3)
            }
            prog3$close()
          }
          namess=vector()
          namess[1]<-sub("@","",paste(isolate(input$ttinpt1)))
          namess[2]<-sub("@","",paste(isolate(input$ttinpt2)))
          namess[3]<-sub("@","",paste(isolate(input$ttinpt3)))
          namess[4]<-sub("@","",paste(isolate(input$ttinpt4)))
          namess[5]<-sub("@","",paste(isolate(input$ttinpt5)))
          namess[6]<-sub("@","",paste(isolate(input$ttinpt6)))
          
          prog<-Progress$new(session,min=0,max=length(namess[namess!=""]))
          prog$inc(amount=0,message=paste("Plotting ",namess[1]))
          
          for (i in 1:6){
            if(namess[i]!="")
            {
              sidesearch(namess[i],session)
              if(file.exists(paste("data/",namess[i],"-fr.csv",sep='')))
              {
                prog$inc(amount=1,message=paste("Plotting ",namess[i]),detail="Cache Found")
                plotfr(namess[i],i)
              }
              else
              {
                prog$inc(amount=1,message=paste("Plotting ",namess[i]),detail="Cache Not Found")
                
                # observe({session$sendCustomMessage(type = 'msg',message=list(text=paste('Cached Data Unavailable..')))})
              }}
            Sys.sleep(1)
          }
          prog$close()
          tpflagall<<-T
          GLOBAL$tpflag1<-T
          GLOBAL$tpflag2<-T
          GLOBAL$tpflag3<-T
          GLOBAL$tpflag4<-T
          GLOBAL$tpflag5<-T
          GLOBAL$tpflag6<-T
          
          
          trackkermap<<-trackkermap%>%setView(80, 22.5, zoom = 4)
          
          output$mymap3<-renderLeaflet(trackkermap )
          
        })
        
      ##### Keyword Search #####
       
   
      observeEvent(input$ksbtton,{
        if(input$ksnuminputstotake=="")
          num=30
        else num=input$ksnuminputstotake
        if(input$kslocation=="")
          loc="India"
        else loc=input$kslocation
        
        
        srch(input$ksinpt,num,geotext =str_replace_all(loc," ",","),sinc=substr(input$ksdaterange[1],1,10),unt=substr(input$ksdaterange[2],1,10))
        dff1<-avgt()
        
        system(paste("java -jar newbot.jar gaikwad.akshay79@gmail.com ggakshayggakshay ",str_replace_all(input$ksinpt," ","+")," ",num," ",substr(input$ksdaterange[1],1,4)," ",substr(input$ksdaterange[2],1,4)," ",str_replace_all(loc," ","+"),sep = ""))
        dff2<-read.csv("a.csv",stringsAsFactors = FALSE)
        unlink("a.csv")
        dff2<-addcolumn(dff2)
        
        dff5<-rbind(dff1,dff2)
        val<-as.integer(100/length(dff5$Post))
        dff5
        
        output$pivot <-  renderDataTable({dff5})
        anuser(input$ksinpt)
        output$kstabnetwork<-renderPlot({actor()})
        output$kstabbimodal<-renderPlot({bimodal()})
        output$mymap <- renderLeaflet({
          
          avgtmap()
          dff<-negma()
          lattt<-as.numeric(dff$ltt)
          longg<-as.numeric(dff$lng)
          
          sents<-gnrc()
          print(sents)
          
         
          
          m<-leaflet()
          m<-m %>% addProviderTiles(providers$Stamen.TonerLite,options = providerTileOptions(noWrap = TRUE)) 
          
          output$angerbox<-flexdashboard::renderGauge({gaugemaker(as.integer(sents$count[1]*val),"Anger")})
          output$anticipationbox<-flexdashboard::renderGauge({gaugemaker(as.integer(sents$count[2]*val),"Ancipation")})
          output$disgustbox<-flexdashboard::renderGauge({gaugemaker(as.integer(sents$count[3]*val),"Disgust")})
          output$fearbox<-flexdashboard::renderGauge({gaugemaker(as.integer(sents$count[4]*val),"Fear")})
          output$posbox<- flexdashboard::renderGauge({gaugemaker(as.integer(sents$count[10]*val),"Positive")})
          output$negbox<- flexdashboard::renderGauge({gaugemaker(as.integer(sents$count[9]*val),"Negative")})
          output$trstbox<- flexdashboard::renderGauge({gaugemaker(as.integer(sents$count[8]*val),"Trust")})
          output$sadbox<- flexdashboard::renderGauge({gaugemaker(as.integer(sents$count[6]*val),"Sad")})
          output$surbox<- flexdashboard::renderGauge({gaugemaker(as.integer(sents$count[7]*val),"Surprise")})
          output$joybox<- flexdashboard::renderGauge({gaugemaker(as.integer(sents$count[5]*val),"Joy")})
          
          colnames(dff)[6]<-("ss")
          m<-m%>% clearMarkers()
          m<-m %>% addMarkers(data =  cbind(longg,lattt),popup =paste(sep = "<br/>",
                                                                      paste("<b>",dff$Postedby,"</b>"),paste("<b>Sentiment:</b>",
                                                                                                             dff$ss)
          ))
        })
        
      })
       
 
      ###hashtagorigin
      observeEvent(input$hobutton,{
        temp<-gsub("#","",input$hoinput)
        system(paste("java -jar findorigin.jar ",temp,sep = ""))
        a<-read.csv(paste("data\\hash_",temp,".csv",sep=""),stringsAsFactors = FALSE)
        a<-data.frame(a)
        output$hotable<-renderDataTable({a})
      })
      
      #tRENDS
      
      observeEvent(input$apitwtrbutton1,{
        output$apitwtrtable1<-renderDataTable(  availtrends(place=isolate(input$apitwtrinput1)))
        
      
      })
      
      observeEvent(input$apitwtrbutton2,{
        output$apitwtrtable2<-renderDataTable(  trends(placen=isolate(input$apitwtrinput2)))
        
        
      })
     
       mm3<-leaflet() %>%addTiles() %>%setView(80, 22.5, zoom = 4)
      observe({output$mymap2<-renderLeaflet(mm3)})
      
      observeEvent(input$mymap2_click,{
        if(GLOBAL$mmflag==F)
        {
          #m3<-m3%>%addMarkers()
          GLOBAL$mmflag=T
        }
        else
        {
          mm3<-mm3%>% clearMarkers()
        }
          click <- input$mymap2_click
          mm3<-mm3 %>% addMarkers(lng=click$lng,lat=click$lat,popup =paste(sep = "<br/>",paste("<b>",click$lat,"</b>"),paste("<b>",click$lng,"</b>")))
          output$mymap2<-renderLeaflet(mm3) 
          output$apittthing<-renderUI(
            
              fluidRow(
                column(5,textInput("thelatapi",label=NULL,value=click$lng,placeholder = "Lat")),
                column(5,textInput("thelateapi",label=NULL,value=click$lat,placeholder = "Long")),
                column(2,actionButton("ggbutton",label=NULL,icon=icon("arrow-circle-right")))
              )
            
          )
        
        
      })
      observeEvent(input$ggbutton,{
        
        output$maptwtrtable<-renderDataTable({mytrends(num=1000 ,lat=input$thelatapi,long=input$thelateapi,rad="500km")})
        
      })
      
      ###srge
      
      observeEvent(input$gsdbtton,{
        abcd<-isolate(input$gsdlocation)
        vec<-unlist(strsplit(abcd,","))
        tempfun(kw=isolate(input$gsdinpt),loc=vec)
        output$ggsg1<-renderTable(ret1())
        output$ggsg2<-renderTable(ret2())
          output$ggsp1<-renderPlot(webtp())
        output$ggsp2<-renderTable(webtpc())
      })
      
      observeEvent(input$ysdbtton,{
        abcd<-isolate(input$ysdlocation)
        vec<-unlist(strsplit(abcd,","))
        youtubetrd(kw=isolate(input$ysdinpt),loc=vec)
        output$ygsg1<-renderTable(youtubedt())
        output$ygsg2<-renderTable(youtubeloc())
        output$ygsp1<-renderPlot(youtubefr())
        output$ygsp2<-renderTable(youtubeqwr())
      })
      
      observeEvent(input$nsdbtton,{
        abcd<-isolate(input$nsdlocation)
        vec<-unlist(strsplit(abcd,","))
        newstrd(kw=isolate(input$nsdinpt),loc=vec)
        output$ngsg1<-renderTable(newsdt())
        output$ngsg2<-renderTable(newsloc())
        output$ngsp1<-renderPlot(newsfr())
        output$ngsp2<-renderTable(newsqwr())
      })
      
     
      
      
      
    }
  })
})