library(shinydashboard)
library(leaflet)
library(bubbles)
library(flexdashboard)
gaugemaker<-function(a,b)
{
  gauge(a, min = 0, max = 100, symbol = '%', label =b,gaugeSectors(
    success = c(40, 0), warning = c(40,60), danger = c(60, 100)
  ))
}

clpl<-c("#DC143C","#00008B","#008000","#1E90FF","#FF8C00","#000000")
mlpl<-c("red","darkblue","darkgreen","lightblue","orange","black")
#clpl<-c("#FF0000","#00008B","#008000","#1E90FF","#FF8C00","#000000")

witheye<-function(i){
return( tags$div(actionButton(paste("ttbtton",i,sep=''),"",icon=icon("eye"),style="width:31%;"),actionButton(paste("ttclrbtn",i,sep=''),"",icon=icon("map-marker"),style="width:31%;"),actionButton(paste("ttfrrbtn",i,sep=''),"",icon=icon("users"),style="width:31%;")))}
withslashedeye<-function(i){
  return( tags$div(actionButton(paste("ttbtton",i,sep=''),"",icon=icon("eye-slash"),style="width:31%;"),actionButton(paste("ttclrbtn",i,sep=''),"",icon=icon("map-marker"),style="width:31%;"),actionButton(paste("ttfrrbtn",i,sep=''),"",icon=icon("users"),style="width:31%;")))}
myside<-list(
      sidebarMenu(
        ### SIDE BAR MENU
        menuItem("Keyword Analysis", icon = icon("search"),
                 
                 menuSubItem("Social Media", tabName = "kstab"),
                 
                 menuSubItem("Open Source", tabName = "opensourceka")
                 
        ),
        
        
        
        menuItem("Social Media Analyis", icon = icon("search"),
                 
                 menuSubItem("Trending Map", tabName = "maptwtrtab"),
                 
                 menuSubItem("Trending Api", tabName = "apitwtrtab"),
                 
                 menuSubItem ("Hashtag Origin", tabName = "hotab")
                 
        ),
        
        menuItem("Target Analysis", icon = icon("search"),
                 
                 menuSubItem("Track", tabName = "tttab"),
                 
                 menuSubItem("Analyze", tabName = "atttab")
                 
        ),
        
        menuItem("Bots Operation", icon = icon("file-text"),
                 
                 menuSubItem("Bot Post", tabName = "bptab"),
                 
                 menuSubItem("Bot Options", tabName = "opbptab")
                 
        ),
        
        menuItem("Surge Detection", icon = icon("line-chart"),
                 
                 menuSubItem("Google", tabName = "gsdtab"),
                 
                 menuSubItem("Youtube", tabName = "ysdtab"),
                 
                 menuSubItem("News", tabName = "nsdtab")
                 
                 
                 
        ),
        
        menuItem("Open Source Analysis", tabName = "opensourcea", icon = icon("file-text")),
        
        menuItem("Api Options",tabName="optab", icon = icon("cog"))
        
        
        
        
        
        
        )
      
    )
mybody<-list(
      tabItems(
        tabItem(
          tabName = "opbptab",
          fluidRow(
            box(width = 6,
                fluidRow(tableOutput("fbuserpass")),
                fluidRow(
                  tabBox(width=12,
                    title = tagList(shiny::icon("gear"), "Facebook"),
                    tabPanel("Add",
                            textInput("opbptabinput1",label=NULL,placeholder = "Username"),
                            textInput("opbptabinput2",label=NULL,placeholder = "Password"),
                            actionButton("opbptabbutton1",label="Add")
                            
                    ),
                    tabPanel("Delete", 
                             textInput("opbptabinput3",label=NULL,placeholder = "Username"),
                             actionButton("opbptabbutton2",label="Delete")
)
                  )
                )
                ),
            box(width = 6,
                fluidRow(tableOutput("twuserpass")),
                fluidRow(
                  tabBox(width=12,
                         title = tagList(shiny::icon("gear"), "Twitter"),
                         tabPanel("Add",
                                  textInput("opbptabinput4",label=NULL,placeholder = "Username"),
                                  textInput("opbptabinput5",label=NULL,placeholder = "Password"),
                                  actionButton("opbptabbutton3",label="Add")
                                  
                         ),
                         tabPanel("Delete", 
                                  textInput("opbptabinput6",label=NULL,placeholder = "Username"),
                                  actionButton("opbptabbutton4",label="Delete")
                         )
                  )
                )
            )
          )
        ),
    
tabItem(tabName="opensourcea",
        
        fluidRow(
          box(width=12,fluidRow(
            column(11,textInput("oainput",label=NULL,placeholder = "UserName")),
            column(1,actionButton(inputId="oabutton",label="",icon=icon("arrow-circle-right")))
          ))
        )
        ,
        fluidRow(
          box(title = "",width = 12,dataTableOutput("oatable"))
        )
        
        
        
),
        tabItem(tabName="apitwtrtab",
                fluidRow(
                  column(6,
                fluidRow(
                  box(width=12,fluidRow(
                    column(10,textInput("apitwtrinput1",label=NULL,placeholder = "Country")),
                    column(1,actionButton(inputId="apitwtrbutton1",label="",icon=icon("arrow-circle-right")))
                  ))
                )
                ,
                fluidRow(
                  box(title = "",width = 12,dataTableOutput("apitwtrtable1"))
                ))
                ,
                column(6,
                fluidRow(
                  box(width=12,fluidRow(
                    column(10,textInput("apitwtrinput2",label=NULL,placeholder = "WOE ID")),
                    column(1,actionButton(inputId="apitwtrbutton2",label="",icon=icon("arrow-circle-right")))
                  ))
                )
                ,
                fluidRow(
                  box(title = "",width = 12,dataTableOutput("apitwtrtable2"))
                ))
                
                
                )
                
        ),
            tabItem(
          
          tabName = "maptwtrtab",
          fluidRow(
            column(6,
                   
                  fluidRow( box(width=12,leafletOutput("mymap2",height="480px"),height = "500px")),
                  fluidRow(
               # column(5,offset=4,dateRangeInput("maptwtrdaterange",format = "dd-mm-yyyy",start="12-12-2016",label=NULL)))
                    column(10,offset=1,uiOutput("apittthing")))   
                   
                   ),
            box(width = 6,dataTableOutput("maptwtrtable")
                
                )
          )
        ),
        
        tabItem(tabName="hotab",
                
                fluidRow(
                  box(width=12,fluidRow(
                    column(11,textInput("hoinput",label=NULL,placeholder = "#HashTag")),
                    column(1,actionButton(inputId="hobutton",label="",icon=icon("arrow-circle-right")))
                  ))
                )
                ,
                fluidRow(
                  box(title = "",width = 12,dataTableOutput("hotable"))
                )

          
                    
        ),
        
        tabItem(tabName = "atttab",
                ##Search
                fluidRow(
                  
                  box(width=5,fluidRow(
                    column(6,textInput("attinput1",label="Handler",placeholder = "@TwitterHandler")),
                    column(4,numericInput("attinput2",label="Tweets",value = 500,min = 50,max=1500,step=50)),
                    column(2,actionButton("attbutton1","",icon=icon("arrow-circle-right"))))),
                  
                  box(width=7,fluidRow(
                    column(4,selectInput("attselect",label="Plot",choices = c("Actor Network","Activity Plot","Bimodal Network","Semantic Analysis"))),
                    column(6,
                           conditionalPanel(
                             condition = "input.attselect == 'Activity Plot'",
                             fluidRow()),
                           conditionalPanel(
                             condition = "input.attselect == 'Actor Network'",
                             numericInput("attinputa1",label="Filter",value = 4,min = 0,max=100)),
                           conditionalPanel(
                             condition = "input.attselect == 'Bimodal Network'",
                             numericInput("attinputb1",label="Filter",value = 4,min = 0,max=100)),
                           conditionalPanel(
                             condition = "input.attselect == 'Semantic Analysis'",
                             fluidRow(
                               column(4,numericInput("attinputc1",label="Filter",value = 4,min = 0,max=100)),
                               column(4,numericInput("attinputc2",label="HashTagFreq",value = 80,min = 0,max=100,step = 5)),
                               column(4,numericInput("attinputc3",label="TermFreq",value = 20,min = 0,max=100,step = 5))
                            ))
                           ),
                    column(2,actionButton("attbutton2","",icon=icon("arrow-circle-right")))
                  ))
                ),
                fluidRow(
                  
                  box(width=12,status = "warning" ,plotOutput("attout")))
                
                
                
        )
        ,
        
        tabItem(tabName = "optab",
                fluidRow(
                  column(7, box(width = 12,title = "Saved APIs",solidHeader = TRUE,status = "warning",tableOutput("apitable"))),
                  column(5,
                         
                         
                         fluidRow(
                           box(width = 12,title = "Add API",solidHeader = TRUE,status = "primary",
                               
                               textInput(inputId = "newapiapikey",label=NULL,placeholder = "API_KEY"),
                               textInput(inputId = "newapiapisecret",label=NULL,placeholder = "API_SECRET"),
                               textInput(inputId = "newapiapitoken",label=NULL,placeholder = "TOKEN"),
                               textInput(inputId = "newapiapitokensecret",label=NULL,placeholder = "TOKEN_SECRET"),
                               fluidRow(column(width=6,textInput(inputId = "newapiapiname",label=NULL,placeholder = "API_NAME")),
                                        column(width=6,actionButton(inputId="addnewapibutton",label = "ADD",width = "100%")))
                               )
                         ),
                         fluidRow(
                           box(width = 12,title = "Delete API",solidHeader = TRUE,background = "red",
                               fluidRow(column(width=6, textInput(inputId = "deleteapiname",label=NULL,placeholder = "API_NAME"))
                                        ,column(width=6,actionButton(inputId="deleteapibutton",label = "DELETE",width = "100%")))
                           )
                         ),
                         fluidRow(
                           box(width = 12,title = "Select API",solidHeader = TRUE,background = "green",
                               
                               fluidRow(column(width=6,numericInput(inputId = "selectapiname",label=NULL,value = 1,min=1))
                                        ,column(width=6, actionButton(inputId="selectapibutton",label = "SELECT",width = "100%")))
                               
                              
                               
                           )
                         )
                         )
                  
                  
                )),
        tabItem(tabName="tttab",
                 tags$div(leafletOutput("mymap3",height="88vh"),style="height:100%;width:100%;padding:0px;margin:0px;"
                          ,
                          tags$style(HTML("
                                          #controls{
                                          cursor:move;opacity:0.1;zoom:0.9;transition :opacity 500ms 1s;
                                                                        }
                                          
                                          #controls:hover{
                                            opacity:0.95;transition-delay:0;
                                          }
                                          ")),
                          absolutePanel(id="controls",draggable = F,fixed=T,top=70,right=20,left="auto",bottom = "auto",width = 350,height="auto",
                                        style="margin:0px;padding:0px;",
                                         
                                         fluidRow(style="margin:0px;",
                                           tags$div(
                                             tags$div(style="width:60%;float:left;",
                                             textInput("ttinpt1",label=NULL,placeholder = "@TwitterHandler",width = "100%")),
                                             uiOutput("viewbt1",style="width:40%;float:right;padding-left:3px;"),
                                            tags$br(),tags$br(),style=paste("background-color: ",clpl[1],";padding:6px;",sep=''))
                                         ,
                                         tags$div(
                                           tags$div(style="width:60%;float:left;",
                                                    textInput("ttinpt2",label=NULL,placeholder = "@TwitterHandler",width = "100%")),
                                           uiOutput("viewbt2",style="width:40%;float:right;padding-left:3px;")
                                           ,tags$br(),tags$br(),style=paste("background-color: ",clpl[2],";padding:6px;",sep=''))
                                         ,
                                         tags$div(
                                           tags$div(style="width:60%;float:left;",
                                                    textInput("ttinpt3",label=NULL,placeholder = "@TwitterHandler",width = "100%")),
                                           uiOutput("viewbt3",style="width:40%;float:right;padding-left:3px;")
                                           ,tags$br(),tags$br(),style=paste("background-color: ",clpl[3],";padding:6px;",sep=''))
                                         ,
                                         tags$div(
                                           tags$div(style="width:60%;float:left;",
                                                    textInput("ttinpt4",label=NULL,placeholder = "@TwitterHandler",width = "100%")),
                                           uiOutput("viewbt4",style="width:40%;float:right;padding-left:3px;")
                                           ,tags$br(),tags$br(),style=paste("background-color: ",clpl[4],";padding:6px;",sep=''))
                                         ,
                                         tags$div(
                                           tags$div(style="width:60%;float:left;",
                                                    textInput("ttinpt5",label=NULL,placeholder = "@TwitterHandler",width = "100%")),
                                           uiOutput("viewbt5",style="width:40%;float:right;padding-left:3px;")
                                           ,tags$br(),tags$br(),style=paste("background-color: ",clpl[5],";padding:6px;",sep=''))
                                         ,
                                         tags$div(
                                           tags$div(style="width:60%;float:left;",
                                                    textInput("ttinpt6",label=NULL,placeholder = "@TwitterHandler",width = "100%")),
                                           uiOutput("viewbt6",style="width:40%;float:right;padding-left:3px;")
                                           ,tags$br(),tags$br(),style=paste("background-color: ",clpl[6],";padding:6px;",sep=''))
                                         ,
                                         
                                          tags$div(
                                          actionButton("ttallbtton","  Show All",icon=icon("eye"),style="width:49%;height:40px;"),
                                          actionButton("ttclrallbtn","  Update (U)",icon=icon("map-marker"),style="width:23%;height:40px;"),
                                          actionButton("ttfrrallbtn","  Update (F)",icon=icon("users"),style="width:23%;height:40px;")
                                         )
                                         
                          )
                          
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         )
                          )
                ),
        tabItem(tabName = "gsdtab",
                ##Search
                fluidRow(
                  box(width=12,fluidRow(
                    column(3,textInput("gsdinpt",label=NULL,placeholder = "Enter")),
                    column(5,dateRangeInput("gsddate",format = "dd-mm-yyyy",start="12-12-2016",label=NULL)),
                    column(2,textInput("gsdlocation",label=NULL,placeholder = "Location")),
                    column(1,actionButton("gsdbtton","",icon=icon("arrow-circle-right")))
                  ))
                ),
                fluidRow(
                  
                  box(width=6,title = "G Surge Graph 1",solidHeader = TRUE,status = "warning" ,tableOutput("ggsg1")),
                  box(width=6,title = "G Surge Graph 2",solidHeader = TRUE,status = "warning",tableOutput("ggsg2"))),
                fluidRow(
                  box(width=12,title = "G Surge Plot 1",solidHeader = TRUE,status = "primary",plotOutput("ggsp1"))
                  
                )
                
                
                
        )
        ,
        tabItem(tabName = "ysdtab",
                ##Search
                fluidRow(
                  box(width=12,fluidRow(
                    column(3,textInput("ysdinpt",label=NULL,placeholder = "Enter")),
                    column(5,dateRangeInput("ysddate",format = "dd-mm-yyyy",start="12-12-2016",label=NULL)),
                    column(2,textInput("ysdlocation",label=NULL,placeholder = "Location")),
                    column(1,actionButton("ysdbtton","",icon=icon("arrow-circle-right")))
                  ))
                ),
                fluidRow(
                  
                  box(width=6,title = "G Surge Graph 1",solidHeader = TRUE,status = "warning" ,tableOutput("ygsg1")),
                  box(width=6,title = "G Surge Graph 2",solidHeader = TRUE,status = "warning",tableOutput("ygsg2"))),
                fluidRow(
                  box(width=12,title = "G Surge Plot 1",solidHeader = TRUE,status = "primary",plotOutput("ygsp1"))
                  #box(width=6,title = "G Surge Plot 2",solidHeader = TRUE,status = "primary",tableOutput("ygsp2"))
                  
                )
                
                
                
        ),
        tabItem(tabName = "nsdtab",
                ##Search
                fluidRow(
                  box(width=12,fluidRow(
                    column(3,textInput("nsdinpt",label=NULL,placeholder = "Enter")),
                    column(5,dateRangeInput("nsddate",format = "dd-mm-yyyy",start="12-12-2016",label=NULL)),
                    column(2,textInput("nsdlocation",label=NULL,placeholder = "Location")),
                    column(1,actionButton("nsdbtton","",icon=icon("arrow-circle-right")))
                  ))
                ),
                fluidRow(
                  
                  box(width=6,title = "G Surge Graph 1",solidHeader = TRUE,status = "warning" ,tableOutput("ngsg1")),
                  box(width=6,title = "G Surge Graph 2",solidHeader = TRUE,status = "warning",tableOutput("ngsg2"))),
                fluidRow(
                  box(width=12,title = "G Surge Plot 1",solidHeader = TRUE,status = "primary",plotOutput("ngsp1"))
                 # box(width=6,title = "G Surge Plot 2",solidHeader = TRUE,status = "primary",tableOutput("ngsp2"))
                  
                )
                
                
                
        )
        ,  tabItem(tabName = "bptab",
                ##Search
                fluidRow(
                  column(6,
                         box(width = 12,background ='black',title="Twitter Post",wellPanel(
                           fluidRow(column(width=12,textAreaInput(inputId="twin", label=NULL, width = "100%", height = "200px",placeholder = "Type Text Here.."))),
                           fluidRow(column(width=12,tags$div(style="color:#000000",tableOutput("twpostfileinout")))),
                           fluidRow(column(width=12,fileInput("twpostfilein",width="100%", label=NULL,placeholder = "Upload Files..."))),
                           fluidRow(column(width=12, actionButton("posttwi","Tweet",width ="100%" )))
                           
                             ))),
                  column(6,
                         box(width = 12,background ='blue',title="Facebook Post",wellPanel(
                           fluidRow(column(width=12,textAreaInput(inputId="fbin", label=NULL, width = "100%", height = "200px",placeholder = "Type Text Here.."))),
                           fluidRow(column(width=12,tags$div(style="color:#000000",tableOutput("fbpostfileinout")))),
                           fluidRow(column(width=12,fileInput("fbpostfilein",width="100%", label=NULL,placeholder = "Upload Files..."))),
                           fluidRow(column(width=12,actionButton("postfb","Post",width ="100%" )))
                         ))))
                
        )

        ,
        
        tabItem(tabName = "kstab",
                ##Search
                
                
                fluidRow(
                  box(width=12,fluidRow(
                    column(11,textInput("ksinpt",label=NULL,placeholder = "Enter")),
                    column(1,actionButton(inputId="ksbtton",label="",icon=icon("arrow-circle-right")))
                  ))
                )
                ,
                
                ##Below Map
                
                
                fluidRow(
                  column(width=2,
                         fluidRow(column(12,flexdashboard::gaugeOutput("angerbox",width='auto',height='auto'))),
                         fluidRow(column(12,flexdashboard::gaugeOutput("anticipationbox",width='auto',height='auto'))),
                         fluidRow(column(12,flexdashboard::gaugeOutput("disgustbox",width='auto',height='auto'))),
                         fluidRow(column(12,flexdashboard::gaugeOutput("fearbox",width='auto',height='auto')))),
                  box(width = 7,leafletOutput("mymap",height="380px"),height = "400px"),
                  column(width=3,
                         fluidRow(
                           box(width = 12,title = "Options",collapsible = TRUE,collapsed = TRUE,wellPanel(
                             fluidRow(textInput("kslocation",label=NULL,placeholder = "Location")),
                             fluidRow(dateRangeInput("ksdaterange",format = "dd-mm-yyyy",start="12-12-2016",label=NULL)),
                             fluidRow(textInput("ksnuminputstotake",label=NULL,placeholder = "Quantity"))
                           )             
                           
                           )),fluidRow(
                             box(width = 12,title = "Download",collapsible = TRUE,collapsed = TRUE,wellPanel(
                               fluidRow(radioButtons("ksdftype", "Dataframe type:",choices = c("Excel (CSV)", "Text (TSV)","Doc"))),
                               fluidRow(radioButtons("ksimgtype", "Image type:",choices = c("Jpeg", "Png"))),
                               fluidRow(downloadButton("ksdownload",'Download'),
                                        actionButton("kspload",icon = icon("cloud-upload"),"Upload",width ="110px" )
                               ))             
                               
                             ))
                  )
                  
                ),
                ### Five Reamaining Sents
                fluidRow(
                  column(2,flexdashboard::gaugeOutput("joybox",width='auto',height='auto')),
                  column(2,flexdashboard::gaugeOutput("posbox",width='auto',height='auto')),
                  column(2,flexdashboard::gaugeOutput("negbox",width='auto',height='auto')),
                  column(2,flexdashboard::gaugeOutput("surbox",width='auto',height='auto')),
                  column(2,flexdashboard::gaugeOutput("trstbox",width='auto',height='auto')),
                  column(2,flexdashboard::gaugeOutput("sadbox",width='auto',height='auto'))
                  
                  
                )
                ,
                fluidRow(
                  
                  box(width=6,status = "warning" ,plotOutput("kstabnetwork")),
                  box(width=6,status = "warning" ,plotOutput("kstabbimodal"))
                  ),
                fluidRow(
                  box(title = "Data :",width = 12,dataTableOutput("pivot"))
                )
                
                
        )
        
      )
      
      
    )
         