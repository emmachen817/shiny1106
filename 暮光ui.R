library(shiny)
library(shinyIncubator)
require(rCharts)
library(rHighcharts)
shinyUI(navbarPage(
    
  
  title = h3(strong('功能選取')),
  
  
  tabPanel(h3(strong('首頁')),
           titlePanel(h1("大數據下應用文字探勘技術結合R於shiny建立之多功能平台以暮光之城與台灣五都美食為例")),
           br(),
           sidebarLayout(
             sidebarPanel(h2("指導老師:謝邦昌教授"), br(),img(src = "my.jpg", height = 400, width = 400) ,width = 3),           
             
             mainPanel(h2('動機'),
                       h3("  在大數據的時代下，網路日益進步與發達，各式各樣的資訊皆可在網路上取得，
                          透過社群媒體網站亦能將重要訊息快速而廣泛地傳播。而資訊的呈現除了傳統書面外，
                          電子化亦是盛行的一種方式，不僅書本轉變為電子書，新聞報紙也逐漸由網路新聞所取代。"),
                       
                       h3(p("  然而，時常閱讀大量網路訊息及文章不僅費時又耗力，因此，",
                            span("我們利用文字探勘技術，建立一個資訊閱讀平台，讓讀者能夠利用平台，
                               達到不需閱讀整篇文章或網頁，即能從中獲得有用的資訊。",
                                 style = "color:dodgerblue") )),
                       br(),
                       h2("目的"),
                       h3("  利用",
                          span("Shiny界面",style = "color:dodgerblue"),
                          "建置一平台，讓網路使用者能夠匯入文本或文章資訊。
                        再透過平台建立之",
                          span("文字探勘技術",style = "color:dodgerblue"),
                          "，呈現",
                          span("詞雲、集群、脈絡分析等圖",style = "color:dodgerblue"),
                          "，讓使用者能夠減少閱讀時間，
                        有效率的了解所匯入文本想傳達的資訊。本研究以",
                          span("暮光之城為例",style = "color:dodgerblue"),
                          "，找出在這些地區有哪些網友推薦的美食。" ),                 
                       
                       h3("  除此之外也建置了",
                          span("爬文及分析系統",style = "color:dodgerblue"),
                          "，
                        使用者能在平台上分析Facebook粉絲專頁。而透過",
                          span("詞雲字的大小、關聯分析",style = "color:dodgerblue"),
                          "得知網友推薦或相關評論。本研究以",
                          span("台灣地區五大都市Facebook美食相關粉絲專頁為例",style = "color:dodgerblue"),
                          "，找出在這些地區有哪些網友推薦的美食。")
                                                                     
                       )
             ,position = c("right"))
  ),
  
  
  tabPanel(h3(strong('詞雲')),
           titlePanel(h2(strong("詞雲展示"))),
           sidebarLayout(
             sidebarPanel(
               fileInput('file1', 'Choose File',accept=c('text/txt','.txt')),
               sliderInput("freq", "字詞頻率控制:",min = 2, max = 30, value = 20),
               sliderInput("max","字詞展示個數:",min = 5, max = 50, value = 30),
               downloadButton('wf', 'DownloadWordFreq')
             ),
             mainPanel(
               plotOutput("plot"),chartOutput("baplot"))
             ,position = c("right"))
  ),
  tabPanel(h3(strong('集群')),
           titlePanel(h2(strong("集群展示"))),
           sidebarLayout(
             mainPanel(plotOutput("clplot", height = 1000, width = 1000)),
             sidebarPanel(
               sliderInput("pz", "集群字體大小",min = 0.5, max = 3, value = 1.23),
               numericInput("cn","集群群數",min=2,max=8,value=5),
               downloadButton('dc', 'Download')), fluid = TRUE)),
  
  tabPanel(h3(strong('脈絡分析')),
           titlePanel(h2(strong("脈絡分析"))),
           
           sidebarLayout(
           sidebarPanel(
             
             numericInput("k","主題數",min=5,max=50,value=10),
             downloadButton('logLiktest', 'DownloadlogLiktest'),
             downloadButton('topic', 'Downloadtopic')),
           
           mainPanel(plotOutput("storyplot",height = 1000, width = 1000))  , fluid = TRUE)),
  
  tabPanel(h3(strong('文字相關')),
           titlePanel(h2(strong("字詞相關展現"))),
           textInput("dd", "輸入:"),
           dataTableOutput(outputId="table"), fluid = TRUE) ,
  
  tabPanel(h3(strong('facebook爬文')),
           titlePanel(h2(strong("台灣美食"))),
           checkboxInput(inputId = "tp",
                         label = strong("台北"),
                         value = FALSE),
           checkboxInput(inputId = "tc",
                         label = strong("台中"),
                         value = FALSE),
           checkboxInput(inputId = "tn",
                         label = strong("台南"),
                         value = FALSE),
           checkboxInput(inputId = "hs",
                         label = strong("高雄"),
                         value = FALSE),
           sidebarLayout(sidebarPanel(
             
             sliderInput("mmax","字詞展示個數:",min = 1, max = 100, value = 100)
           ),
                         # Show a tabset that includes a plot, summary, and table view
                         # of the generated distribution
                         mainPanel(plotOutput("ptplot",height = 1500, width = 1000 )), fluid = TRUE)),
  
  tabPanel(h3(strong('爬文內容')),
           titlePanel(h2(strong("文章內容"))),
           textInput("name", "輸入:"),
           dataTableOutput(outputId="table2"), 
           tags$div(tags$a(href="https://www.google.com.tw/maps?hl=zh-TW&tab=wl", h3("連結Google map"))),fluid = TRUE)

  
))


