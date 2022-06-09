library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2) 
library(readr)
library(dplyr)
library(tidyr)
library(showtext)
library(plotly)
library(ggthemes)
library(maps)
library(shinyjs)
library(shinycssloaders)

countries <- c('tw','gb','us')
img_urls <- paste0(
  'https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/',
  countries, '.svg'
)
y2 <- list(
  tickfont = list(color = "#2ca02c"),
  titlefont = list(color = "#2ca02c"),
  overlaying = "y",
  side = "right",
  anchor="free",
  position=1)
y3 <- list(
  tickfont = list(color = "#ff9130"),
  titlefont = list(color = "#ff9130"),
  overlaying = "y",
  side = "right")
y4 <- list(
  tickfont = list(color = "#d62728"),
  titlefont = list(color = "#d62728"),
  overlaying = "y",
  side = "right",
  anchor="free",
  position=-1)

ui <- dashboardPage(
  skin = "red",
  
  # Header
  dashboardHeader(title = "Covid19 Dashboard"),
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "首頁",
        icon = icon("home", style = "padding-right:40px"), tabName = "Firstpage"
      ),
      menuItem(
        "關於我們",
        icon = icon("marker", style = "padding-right:40px"), tabName = "about"
      ),
      menuItem(
        "台灣銷售額",
        icon = icon("table", style = "padding-right:40px"), tabName = "TAIWAN"
      ),
      menuItem(
        "各國指標",
        icon = icon("globe", style = "padding-right:40px"), tabName = "world"
      )
    )
  ),
  # body
  dashboardBody(
    useShinyjs(),
    tags$style(HTML("
                     .main-header {height:20px}
                     .main-sidebar {
                           font-size: 25px!important;
                           background-color:red}
                     .container-fluid{
                           margin-left:-20px;}
                     .main-header .navbar{
                     height:20px
                     }
                     .sidebar-toggle{height:50px;}
                    ")),
    # Boxes need to be put in a row (or column)
    tabItems(
      tabItem(
        tabName = "Firstpage",
        tags$div(" ", style = "padding-top:0px"),
        tags$style(HTML("
                   .nav-tabs-custom a{font-size:25px}
                    ")),
        fluidRow(
          #valueBox(15*200,"全球確診人數",icon = icon("hourglass-3")),
        ),
        fluidRow(
          tabBox(
            title = " ",width = 100,
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tabset1", height = "300px",
            tabPanel("確診人數",
                     plotlyOutput(outputId = "covidConfirmed", height = "750px")%>% withSpinner(color="#dd4b39")
            ),
            tabPanel("死亡人數", 
                     plotlyOutput(outputId = "covidDeaths", height = "750px")%>% withSpinner(color="#dd4b39")
            )
          )
          
        )

      ),
      tabItem(
        tabName = "about",
        fluidPage(
          fluidRow(
            img(src = 'top.jpg',width = "110%",style="margin-top:-20px")
          ),
        ),
        h1('TSDC 5th',style = "font-size:50px;
                             font-weight: 600;
                             margin-top:40px;
                             margin-left:100px;
                             margin-bottom:50px;
                             background:white;
                             border-width: 5px; 
                             border-style:solid ;
                             width: 250px;
                             border-color: rgb(37, 37, 37); 
                             padding: 5px;
                             text-align: center;
                             border-radius: 50px;
                             "),
        tags$div(' ',style="width:100%;
                       height: 4px;
                       background: rgb(37, 37, 37);
                       position: relative;
                       display: block;
                       margin: auto;
                       width:90%;
            "),
        tags$div(
          p('專案背景',style="@import url('https://fonts.googleapis.com/css2?family=Noto+Sans+TC:wght@300;400;700&display=swap');
                        font-size:35px;
                        font-weight:700;
                        margin-left:50px;
                        margin-bottom:20px;
                        margin-top:50px;
                        font-family: 'Noto Sans TC', sans-serif;
          "),
          p('2019年年末，首例新冠肺炎案例出現於中國境內。時至今日，肺炎已蔓延至全球各地.。
            2021年的五月，新冠肺炎的問題也在台灣爆發，本土單日的確診案例從雙位數飆升至200多人，
            距離四級警戒的全台封城可說是只差一步。而台灣利用不到半年的時間，從三級警戒達到零確診的紀錄。',
            style="
          font-size:20px;
          margin-left:50px;
          margin-bottom:40px;
          font-family: 'Noto Sans TC', sans-serif;
          "),
          p('從產業角度來看，各大行業也造成了巨大的影響，交通層面上，禁止了非本國籍的乘客進入，
            因此飛機和大眾交通工具的使用上明顯比以往減少了許多。餐飲的方面，伴隨著不同區域的嚴重程度，
            也有禁止內用、採用梅花座、定期消毒等規定，依各家餐廳是否提供外帶，也影響著店家的收益。',
            style="
          font-size:20px;
          margin-left:50px;
          margin-bottom:60px;
          font-family: 'Noto Sans TC', sans-serif;
          "),
          p('專案動機',style="
                        font-size:35px;
                        font-weight:700;
                        margin-left:50px;
                        margin-bottom:20px;
                        font-family: 'Noto Sans TC', sans-serif;
          "),
          p(' 無獨有偶，據全球經貿報導指出，旅遊業出現了大量退房和退票的情況。部分工廠也因疫情影響而延後復工復產。',
            style="
          font-size:20px;
          margin-left:50px;
          margin-bottom:40px;
          font-family: 'Noto Sans TC', sans-serif;
          "),
          p('然而，根據網路新聞所提供的手機簡訊調查，有將近66%民眾的收入受到疫情影響，而在防疫期間隨著確診人數的增長、
            或者警戒等級的調升，衛福部也祭出更嚴謹的疫情政策。其中又以餐飲業、觀光業、旅遊業甚為嚴重。 ',
            style="
          font-size:20px;
          margin-left:50px;
          margin-bottom:40px;
          font-family: 'Noto Sans TC', sans-serif;
          "),
          p('一直以來交通、餐飲、旅遊等行業皆和我們的生活密不可分，在疫情下，許多民眾皆減少了這部分的需求。
            而在生活或是消費上，民眾也因應了疫情政策而有了不同以往的消費模式。 我們希望能夠透過專案的發想與研究，
            深入探討疫情對於各個常見的行業有何影響。 ',
            style="
          font-size:20px;
          margin-left:50px;
          margin-bottom:40px;
          font-family: 'Noto Sans TC', sans-serif;
          "),
          style="display: block;
                 margin: auto;
                 width:90%"
        )
      ),
      tabItem(
        tabName = "TAIWAN",
        tags$div(" ", style = "padding-top:50px;margin-left:100px;"),
        sidebarLayout(
          sidebarPanel(
            setSliderColor(c("#dd4b39", "#dd4b39", "#dd4b39", "#dd4b39"), c(1, 2, 3, 4)),
            sliderInput("slider_taiwan_sale_industry", "時間:",
                        min = as.Date("2017-01-01"), max = as.Date("2021-11-01"), value = c(as.Date("2018-11-01"), as.Date("2020-11-01")),
                        timeFormat = "%Y/%m"
            ),
            pickerInput(
              inputId = "areataiwan_sale_industry", label = "縣市",
              choices = c(
                "宜蘭縣", "花蓮縣", "金門縣", "南投縣", "屏東縣",
                "苗栗縣", "桃園市", "高雄市", "基隆市", "連江縣",
                "雲林縣", "新北市", "新竹市", "新竹縣", "嘉義市",
                "嘉義縣", "彰化縣", "臺中市", "臺北市", "臺東縣",
                "臺南市", "澎湖縣"
              ),
              options = list(title = "1456"), selected = "新北市"
            ),
            awesomeCheckboxGroup(
              inputId = "tw_sale_industry", label = "產業", choices = c(
                "農、林、漁、牧業","製造業","不動產業","批發及零售業","運輸及倉儲業","住宿及餐飲業","金融及保險業",
                "教育業","醫療保健及社會工作服務業","藝術、娛樂及休閒服務業"
              ),
              selected = "農、林、漁、牧業", status = "danger"
            )
          ),
          mainPanel(
            plotlyOutput(outputId = "TaiwanSaleplot", height = "500px")%>% withSpinner(color="#dd4b39"),
            #hidden(box(id = "warning",
            #  width = 12,height = 500, background = "yellow",
            #  "A box with a solid black background",style = "font-size:40px;text-align: center;padding-top:230px"
            #)),
            tableOutput("values_AreaSale"),
            verbatimTextOutput("summary_AreaSale"),
            style = "padding-right:50px;"
          )
        )
      ),
      tabItem(
        tabName = "world",
        tags$div(
          #tabsetPanel(
          tabBox(
            width = 70,id = "tabset1",
            tabPanel(
              "台灣指標",
              tags$style(HTML("
                     .col-sm-8 div{
                        margin-bottom:40px;
                     }
                    ")),
              sidebarLayout(
                sidebarPanel(
                  sliderInput("slider_taiwanall", "時間：",
                              min = as.Date("2017-01-01"), max = as.Date("2021-11-01"), value = c(as.Date("2018-11-01"), as.Date("2020-11-01")),
                              timeFormat = "%Y/%m"
                  ),
                  #awesomeCheckboxGroup(
                  #  inputId = "Group_taiwanall", label = "指標（與銷售額比較）：", choices = c("銷售額年增率" = "growth", "失業率" = "n_unem", "油價" = "n_oil"),
                  #  selected = "total", status = "danger"
                  #),
                  p("指標（與銷售額比較）:",style = "font-weight:600"),
                  awesomeCheckbox(
                    inputId = "growth",label = "銷售額年增率",status = "danger"
                  ),
                  awesomeCheckbox(
                    inputId = "unem",label = "失業率",status = "danger"
                  ),
                  awesomeCheckbox(
                    inputId = "ios",label = "油價",status = "danger"
                  )
                ),
                mainPanel(
                  hidden(plotlyOutput(outputId = "TaiwanAllplot_growth", height = "500px")),
                  hidden(plotlyOutput(outputId = "TaiwanAllplot_n_unem", height = "500px")),
                  hidden(plotlyOutput(outputId = "TaiwanAllplot_oil", height = "500px")),
                  tableOutput("values"),
                  verbatimTextOutput("summary"),
                  style = "
                        padding-right:50px;
                        "
                )
              )
            ),
            tabPanel(
              "各國指標",
              sidebarLayout(
                sidebarPanel(
                  sliderInput("slider_worldall", "時間:",
                              min = as.Date("2017-01-01"), max = as.Date("2021-11-01"), value = c(as.Date("2018-11-01"), as.Date("2020-11-01")),
                              timeFormat = "%Y/%m"
                  ),
                  p("指標:",style = "font-weight:600"),
                  awesomeCheckbox(
                    inputId = "unemw",label = "失業率",status = "danger",value = TRUE
                  ),
                  awesomeCheckbox(
                    inputId = "iosw",label = "油價",status = "danger"
                  ),
                  awesomeCheckbox(
                    inputId = "case",label = "確診人數",status = "danger"
                  ),
                  multiInput(
                    inputId = "Group_worldcountry", label = "國家 :", choices = NULL,selected = "tw",
                    choiceNames = lapply(
                      seq_along(countries),
                      function(i) {
                        tagList(
                          tags$img(src = img_urls[i], width = 50, height = 30),
                          
                        )
                      }
                    ),
                    choiceValues = countries
                  )
                ),
                mainPanel(
                  hidden(plotlyOutput(outputId = "WorldUnemplot", height = "500px")),
                  hidden(plotlyOutput(outputId = "Worldoilplot", height = "500px")),
                  hidden(plotlyOutput(outputId = "Worldcaseplot", height = "500px")),
                  style = "padding-right:50px;"
                )
              ),
              includeHTML('timeline.html'),
              includeCSS('style.css'),
              includeScript('javascript.js')
            )
          ),
          style = "margin-top:50px;margin-left:10px"
        ),
      )
    )
  )
)

server <- shinyServer(function(input, output, session){
  autoInvalidate <- reactiveTimer(intervalMs = 50*1000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
  t <- list(
    family = "Microsoft JhengHei",
    size = 14)
  #---------------------------------------------------
  #--------------------Taiwan_Sale--------------------
  #---------------------------------------------------
  
  TaiwanSaleArea <- reactive({
    data_directory = "data/"
    TaiwanSaleIndustry =  read.csv( file.path(data_directory, "tw_sale_industry.csv"), stringsAsFactors = F, fileEncoding ="UTF-8")
    #---------------------
    TaiwanSaleIndustry$time = as.Date( TaiwanSaleIndustry$time ,format="%Y-%m-%d")
    TaiwanSaleArea <- subset(TaiwanSaleIndustry, TaiwanSaleIndustry$time>=as.Date(input$slider_taiwan_sale_industry[1],format="%Y-%m-%d") & TaiwanSaleIndustry$time<=as.Date(input$slider_taiwan_sale_industry[2],format="%Y-%m-%d")& 
                               TaiwanSaleIndustry$industry %in% input$tw_sale_industry&
                               TaiwanSaleIndustry$city == input$areataiwan_sale_industry)
    
  })
  output$values_AreaSale <- renderTable({
    TaiwanSaleArea()
  })
  output$summary_AreaSale <- renderPrint({  
    input$tw_sale_industry
  })
  
  output$TaiwanSaleplot = renderPlotly({
    plot_ly(TaiwanSaleArea(),x=~time,y=~total, color = ~industry ,colors = "Set2") %>% add_lines()%>% layout(xaxis = list(title = "時間以兩個月為一期"),
                                                                                                             yaxis = list (title = "銷售額"),font=t, margin = m)
    #ggplot(TaiwanSaleArea(), aes(x=time, y = total, colour =industry ,group = industry,shape=industry)) + 
    #  geom_line(size = 2)
  })
  
  #---------------------------------------------------
  #--------------------Taiwan_All---------------------
  #---------------------------------------------------
  sub_data <- reactive({
    data_directory = "data/"
    taiwan =  read.csv( file.path(data_directory, "tw_output.csv"), stringsAsFactors = F)
    #---------------------
    taiwan$time = as.Date( taiwan$time ,format="%Y-%m-%d")
    #taiwan <- taiwan %>% pivot_longer(-time,names_to = "index",values_to = "data")
    sub_data <- subset(taiwan, taiwan$time>=as.Date(input$slider_taiwanall[1],format="%Y-%m-%d") &  taiwan$time<=as.Date(input$slider_taiwanall[2],format="%Y-%m-%d"))
    
  })
  
  output$values <- renderTable({
    sub_data()
  })
  output$summary <- renderPrint({  
    input$oil
  })
  
  growth <- list(overlaying = "y",side = "right",title = "<b>銷售額年增率</b>")
  unem <- list(overlaying = "y",side = "right",title = "<b>失業率</b>")
  oil <- list(overlaying = "y",side = "right",title = "<b>油價</b>")
  m <- list(l = 50,r = 50,b = 50,t = 50,pad = 4)
  
  output$TaiwanAllplot_growth = renderPlotly({
    plot_ly(sub_data(), x = ~time, y = ~total,name = "銷售額") %>% add_lines %>%
      add_lines(x = ~time, y = ~growth, mode = "lines",yaxis = "y2", name = "銷售額年增率")%>%
      layout(title = "<b>銷售額與銷售額年增率</b>",xaxis = list(title="時間以兩個月為一期"),yaxis = list(title="銷售額"),yaxis2 = growth,font=t, margin = m)
  })
  output$TaiwanAllplot_n_unem = renderPlotly({
    plot_ly(sub_data(), x = ~time, y = ~total,name = "銷售額") %>% add_lines %>%
      add_lines(x = ~time, y = ~n_unem, mode = "lines",yaxis = "y2", name = "失業率")%>%
      layout(title = "<b>銷售額與失業率</b>",xaxis = list(title="時間以兩個月為一期"),yaxis = list(title="銷售額"),yaxis2 = unem,font=t, margin = m)
  })
  output$TaiwanAllplot_oil = renderPlotly({
    plot_ly(sub_data(), x = ~time, y = ~total,name = "銷售額") %>% add_lines %>%
      add_lines(x = ~time, y = ~n_oil, mode = "lines",yaxis = "y2", name = "油價")%>%
      layout(title = "<b>銷售額與油價</b>",xaxis = list(title="時間以兩個月為一期"),yaxis = list(title="銷售額"),yaxis2 = oil,font=t, margin = m)
  })
  observeEvent(input$growth, {
    if(input$growth == TRUE){
      showElement("TaiwanAllplot_growth")
    }else{
      hideElement("TaiwanAllplot_growth")
    }
  })
  observeEvent(input$unem, {
    if(input$unem == TRUE){
      showElement("TaiwanAllplot_n_unem")
    }else{
      hideElement("TaiwanAllplot_n_unem")
    }
  })
  observeEvent(input$ios, {
    if(input$ios == TRUE){
      showElement("TaiwanAllplot_oil")
    }else{
      hideElement("TaiwanAllplot_oil")
    }
  })

  #---------------------------------------------------
  #--------------------World_All---------------------
  #---------------------------------------------------
  #setwd("C:/tsdc/5th/Output")
  sub_world_data <- reactive({
    data_directory = "data/"
    world = read.csv( file.path(data_directory, "world_covid.csv"), stringsAsFactors = F)
    #world$time = as.Date(paste(world$time,"/01",sep = ""),"%Y/%m/%d")
    #data_directory = "data/"
    #world =  read.csv( file.path(data_directory, "uk_all.csv"), stringsAsFactors = F)
    #---------------------
    world$time = as.Date( world$time ,format="%Y-%m-%d")
    #world <- world %>% pivot_longer(-c(time,country),names_to = "index",values_to = "data")
    #world <- world %>% pivot_longer(-c(time,country),names_to = "index",values_to = "data")
    sub_world_data <- subset(world, world$time>=as.Date(input$slider_worldall[1],format="%Y-%m-%d") &  world$time<=as.Date(input$slider_worldall[2],format="%Y-%m-%d")& 
                               #world$index %in%"n_unem" &
                               #world$n_unem <-  input$Group_worldall=="失業率" &
                               #world$n_oil <-  input$Group_worldall=="油價" &
                               world$country %in% input$Group_worldcountry)
  })
  
  output$WorldAll_values <- renderTable({
    sub_world_data()
  })
  output$WorldAll_summary <- renderPrint({  
    input$Group_worldcountry
  })
  
  observeEvent(input$unemw, {
    if(input$unemw == TRUE){
      showElement("WorldUnemplot")
    }else{
      hideElement("WorldUnemplot")
    }
  })
  observeEvent(input$iosw, {
    if(input$iosw == TRUE){
      showElement("Worldoilplot")
    }else{
      hideElement("Worldoilplot")
    }
  })
  observeEvent(input$case, {
    if(input$case == TRUE){
      showElement("Worldcaseplot")
    }else{
      hideElement("Worldcaseplot")
    }
  })
  #plot_ly()%>%
  #  add_trace(data = stock, type = 'scatter', mode = 'lines', fill = 'tozeroy', x = ~date, y = ~GOOG, name = 'GOOG')
  
  output$WorldUnemplot = renderPlotly({
    plot_ly(sub_world_data(),x=~time,y=~n_unem, color = ~country, colors = "Set2") %>% add_lines()%>%
      add_trace(x = ~time, y = ~case, mode = "lines", fill = 'tozeroy',yaxis = "y2", name = "確診人數")%>% 
      layout(title = "<b>各國失業率</b>",xaxis = list(title = "時間以兩個月為一期"),yaxis = list (title = "失業率"),yaxis2 = unem,font=t, margin = m)
  })
  output$Worldoilplot = renderPlotly({
    plot_ly(sub_world_data(),x=~time,y=~n_oil, color = ~country, colors = "Set2") %>% add_lines()%>% layout(title = "<b>各國油價</b>",
                                                                                                            xaxis = list(title = "時間以兩個月為一期"),
                                                                                                            yaxis = list (title = "油價"),font=t, margin = m)
  })
  output$Worldcaseplot = renderPlotly({
    plot_ly(sub_world_data(),x=~time,y=~case, color = ~country, colors = "Set2", type = 'scatter', mode = 'lines', fill = 'tozeroy') %>% layout(title = "<b>各國確診人數</b>",
                                                                                                            xaxis = list(title = "時間以兩個月為一期"),
                                                                                                            yaxis = list (title = "確診人數"),font=t, margin = m)
  })
  
  #------------------------------
  # Confirmed
  #------------------------------
  
  df_confirmed <- read.csv(file = "data/worldMap/time_series_covid_19_confirmed.csv",sep =",")
  df_confirmed <- df_confirmed %>% rename(Country = "Country.Region") 
  
  Conf_wide_1 <- read.csv(file = "data/worldMap/Conf_wide1.csv",sep = ",")
  
  df_confirmed <- merge(df_confirmed,Conf_wide_1,by ="Country",all=T )
  df_confirmed <- df_confirmed %>% filter(Lat != "NA")
  
  world <- ggplot() +
    borders("world", colour = "gray85", fill = "gray80") +
    theme_map()
  
  data <- filter(df_confirmed,df_confirmed[,ncol(df_confirmed)]>0)
  Count <- as.integer(unlist(df_confirmed[,ncol(df_confirmed)]))
  
  map_Conf <- world +
    geom_point(aes(x = Long, y = Lat, size = Count,name= Country),
               data = df_confirmed, 
               colour = 'purple', alpha = .5) +
    scale_size_continuous(range = c(1,8), 
                          breaks = c(250, 500, 750, 1000)) +
    labs(size = 'Cases')
  output$covidConfirmed = renderPlotly({
    ggplotly(map_Conf, tooltip = c('Count','Country'))
  })
  
  #------------------------------
  # deaths
  #------------------------------
  df_deaths <- read.csv(file = "data/worldMap/time_series_covid_19_deaths.csv",sep =",")
  df_deaths <- df_deaths %>% 
    rename(Country = "Country.Region") 
  Conf_wide_2 <- read.csv(file = "data/worldMap/Conf_wide2.csv",sep = ",")
  
  df_deaths <- merge(df_deaths,Conf_wide_2,by ="Country",all=T )
  df_deaths <- df_deaths %>% filter(Lat != "NA")
  
  data <- filter(df_deaths,df_deaths[,ncol(df_deaths)]>0)
  Countdeath <- as.integer(unlist(data[,ncol(df_deaths)]))
  
  map_death <- world +
    geom_point(aes(x = Long, y = Lat, size = Countdeath, name= Country),
               data = data, 
               colour = 'red', alpha = .5) +
    scale_size_continuous(range = c(1, 8), 
                          breaks = c(250, 500, 750, 1000)) +
    labs(size = 'Cases')
  output$covidDeaths = renderPlotly({
    ggplotly(map_death, tooltip = c('Countdeath','country'))
  })
  
})




shinyApp(ui = ui, server = server)