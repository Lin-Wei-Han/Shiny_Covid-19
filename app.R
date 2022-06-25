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
library(timevis)
library(lubridate)
library(DT)
library(tidyverse)

countries <- c('tw','gb','us')
img_urls <- paste0(
  'https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/',
  countries, '.svg'
)
data_directory = "data/"
timeLine = read.csv( file.path(data_directory, "event.csv"), stringsAsFactors = F, fileEncoding ="UTF-8")
timeLine <- timeLine %>% pivot_longer(-c(time),names_to = "group",values_to = "content")
timeLine$group <- recode(timeLine$group, 'tw_event' = "台灣", 'us_event' = "美國", 'gb_event' = "英國")
timeLine$time = as.Date( timeLine$time ,format="%Y-%m-%d")
time_data <- data.frame(
  id      = 1:531,
  content = timeLine$content,
  start   = timeLine$time,
  group = timeLine$group
)

timevisDataGroups <- data.frame(
  id = unique(timeLine$group),
  content = unique(timeLine$group)
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
    fluidPage(list(tags$head(HTML('<link rel="icon", href="coronavirus.png"/>')))),
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
                     plotlyOutput(outputId = "covidConfirmed", height = "850px")%>% withSpinner(color="#dd4b39")
            ),
            tabPanel("死亡人數", 
                     plotlyOutput(outputId = "covidDeaths", height = "850px")%>% withSpinner(color="#dd4b39")
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
            #tableOutput("values_AreaSale"),
            #verbatimTextOutput("summary_AreaSale"),
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
                    inputId = "growth",label = "銷售額年增率",status = "danger",value = TRUE
                  ),
                  awesomeCheckbox(
                    inputId = "unem",label = "失業率",status = "danger"
                  ),
                  awesomeCheckbox(
                    inputId = "ios",label = "油價",status = "danger"
                  ),
                  p("敘述性統計:",style = "font-weight:600"),
                  materialSwitch(inputId = "stat",value = TRUE,status = "danger"
                  )
                ),
                mainPanel(
                  hidden(plotlyOutput(outputId = "TaiwanAllplot_growth", height = "500px")),
                  hidden(plotlyOutput(outputId = "TaiwanAllplot_n_unem", height = "500px")),
                  hidden(plotlyOutput(outputId = "TaiwanAllplot_oil", height = "500px")),
                  dataTableOutput(outputId = "TaiwanAll_stat"),
                  #verbatimTextOutput("summary"),
                  style = "
                        padding-right:50px;
                        "
                )
              )
            ),
            tabPanel(
              "各國指標",
                fluidRow(
                  column(sliderInput("slider_worldall", "時間:",
                                     min = as.Date("2017-01-01"), max = as.Date("2021-11-01"), value = c(as.Date("2018-11-01"), as.Date("2020-11-01")),
                                     timeFormat = "%Y/%m",width=500
                  ),width=4),
                  column(
                    p("指標:",style = "font-weight:600"),
                    awesomeCheckbox(
                      inputId = "unemw",label = "失業率",status = "danger",value = TRUE
                    ),
                    awesomeCheckbox(
                      inputId = "iosw",label = "油價",status = "danger"
                    ),width=1),
                  column(
                    multiInput(
                      inputId = "Group_worldcountry", label = "國家 :", choices = NULL,selected = "tw",
                      choiceNames = lapply(
                        seq_along(countries),
                        function(i) {
                          tagList(
                            tags$img(src = img_urls[i], width = 30, height = 20),
                            
                          )
                        }
                      ),
                      choiceValues = countries
                    ),width=7)
                ),
              fluidRow(
                  hidden(plotlyOutput(outputId = "WorldUnemplot", height = "500px")),
                  hidden(plotlyOutput(outputId = "Worldoilplot", height = "500px")),
                  style = "padding-right:50px;"
                ),
              timevisOutput("timelineGroups"),
              #includeHTML('timeline.html'),
              #includeCSS('style.css'),
              #includeScript('javascript.js')
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
  
  observe({
    if (input$slider_taiwan_sale_industry[1] == input$slider_taiwan_sale_industry[2]){
      updateSliderInput(session = session, "slider_taiwan_sale_industry", value = c(input$slider_taiwan_sale_industry[1], input$slider_taiwan_sale_industry[1] %m+% months(3)),timeFormat = "%Y/%m")
    }     
  })
  
  output$TaiwanSaleplot = renderPlotly({
    plot_ly(TaiwanSaleArea(),x=~time,y=~total, color = ~industry ,colors = "Set2") %>% add_lines()%>% layout(xaxis = list(title = "時間以兩個月為一期"),
                                                                                                             yaxis = list (title = "銷售額（新台幣/元）"),font=t, margin = m)
    #ggplot(TaiwanSaleArea(), aes(x=time, y = total, colour =industry ,group = industry,shape=industry)) + 
    #  geom_line(size = 2)
  })
  
  #---------------------------------------------------
  #--------------------Taiwan_All---------------------
  #---------------------------------------------------
  sub_data <- reactive({
    data_directory = "data/"
    taiwan =  read.csv( file.path(data_directory, "tw_output.csv"), stringsAsFactors = F)
    #-------時間轉換--------
    taiwan$time = as.Date( taiwan$time ,format="%Y-%m-%d")
    sub_data <- subset(taiwan, taiwan$time>=as.Date(input$slider_taiwanall[1],format="%Y-%m-%d") &  taiwan$time<=as.Date(input$slider_taiwanall[2],format="%Y-%m-%d"))
    
  })
  #-------避免時間複選--------
  observe({
    if (input$slider_taiwanall[1] == input$slider_taiwanall[2]){
      updateSliderInput(session = session, "slider_taiwanall", value = c(input$slider_taiwanall[1], input$slider_taiwanall[1] %m+% months(3)),timeFormat = "%Y/%m")
    }     
  })
  #-------Y軸進階參數設定--------
  growth <- list(overlaying = "y",side = "right",title = "<b>銷售額年增率（%）</b>")
  unem <- list(overlaying = "y",side = "right",title = "<b>失業率（%）</b>")
  oil <- list(overlaying = "y",side = "right",title = "<b>油價（公升/元）</b>")
  m <- list(l = 50,r = 50,b = 50,t = 50,pad = 4)
  #-------繪圖--------
  output$TaiwanAllplot_growth = renderPlotly({
    plot_ly(sub_data(), x = ~time, y = ~total,name = "銷售額") %>% add_lines %>%
      add_lines(x = ~time, y = ~growth, mode = "lines",yaxis = "y2", name = "銷售額年增率")%>%
      layout(title = "<b>銷售額與銷售額年增率</b>",legend = list(orientation = 'h'),xaxis = list(title="時間以兩個月為一期"),yaxis = list(title="銷售額（新台幣/元）"),yaxis2 = growth,font=t, margin = m)
  })
  output$TaiwanAllplot_n_unem = renderPlotly({
    plot_ly(sub_data(), x = ~time, y = ~total,name = "銷售額") %>% add_lines %>%
      add_lines(x = ~time, y = ~n_unem, mode = "lines",yaxis = "y2", name = "失業率")%>%
      layout(title = "<b>銷售額與失業率</b>",legend = list(orientation = 'h'),xaxis = list(title="時間以兩個月為一期"),yaxis = list(title="銷售額（新台幣/元）"),yaxis2 = unem,font=t, margin = m)
  })
  output$TaiwanAllplot_oil = renderPlotly({
    plot_ly(sub_data(), x = ~time, y = ~total,name = "銷售額") %>% add_lines %>%
      add_lines(x = ~time, y = ~n_oil, mode = "lines",yaxis = "y2", name = "油價")%>%
      layout(title = "<b>銷售額與油價</b>",legend = list(orientation = 'h'),xaxis = list(title="時間以兩個月為一期"),yaxis = list(title="銷售額（新台幣/元）"),yaxis2 = oil,font=t, margin = m)
  })
  #-------敘述性統計--------
  #data_directory = "data/"
  #data =  read.csv( file.path(data_directory, "tw_output.csv"), stringsAsFactors = F)
  
  #summary statistics
  sub_stat <- reactive({
    data_directory = "data/"
    taiwan =  read.csv( file.path(data_directory, "tw_output.csv"), stringsAsFactors = F)
    #-------時間轉換--------
    taiwan$time = as.Date( taiwan$time ,format="%Y-%m-%d")
    taiwan <- subset(taiwan, taiwan$time>=as.Date(input$slider_taiwanall[1],format="%Y-%m-%d") &  taiwan$time<=as.Date(input$slider_taiwanall[2],format="%Y-%m-%d"))
    ind <- sapply(taiwan, is.numeric)
    sub_stat <- round(cor(taiwan[, ind]),digits = 2 )
  })
  output$TaiwanAll_stat <- renderDataTable({
    datatable(sub_stat(),filter='none', options = list(ordering = FALSE, autoWidth = TRUE,paging = FALSE,searching = FALSE
    ))
  })
  
  #-------選取事件判斷--------
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
  observeEvent(input$stat, {
    if(input$stat == TRUE){
      showElement("TaiwanAll_stat")
    }else{
      hideElement("TaiwanAll_stat")
    }
  })
  
  #---------------------------------------------------
  #--------------------World_All---------------------
  #---------------------------------------------------
  sub_world_data <- reactive({
    data_directory = "data/"
    world = read.csv( file.path(data_directory, "world_covid.csv"), stringsAsFactors = F)
    #-------時間轉換--------
    world$time = as.Date( world$time ,format="%Y-%m-%d")
    #----Max min scaler-----
    minmax_tra <- function(x, na.rm = TRUE) {
      return((x- min(x)) /(max(x)-min(x)))
    }
    world$case <- minmax_tra(world$case)
    
    sub_world_data <- subset(world, world$time>=as.Date(input$slider_worldall[1],format="%Y-%m-%d") &  world$time<=as.Date(input$slider_worldall[2],format="%Y-%m-%d")& 
                               world$country %in% input$Group_worldcountry)%>%
      mutate(country = recode(country, 'tw'='台灣', 'us'='美國', 'gb'='英國') )
  })
  
  observe({
    if (input$slider_worldall[1] == input$slider_worldall[2]){
      updateSliderInput(session = session, "slider_worldall", value = c(input$slider_worldall[1], input$slider_worldall[1] %m+% months(3)),timeFormat = "%Y/%m")
    }     
  })
  
  output$WorldAll_values <- renderTable({
    sub_world_data()
  })
  output$WorldAll_summary <- renderPrint({  
    input$Group_worldcountry
  })
  
  #-------選取事件判斷--------
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
  fixed_pal <- reactive({
    
    pal = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3") 
    names(pal) = c('台灣', '美國', '英國')
    Group_worldcountry_chinese = recode(input$Group_worldcountry, 'tw'='台灣', 'us'='美國', 'gb'='英國') 
    
    fixed_pal = pal[Group_worldcountry_chinese]
    
  })
  #plot_ly()%>%
  #  add_trace(data = stock, type = 'scatter', mode = 'lines', fill = 'tozeroy', x = ~date, y = ~GOOG, name = 'GOOG')
  case <- list(overlaying = "y",side = "right",title = "<b>確診人數</b>")
  
  output$WorldUnemplot = renderPlotly({
    plot_ly(sub_world_data(),x=~time,y=~n_unem, color = ~country, colors = fixed_pal()) %>% add_lines()%>%
      add_trace(x = ~time, y = ~case,type="scatter", mode = "lines", fill = 'tozeroy',yaxis = "y2", name = "確診人數")%>% 
      layout(title = "<b>各國失業率</b>",legend = list(x = 0.01, y = 1),xaxis = list(title = "時間以兩個月為一期"),yaxis = list (title = "失業率（%）"),yaxis2 = case,font=t, margin = m)
  })
  output$Worldoilplot = renderPlotly({
    plot_ly(sub_world_data(),x=~time,y=~n_oil, color = ~country, colors = fixed_pal()) %>% add_lines()%>%
      add_trace(x = ~time, y = ~case,type="scatter", mode = "lines", fill = 'tozeroy',yaxis = "y2", name = "確診人數")%>%
      layout(title = "<b>各國油價</b>",legend = list(x = 0.01, y = 1),xaxis = list(title = "時間以兩個月為一期"),yaxis = list (title = "油價（桶/元）"),yaxis2 = case,font=t, margin = m)
  })

  #------------------------------
  # time line
  #------------------------------
  selected_data <- reactive({
    test <- time_data[time_data$start %in% seq(from=min(as.Date(strftime(req(input$slider_worldall[[1]]), "%Y-%m-%d"))),
                                                 to=max(as.Date(strftime(req(input$slider_worldall[[2]]), "%Y-%m-%d"))), by = 0.02),]
  })

  
  output$timelineGroups <- renderTimevis({
    timevis(data = selected_data(), groups = timevisDataGroups, options = list(stack = FALSE))
  })
  #------------------------------
  # Confirmed
  #------------------------------
  #---df_confirmed資料---#
  df_confirmed <- read.csv(file = "data/worldMap/time_series_covid_19_confirmed.csv",sep =",")
  df_confirmed <- df_confirmed %>% rename(Country = "Country.Region") 
  
  Conf_wide_1 <- read.csv(file = "data/worldMap/Conf_wide1.csv",sep = ",")
  
  df_confirmed <- merge(df_confirmed,Conf_wide_1,by ="Country",all=T )
  df_confirmed <- df_confirmed %>% filter(Lat != "NA")
  #---df_deaths資料---#
  df_deaths <- read.csv(file = "data/worldMap/time_series_covid_19_deaths.csv",sep =",")
  df_deaths <- df_deaths %>% 
    rename(Country = "Country.Region") 
  Conf_wide_2 <- read.csv(file = "data/worldMap/Conf_wide2.csv",sep = ",")
  
  df_deaths <- merge(df_deaths,Conf_wide_2,by ="Country",all=T )
  df_deaths <- df_deaths %>% filter(Lat != "NA")
  
  codes <- read_csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv',
                    col_types = cols(
                      COUNTRY = col_character(),
                      `GDP (BILLIONS)` = col_double(),
                      CODE = col_character()
                    ))
  #---資料清理---#
  df_confirmed <- df_confirmed %>%
    pivot_longer( -c("Province.State", "Country", "Lat", "Long"),names_to = "Date",values_to = "Confirmed") 
  df_confirmed$Date <-  gsub("X","",df_confirmed$Date)
  df_confirmed <- separate(df_confirmed,Date,c("year","month","day")) 
  df_confirmed$year <-  as.integer(df_confirmed$year) 
  df_confirmed$month <-  as.integer(df_confirmed$month) 
  df_confirmed$day <-  as.integer(df_confirmed$day) 
  df_confirmed <- df_confirmed %>%  
    mutate(Date = make_datetime(year, month, day)) %>%
    select(-year,-month,-day)
  
  df_deaths <- df_deaths %>%
    pivot_longer( -c("Province.State", "Country", "Lat", "Long"),names_to = "Date",values_to = "Deaths") 
  df_deaths$Date <-  gsub("X","",df_deaths$Date)
  df_deaths <- separate(df_deaths,Date,c("year","month","day")) 
  df_deaths$year <-  as.integer(df_deaths$year) 
  df_deaths$month <-  as.integer(df_deaths$month) 
  df_deaths$day <-  as.integer(df_deaths$day) 
  df_deaths <- df_deaths %>%  
    mutate(Date = make_datetime(year, month, day)) %>%
    select(-year,-month,-day)
  
  #---total---#
  df_total <- df_confirmed %>%
    left_join(df_deaths) 
  
  ## We all know "Diamond Princess" and "MS Zaandam" are cruises, So we have to remove them from the data
  
  df_total <- df_total %>%
    filter(Country != "Diamond Princess") %>%
    filter(Country != "MS Zaandam")
  
  df_total$Deaths[is.na(df_total$Deaths)] <- 0
  
  ## Created a dataset including latest news of COVID-19
  
  cases_latest <- df_total %>%
    group_by(Country, Date) %>%
    summarise(Confirmed  = sum(Confirmed),
              Deaths = sum(Deaths)) %>%
    mutate("New Cases" = Confirmed - lag(Confirmed, 1) ) %>%
    filter(!is.na(Date)) %>%
    filter(Date == max(Date))
  
  day_latest <- max(cases_latest$Date)
  cases_total_date <- df_total %>%
    group_by(Date) %>%
    summarise(Confirmed = sum(Confirmed),
              Deaths = sum(Deaths)) %>%
    mutate("New_Cases" = Confirmed - lag(Confirmed, 1))
  
  cases_total_date$New_Cases[is.na(cases_total_date$New_Cases)] <- 0 
  
  cases_total_latest <- cases_total_date %>%
    filter(Date == max(Date))
  codes <- codes %>%
    select(COUNTRY, CODE) %>%
    rename(Region = COUNTRY ,
           Code = CODE) %>%
    rownames_to_column("id")
  
  codes$id <- as.integer(codes$id)
  
  ## Making sure countries's and regions' names are in line with other datasets.
  
  codes$Region <- codes$Region %>%
    str_replace(pattern = "United States", replacement = "US") %>%
    str_replace(pattern = "Macedonia", replacement = "North Macedonia") %>%
    str_replace(pattern = "Czech Republic", replacement = "Czechia") %>%
    str_replace(pattern = "Taiwan", replacement = "Taiwan*") %>%
    str_replace(pattern = "West Bank", replacement = "West Bank and Gaza") %>%
    str_replace(pattern = "Congo, Democratic Republic of the", replacement = "Congo (Kinshasa)") %>%
    str_replace(pattern = "Congo, Republic of the", replacement = "Congo (Brazzaville)") %>%
    str_replace(pattern = "Bahamas, The", replacement = "Bahamas") %>%
    str_replace(pattern = "Swaziland", replacement = "Eswatini") %>%
    str_replace(pattern = "Gambia, The", replacement = "Gambia")
  
  cases_latest_codes <- cases_latest %>%
    left_join(codes, by = c("Country" = "Region" )) %>%
    arrange(desc(Confirmed))
  
  ## Setting boundries' color as light grey
  
  line <- list(color = toRGB("#d1d1d1"), width = 0.2)
  
  ## Specifing parameters of the 3D map
  geo <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = 'orthographic'),
    resolution = '100',
    showcountries = TRUE,
    countrycolor = '#d1d1d1',
    showocean = TRUE,
    oceancolor = '#064273',
    showlakes = TRUE,
    lakecolor = '#99c0db',
    showrivers = TRUE,
    rivercolor = '#99c0db',
    bgcolor = '#e8f7fc')
  
  #cases_latest_codes <- read.csv(file = "data/worldMap/cases_latest_codes.csv",sep = ",")
  
  # light grey boundaries
  #l <- list(color = toRGB("grey"), width = 0.5)
  
  # specify map projection/options
  #g <- list(
  #  showframe = FALSE,
  #  showcoastlines = FALSE,
  #  projection = list(type = 'Mercator')
  #)
  output$covidConfirmed = renderPlotly({
    #plot_geo(cases_latest_codes) %>% add_trace(
    #  z = ~Confirmed, color = ~Confirmed, colors = 'Purples',
    #  text = ~Country, locations = ~Code, marker = list(line = l)
    #) %>% colorbar(title = '確診數') %>% layout(
    #  geo = g
    #)
    plot_geo() %>%
      layout(geo = geo,
             paper_bgcolor = '#e8f7fc',
             title = paste0("World COVID-19 Confirmed by Region at", day_latest)) %>%
      add_trace(data = cases_latest_codes,
                z = ~Confirmed,
                colors = "Reds",
                text = ~'Country',
                locations = ~Code,
                marker = list(line = line))
  })
  
  #------------------------------
  # deaths
  #------------------------------
  #df_deaths <- read.csv(file = "data/worldMap/time_series_covid_19_deaths.csv",sep =",")
  #df_deaths <- df_deaths %>% 
  #  rename(Country = "Country.Region") 
  #Conf_wide_2 <- read.csv(file = "data/worldMap/Conf_wide2.csv",sep = ",")
  
  #df_deaths <- merge(df_deaths,Conf_wide_2,by ="Country",all=T )
  #df_deaths <- df_deaths %>% filter(Lat != "NA")
  
  g <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = 'Mercator')
  )
  
  output$covidDeaths = renderPlotly({
    #plot_geo(cases_latest_codes) %>% add_trace(
    #  z = ~Deaths, color = ~Deaths, colors = 'Reds',
    #  text = ~Country, locations = ~Code, marker = list(line = l)
    #) %>% colorbar(title = '死亡數') %>% layout(
    #  geo = g
    #)
    plot_geo() %>%
      layout(geo = geo,
             paper_bgcolor = '#e8f7fc',
             title = paste0("World COVID-19 Deaths by Region at", day_latest)) %>%
      add_trace(data = cases_latest_codes,
                z = ~Deaths,
                colors = "Reds",
                text = ~'Country',
                locations = ~Code,
                marker = list(line = line))
  })
  
})




shinyApp(ui = ui, server = server)