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
        "Home",
        icon = icon("home", style = "padding-right:40px"), tabName = "Firstpage"
      ),
      menuItem(
        "About",
        icon = icon("marker", style = "padding-right:40px"), tabName = "about"
      ),
      menuItem(
        "Taiwan data",
        icon = icon("table", style = "padding-right:40px"), tabName = "TAIWAN"
      ),
      menuItem(
        "World",
        icon = icon("globe", style = "padding-right:40px"), tabName = "world"
      )
    )
  ),
  # body
  dashboardBody(
    tags$style(HTML(".main-header {height:20px}
                     .main-sidebar {
                           font-size: 25px!important;
                           background-color:red}
                     .container-fluid{
                           margin-left:-20px;

                    ")),
    # Boxes need to be put in a row (or column)
    tabItems(
      tabItem(
        tabName = "Firstpage",
        tags$div(" ", style = "padding-top:35px"),
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
                     plotlyOutput(outputId = "covidConfirmed", height = "750px")
            ),
            tabPanel("死亡人數", 
                     plotlyOutput(outputId = "covidDeaths", height = "750px")
            )
          )
          
        )

      ),
      tabItem(
        tabName = "TAIWAN",
        tags$div(" ", style = "padding-top:112px;margin-left:100px;"),
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
            plotlyOutput(outputId = "TaiwanSaleplot", height = "500px"),
            tableOutput("values_AreaSale"),
            verbatimTextOutput("summary_AreaSale"),
            style = "padding-right:50px;"
          )
        )
      ),
      tabItem(
        tabName = "world",
        tags$div(
          tabsetPanel(
            tabPanel(
              "台灣指標",
              sidebarLayout(
                sidebarPanel(
                  sliderInput("slider_taiwanall", "時間:",
                              min = as.Date("2017-01-01"), max = as.Date("2021-11-01"), value = c(as.Date("2018-11-01"), as.Date("2020-11-01")),
                              timeFormat = "%Y/%m"
                  ),
                  awesomeCheckboxGroup(
                    inputId = "Group_taiwanall", label = "123", choices = c("銷售額" = "total", "銷售額年增率" = "growth", "失業率" = "n_unem", "油價" = "n_oil"),
                    selected = "total", status = "danger"
                  ),
                ),
                mainPanel(
                  plotlyOutput(outputId = "TaiwanAllplot", height = "500px"),
                  plotlyOutput(outputId = "TaiwanAllplot_growth", height = "500px"),
                  plotlyOutput(outputId = "TaiwanAllplot_n_unem", height = "500px"),
                  plotlyOutput(outputId = "TaiwanAllplot_oil", height = "500px"),
                  plotlyOutput(outputId = "TaiwanAllplot_test", height = "500px"),
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
                  awesomeCheckboxGroup(
                    inputId = "Group_worldall", label = "指標", choices = c("失業率" = "n_unem", "油價" = "n_oil"),
                    selected = "銷售額"
                  ),
                  multiInput(
                    inputId = "Group_worldcountry", label = "國家 :", choices = NULL,
                    choiceNames = lapply(
                      seq_along(countries),
                      function(i) {
                        tagList(
                          tags$img(src = img_urls[i], width = 50, height = 30),
                          countries[i]
                        )
                      }
                    ),
                    choiceValues = countries
                  )
                ),
                mainPanel(
                  plotlyOutput(outputId = "WorldAllplot", height = "500px"),
                  tableOutput("WorldAll_values"),
                  verbatimTextOutput("WorldAll_summary"),
                  style = "padding-right:50px;"
                )
              )
            )
          ),
          style = "margin-top:70px;margin-left:40px"
        ),
      )
    )
  )
)

server <- shinyServer(function(input, output, session){
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
    plot_ly(TaiwanSaleArea(),x=~time,y=~total, color = ~industry ,colors = "Set2") %>% add_lines()
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
    input$Group_taiwanall
  })
  
  output$TaiwanAllplot = renderPlotly({
    plot_ly(sub_data(), x = ~time, y = ~total,name = "銷售額") %>% add_lines %>%
      add_lines(x = ~time, y = ~n_unem, mode = "lines",yaxis = "y2", name = "失業率")%>%
      layout(title = "<b>銷售額與失業率</b>",yaxis2 = list(overlaying = "y", side = "right"))
  })
  output$TaiwanAllplot_growth = renderPlotly({
    plot_ly(sub_data(), x = ~time, y = ~total,name = "銷售額") %>% add_lines %>%
      add_lines(x = ~time, y = ~growth, mode = "lines",yaxis = "y2", name = "銷售額年增率")%>%
      layout(title = "<b>銷售額與銷售額年增率</b>",yaxis2 = list(overlaying = "y", side = "right"))
  })
  output$TaiwanAllplot_n_unem = renderPlotly({
    plot_ly(sub_data(), x = ~time, y = ~total,name = "銷售額") %>% add_lines %>%
      add_lines(x = ~time, y = ~n_unem, mode = "lines",yaxis = "y2", name = "失業率")%>%
      layout(title = "<b>銷售額與失業率</b>",yaxis2 = list(overlaying = "y", side = "right"))
  })
  output$TaiwanAllplot_oil = renderPlotly({
    plot_ly(sub_data(), x = ~time, y = ~total,name = "銷售額") %>% add_lines %>%
      add_lines(x = ~time, y = ~n_oil, mode = "lines",yaxis = "y2", name = "油價")%>%
      layout(title = "<b>銷售額與油價</b>",yaxis2 = list(overlaying = "y", side = "right"))
  })
  output$TaiwanAllplot_test = renderPlotly({
    plot_ly(sub_data(), x = ~time, y = ~total,name = "銷售額") %>% add_lines %>%
      add_lines(x = ~time, y = ~growth, mode = "lines",yaxis = "y2", name = "銷售額年增率")%>%
      add_lines(x = ~time, y = ~n_unem, mode = "lines",yaxis = "y3", name = "失業率")%>%
      add_lines(x = ~time, y = ~n_oil, mode = "lines",yaxis = "y4", name = "油價")%>%
      layout(
        title = "台灣指標比較", yaxis2 = y2, yaxis3 = y3, yaxis4 = y4,
        xaxis = list(domain = c(0.1, 0.95))
        )
      
  })
  
  #---------------------------------------------------
  #--------------------World_All---------------------
  #---------------------------------------------------
  #setwd("C:/tsdc/5th/Output")
  sub_world_data <- reactive({
    data_directory = "data/"
    world = read.csv( file.path(data_directory, "world.csv"), stringsAsFactors = F)
    #world$time = as.Date(paste(world$time,"/01",sep = ""),"%Y/%m/%d")
    #data_directory = "data/"
    #world =  read.csv( file.path(data_directory, "uk_all.csv"), stringsAsFactors = F)
    #---------------------
    world$time = as.Date( world$time ,format="%Y-%m-%d")
    world <- world %>% pivot_longer(-c(time,country),names_to = "index",values_to = "data")
    #world <- world %>% pivot_longer(-c(time,country),names_to = "index",values_to = "data")
    sub_world_data <- subset(world, world$time>=as.Date(input$slider_worldall[1],format="%Y-%m-%d") &  world$time<=as.Date(input$slider_worldall[2],format="%Y-%m-%d")& 
                               world$index %in%"n_unem" &
                               #world$n_unem <-  input$Group_worldall=="失業率" &
                               #world$n_oil <-  input$Group_worldall=="油價" &
                               world$country %in% input$Group_worldcountry)
  })
  sub_world_data2 <- reactive({
    data_directory = "data/"
    world = read.csv( file.path(data_directory, "world.csv"), stringsAsFactors = F)
    world$time = as.Date( world$time ,format="%Y-%m-%d")
    world <- world %>% pivot_longer(-c(time,country),names_to = "index",values_to = "data")
    sub_world_data2 <- subset(world, world$time>=as.Date(input$slider_worldall[1],format="%Y-%m-%d") &  world$time<=as.Date(input$slider_worldall[2],format="%Y-%m-%d")& 
                                world$index %in%"n_oil" &
                                world$country %in% input$Group_worldcountry)
  })
  
  output$WorldAll_values <- renderTable({
    sub_world_data()
  })
  output$WorldAll_summary <- renderPrint({  
    input$Group_worldcountry
  })
  
  output$WorldAllplot = renderPlotly({
    if (all(input$Group_worldall == c("n_unem","n_oil")))  {
      subplot(
        plot_ly(sub_world_data(),x=~time,y=~data, color = ~country, colors = "Set2") %>% add_lines(),
        plot_ly(sub_world_data2(),x=~time,y=~data, color = ~country, colors = "Set2") %>% add_lines(), nrows = 2
      )
    }  
    else if (all(input$Group_worldall == "n_unem"))  {
      plot_ly(sub_world_data(),x=~time,y=~data, color = ~country, colors = "Set2") %>% add_lines()
    }
    else if (all(input$Group_worldall == "n_oil"))  {
      plot_ly(sub_world_data2(),x=~time,y=~data, color = ~country, colors = "Set2") %>% add_lines()
    }
    
    
    #plot_ly(sub_world_data(),x=~time) %>%
    #  add_lines(x=input$slider_worldall,y=input$Group_worldall,name="input$Group_worldall")%>%
    #  add_lines(x=input$slider_worldall,y=input$Group_worldall,name="input$Group_worldall",yaxis="y2")%>%
    #  layout(
    #    title="y",yaxis2=a,xaxis=list(title="period")
    #  )
    
    #ggplot(sub_world_data(), aes(x=time, y = data, colour =country ,group = country,shape=country)) + 
    #geom_line(size = 2)
  })
  
  #------------------------------
  # Confirmed
  #------------------------------
  
  df_confirmed <- read.csv(file = "C:/Users/FX505/Downloads/time_series_covid_19_confirmed.csv",sep =",")
  df_confirmed <- df_confirmed %>% rename(Country = "Country.Region") 
  
  Conf_wide_1 <- read.csv(file = "C:/Users/FX505/Downloads/Conf_wide1.csv",sep = ",")
  
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
  df_deaths <- read.csv(file = "C:/Users/FX505/Downloads/time_series_covid_19_deaths.csv",sep =",")
  df_deaths <- df_deaths %>% 
    rename(country = "Country.Region") 
  
  data <- filter(df_deaths,df_deaths[,ncol(df_deaths)]>0)
  Countdeath <- as.integer(unlist(data[,ncol(df_deaths)]))
  
  map_death <- world +
    geom_point(aes(x = Long, y = Lat, size = Countdeath, name= country),
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