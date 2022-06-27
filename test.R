data_directory = "data/"
world = read.csv( file.path(data_directory, "world_covid.csv"), stringsAsFactors = F)
#X_std = ((world$case - min(world$case)) / (max(world$case) - min(world$case)))
world$case <- minmax_tra(world$case)

plot_ly(world,x=~time,y=~n_unem, color = ~country) %>% add_lines()%>%
  add_trace(x = ~time, y = ~case,type="scatter", mode = "lines", fill = 'tozeroy',yaxis = "y2", name = "確診人數")%>% 
  layout(title = "<b>各國失業率</b>",legend = list(x = 0.01, y = 1),xaxis = list(title = "時間以兩個月為一期"),yaxis = list (title = "失業率（%）"))

minmax_tra <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

#---------------------

data_directory = "D:/Shiny_Covid-19/data/"
data =  read.csv( file.path(data_directory, "tw_output.csv"), stringsAsFactors = F)
data <- data[,-5]
data <- select(data,time)
#summary statistics
my.summary <- function(x, na.rm=TRUE){
  result <- c(Mean=mean(x, na.rm=na.rm),
              SD=sd(x, na.rm=na.rm))
}
ind <- sapply(data, is.numeric)
s <- sapply(data[, ind], my.summary) %>%
  rename(
    mean = "平均數",
    SD = "標準差"
  ) 
s <- row.names(s,c("平均數","標準差"))
rownames(s) <- c("平均數","標準差")
#cor
d <- round(cor(data[, ind]),digits = 2 )
d <- data.frame(round(cor(data[, ind]),digits = 2 ))

data <- setNames(data, c("time","失業率","油價","PM2.5","銷售額年增率","銷售額"))

#---------------------
t <- list(
  family = "Microsoft JhengHei",
  size = 14)
m <- list(l = 50,r = 50,b = 50,t = 50,pad = 4)
data_directory = "D:/Shiny_Covid-19/data/"
tw_sale_industry =  read.csv( file.path(data_directory, "tw_sale_industry.csv"), stringsAsFactors = F, fileEncoding ="UTF-8")
tw_sale_industry <- subset(tw_sale_industry,tw_sale_industry$industry == "住宿及餐飲業")
tw_sale_industry <- subset(tw_sale_industry,tw_sale_industry$city == "新北市")

plot_ly(tw_sale_industry,x=~time,y=~total, color = ~industry ,colors = "Set2") %>% add_lines()%>% layout(xaxis = list(title = "時間以兩個月為一期"),
                                                                                                         yaxis = list (title = "銷售額（新台幣/元）"),font=t, margin = m)
#---------------------
unem <- list(overlaying = "y",side = "right",title = "<b>失業率（%）</b>")

tw_output =  read.csv( file.path(data_directory, "tw_output.csv"), stringsAsFactors = F)

plot_ly(tw_output, x = ~time, y = ~total,name = "銷售額") %>% add_lines %>%
  add_lines(x = ~time, y = ~n_unem, mode = "lines",yaxis = "y2", name = "失業率")%>%
  layout(title = "<b>銷售額與失業率</b>",legend = list(orientation = 'h'),xaxis = list(title="時間以兩個月為一期"),yaxis = list(title="銷售額（新台幣/元）"),yaxis2 = unem,font=t, margin = m)

#---------------------
world = read.csv( file.path(data_directory, "world_covid.csv"), stringsAsFactors = F)
world$country <-  recode(world$country, 'tw'='台灣', 'us'='美國', 'gb'='英國') 
#fixed_pal = pal[Group_worldcountry_chinese]
  
case <- list(overlaying = "y",side = "right",title = "<b>確診人數</b>")

plot_ly(world,x=~time,y=~n_unem, color = ~country) %>% add_lines()%>%
  add_trace(x = ~time, y = ~case,type="scatter", mode = "lines", fill = 'tozeroy',yaxis = "y2", name = "確診人數")%>% 
  layout(title = "<b>各國失業率</b>",legend = list(x = 0.01, y = 1),xaxis = list(title = "時間以兩個月為一期"),yaxis = list (title = "失業率（%）"),yaxis2 = case,font=t, margin = m)

  