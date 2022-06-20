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

#summary statistics
my.summary <- function(x, na.rm=TRUE){
  result <- c(Mean=mean(x, na.rm=na.rm),
              SD=sd(x, na.rm=na.rm))
}
ind <- sapply(data, is.numeric)
sapply(data[, ind], my.summary)  

#cor
d <- round(cor(data[, ind]),digits = 2 )
d <- data.frame(round(cor(data[, ind]),digits = 2 ))

install.packages("gtExtras")
library(gtExtras)
head(d)
