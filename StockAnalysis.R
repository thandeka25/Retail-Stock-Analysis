#load the packages
require(rio)
require(lubridate)
require(tidyverse)
require(plotly)

#import the datasets, remove null values and 
uniqloDF <- rio::import("FRCOY.csv") %>% filter(Open != 'null') %>%  #uniqlo
  mutate(Date = as.Date(Date)) %>% mutate(Stock = "UNIQ")
luluDF <- rio::import("LULU.csv") %>% filter(Open != 'null') %>%    #Lululemon
  mutate(Date = as.Date(Date)) %>% mutate(Stock = "LULU")
shopDF <- rio::import("SHOP.csv") %>% filter(Open != 'null') %>%    #Shopify
  mutate(Date = as.Date(Date)) %>% mutate(Stock = "SHOP")         
wmtDF <- rio::import("WMT.csv") %>% filter(Open != 'null') %>%     #Walmart
  mutate(Date = as.Date(Date)) %>% mutate(Stock = "WMT")
tgtDF <- rio::import("TGT.csv") %>% filter(Open != 'null') %>%     #Target
  mutate(Date = as.Date(Date)) %>% mutate(Stock = "TGT")
costDF <- rio::import("COST.csv") %>% filter(Open != 'null') %>%     #Costco
  mutate(Date = as.Date(Date)) %>% mutate(Stock = "COST")

#bind rows to create a combined data frame
comDF <- bind_rows(uniqloDF, luluDF) %>% bind_rows(., shopDF) %>% 
  bind_rows(., wmtDF) %>% bind_rows(., tgtDF) %>% bind_rows(., costDF) %>% 
  mutate(Stock = as.factor(Stock))
str(comDF)

#transform data to create an end of month table
#last day data
lastDay <- comDF   %>% group_by(Year=year(Date), Month=month(Date), Stock) %>% 
  filter(Date == max(Date)) %>% 
  select(Close)
#first day data
firstDay <- comDF   %>% group_by(Year=year(Date), Month=month(Date), Stock) %>% 
  filter(Date == min(Date)) %>% 
  select(Open)
#monthly volume data
monthVol <- comDF %>% group_by(Year=year(Date), Month=month(Date), Stock) %>%summarise(TotVolume = sum(Volume))
#combined end of month data
monthData <- inner_join(lastDay, firstDay, by = c( 'Year', 'Month', 'Stock' )) %>% 
  inner_join(., monthVol, by = c( 'Year', 'Month', 'Stock' ))   %>% 
  mutate(Incr = Close - Open) %>% 
  mutate(PercInc = 100*Incr/Open)

#stocks and their biggest increase in a month
monthData %>% group_by(Stock) %>% filter(PercInc == max(PercInc)) %>% arrange(desc(PercInc))
#stocks and their highest volume in a month
monthData %>% group_by(Stock) %>% filter(TotVolume == max(TotVolume)) %>% arrange(desc(TotVolume))
monthData %>% group_by(Stock) %>% filter(TotVolume == min(TotVolume)) %>% arrange(TotVolume)


#get maximum deviation from five year mean volume
comDF %>% mutate(DiffPrice = Close - Open)  %>% mutate(DiffPricePerc = 100*DiffPrice/Open)  %>%  
  group_by(Stock) %>% mutate(AverageVolume = mean(Volume)) %>% 
  mutate(DiffVolume = Volume - AverageVolume) %>% mutate(DiffVolumePerc = 100*DiffVolume/AverageVolume) %>%
  arrange(desc(DiffVolume))


#moving average formula
mAverage <-  function(x, n){stats::filter(x, rep(1 / n, n), sides = 2)}


#levers
StartDate <- "2017-02-26"
endDate <- "2019-02-25"
dayNum <-30

#candlestick plot
pricePlot <- uniqloDF %>% plot_ly(x = ~Date, type="candlestick",
               open = ~Open, close = ~Close,
               high = ~High, low = ~Low, 
               increasing = list(line = list(color = 'green')), 
               decreasing=list(line = list(color = 'blue')) )%>% 
  layout(title = "Uniqlo Candlestick Chart", 
         xaxis = list(range = c(StartDate, endDate)))  %>%
  add_lines(y = mAverage(uniqloDF$Close, dayNum), x = uniqloDF$Date, name = 'moving average', color = I('red'))

volPlot <- uniqloDF %>% plot_ly(x = ~Date, y = ~Volume, name = 'Volume', type = 'bar') %>%
  layout(yaxis = list(title = "Volume"))

subplot(pricePlot, volPlot, heights = c(0.5,0.3), nrows=2, shareX = TRUE, titleY = TRUE) %>%
  layout(legend = list(orientation = 'h', x = 0.7, y = 1, bgcolor = 'transparent',
                       xanchor = 'center', font = list(size = 12)))


### Stock candlestick chat plotting function ####
stockPlot <- function(df, startDate, endDate, dayNum, k=2) {
  #moving average function
  if(k==1) {
    mAverage <-  function(x, n){stats::filter(x, rep(1 / n, n), sides = 1)}
  } else {
    mAverage <-  function(x, n){stats::filter(x, rep(1 / n, n), sides = 2)}
  }
  
  #candlestick plot
  pricePlot <- df %>% plot_ly(x = ~Date, type="candlestick",
                                      open = ~Open, close = ~Close,
                                      high = ~High, low = ~Low, 
                                      increasing = list(line = list(color = 'green')), 
                                      decreasing=list(line = list(color = 'blue')) )%>% 
    layout(title = "Candlestick Chart", 
           xaxis = list(range = c(startDate, endDate)))  %>%
    add_lines(y = mAverage(df$Close, dayNum), x = df$Date, name = 'moving average', color = I('red'))
  
  #volume plot
  volPlot <- df %>% plot_ly(x = ~Date, y = ~Volume, name = 'Volume', type = 'bar') %>%
    layout(yaxis = list(title = "Volume"))
  
  #combined plot
  subplot(pricePlot, volPlot, heights = c(0.5,0.3), nrows=2, shareX = TRUE, titleY = TRUE) %>%
    layout(legend = list(orientation = 'h', x = 0.7, y = 1, bgcolor = 'transparent',
                         xanchor = 'center', font = list(size = 12)))
}

#test function
stockPlot(luluDF, "2016-02-26", "2019-02-25", 30)





