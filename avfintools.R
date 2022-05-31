library(alphavantager)
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(plotly)
library(ggplot2)


av_api_key("YOUR_AV_KEY_HERE")


#Get Data from Stocks
getdaily <- function (ticker) {
  retdata <- av_get(symbol = ticker, av_fun = 'TIME_SERIES_DAILY', outputsize = 'full')
  assign(paste0(ticker, 'daily'), addreturns(retdata), envir = .GlobalEnv)
}
getdailyc <- function(ticker) {
  retdata <- av_get(symbol = ticker, av_fun = 'TIME_SERIES_DAILY', outputsize = 'full')
  retdata <- retdata %>% dplyr::mutate(timestamp = timestamp + hours(3))
  assign(paste0(ticker, 'daily'), retdata, envir = .GlobalEnv)
}
getwkly <- function (ticker) {
  retdata <- av_get(symbol = ticker, av_fun = 'TIME_SERIES_WEEKLY', outputsize = 'full')
  assign(paste0(ticker, 'weekly'), addreturns(retdata), envir = .GlobalEnv)
}

# (specifically for during market hours, converted to PST time, option to not truncate the data)
get15 <- function (ticker, truncated = TRUE) {
  retdata <- av_get(symbol = ticker, av_fun = 'TIME_SERIES_INTRADAY', outputsize = 'full', interval = '15min')
  retdata <- addreturns(retdata)
  retdata <- retdata %>% dplyr::mutate(timestamp = timestamp - hours(3))
  if (truncated == TRUE) {
    retdata <- filter(retdata, between(hour(timestamp), 6, 13))
    assign(paste0(ticker, '15'), addreturns(retdata), envir = .GlobalEnv)
  } else {
    assign(paste0(ticker, '15'), addreturns(retdata), envir = .GlobalEnv)
  }
  
}
get60 <- function (ticker, truncated = TRUE) {
  retdata <- av_get(symbol = ticker, av_fun = 'TIME_SERIES_INTRADAY', outputsize = 'full', interval = '60min')
  retdata <- addreturns(retdata)
  retdata <- retdata %>% dplyr::mutate(timestamp = timestamp - hours(3))
  if (truncated == TRUE) {
    retdata <- filter(retdata, between(hour(timestamp), 7, 13))
    assign(paste0(ticker, '60'), addreturns(retdata), envir = .GlobalEnv)
  } else {
    assign(paste0(ticker, '60'), addreturns(retdata), envir = .GlobalEnv)
  }
}
get5 <- function (ticker, truncated = TRUE) {
  retdata <- av_get(symbol = ticker, av_fun = 'TIME_SERIES_INTRADAY', outputsize = 'full', interval = '5min')
  retdata <- addreturns(retdata)
  retdata <- retdata %>% dplyr::mutate(timestamp = timestamp - hours(3))
  if (truncated == TRUE) {
    retdata <- filter(retdata, between(hour(timestamp), 6, 13))
    assign(paste0(ticker, '5'), addreturns(retdata), envir = .GlobalEnv)
  } else {
    assign(paste0(ticker, '5'), addreturns(retdata), envir = .GlobalEnv)
  }
}


getweekly <- function(ticker){
  retdata <- av_get(symbol = ticker, av_fun = 'TIME_SERIES_Weekly')
  retdata <- addreturns(retdata)
  assign(paste0(ticker, 'wkl'), addreturns(retdata), envir = .GlobalEnv)
}



#Get Crypto
cr1h <- function(coin_name) {
  retdata <- av_get(symbol = coin_name, av_fun = "CRYPTO_INTRADAY", interval = "60min", outputsize = "full", market = "USD")
  retdata <- addreturns(retdata)
  retdata <- retdata %>% dplyr::mutate(timestamp = timestamp + hours(3))
  assign(paste0(coin_name, '1h'), addreturns(retdata), envir = .GlobalEnv)
}
cr1d <- function (coin_name) {
  retdata <- av_get(symbol = coin_name, av_fun = 'DIGITAL_CURRENCY_DAILY', market = 'USD')
  assign(paste0(coin_name, '1d'), retdata, envir = .GlobalEnv)
}


#Works only with daily data
compare_returns <- function (a, b, a_name, b_name){
  comb <- merge(a, b, by='timestamp')
  print(names(comb))
  f <- list(
    family = "Korea1",
    size = 18,
    color = "#7f7f7f"
  )
  x1 <- list(
    title = "Dates",
    titlefont = f
  )
  y1 <- list(
    title = "Returns in %s",
    titlefont = f
  )
  print(comb)
  fig2 <- plot_ly(comb, type = 'scatter', mode = 'lines')%>% 
    add_trace(y = ~mrreturns.x, x = ~timestamp, name = paste(a_name,'Cum. Mult. Returns'), fill = 'tozeroy', mode = 'lines', fillcolor ='#e6e2cd') %>% 
    add_trace(y = ~mrreturns.y, x = ~timestamp, name = paste(b_name,'Cum. Mult. Returns'), fill = 'tozeroy', mode = 'lines', fillcolor ='#a3ffb4') %>% 
    add_trace(y = ~cum_ret.x, x = ~timestamp, name = paste(a_name,'Cum. Nom. Returns'), fill = 'tozeroy', mode = 'lines', fillcolor ='#C4FBF8') %>% 
    add_trace(y = ~cum_ret.y, x = ~timestamp, name = paste(b_name,'Cum. Nom. Returns'), fill = 'tonexty', mode = 'lines', fillcolor = '#F7C5D0') %>%
    add_trace(y = ~tot_ret.x, x = ~timestamp, name = paste(a_name, 'Returns'), fill ='none') %>%
    add_trace(y = ~tot_ret.y, x = ~timestamp, name = paste(b_name, 'Returns'), fill ='none')
  
  fig2 <- fig2 %>% layout(title = paste(a_name, "vs.", b_name, "Returns"), xaxis = x1, yaxis = y1)
  print(paste(a_name, "vs.", b_name, "Returns"))
  return(fig2)
}
idret <- function(df){
  counter <- dim(df)[1]
  blank <- data.frame(rep(0,dim(df)[1]))
  i <- 0
  while (counter != 0) 
  {
    i = i + 1
    if(i > 1){
      l = i - 1
      blank[i,] = 100 * (df$open[i] - df$close[l])/(df$close[l])
    }  
    counter = counter - 1 
  }
  colnames(blank) <- 'idreturns'
  return(blank)
}
trtomr <- function(df) {
  counter <- dim(df)[1]
  blank <- data.frame(rep(0,dim(df)[1]))
  i <- 0
  st <- 1
  while (counter != 0) 
  {
    i = i + 1
    st = st * (1 + (df$tot_ret[i]/100)) 
    blank[i,] = (st - 1) * 100
    counter = counter - 1 
  }
  colnames(blank) <- 'mrreturns'
  return(blank)
  
}
ret_to_cr <- function(list_of_returns) {
  blank <- data.frame(rep(0,length(list_of_returns)))
  counter = length(list_of_returns)
  i = 0
  colnames(blank) <- 'cum_ret'
  while (counter != 0) {
    i = i + 1
    if(i== 1){
      blank[i,] <- as.numeric(list_of_returns[i])
    }
    if(i>1) {
      l = i - 1
      blank[i,] <- as.numeric(list_of_returns[i] + blank[l,])
    }
    counter = counter - 1
  }
  return(blank)
}

thedayafter <- function(dataset, price_input){
  stdev <- sd(dataset$returns)
  meme <- mean(dataset$returns)
  nstd <- (price_input - meme)/stdev
  lb <- stdev*(nstd-.5)
  ub <- stdev*(nstd+.5)
  mb <- month(Sys.Date()) - 1
  ma <- month(Sys.Date()) + 2
  dfil <- intersect(which(month(dataset$timestamp) >= mb), which(month(dataset$timestamp) <= ma))
  dind <- intersect(which(dataset$returns> lb),which(dataset$returns< ub)) + 1
  ndind <- intersect(dfil, dind)
  ndf<- tibble(dataset$returns[ndind], dataset$idreturns[ndind])
  print(summary(ndf))
  print(ndf)
  colnames(ndf) <- c('returns','idreturns')
  print(paste("Mean Return:", as.character(mean(ndf$returns))))
  print((paste("Mean AH/PM Return:", as.character(mean(ndf$idreturns)))))
  print((paste("Mean Total Next Day Return:", as.character((mean(ndf$idreturns)+mean(ndf$returns))))))
  print(paste(as.character(round((sum(ndf$returns>0)/length(ndf$returns)) * 100, 2)), " % of the time, next trading day returns are positive"))
  print(paste(as.character(round((sum(ndf$idreturns>0)/length(ndf$idreturns)) * 100, 2)), " % of the time, after-hours of this day/ pre-market of next day are  positive"))
  plot_ly(ndf, type = 'histogram', x=~returns, histnorm = 'probability', name = 'Trading Day') %>% add_histogram(x = ~idreturns, name = "AH + PM") %>% layout(title = paste('One Day Returns after', as.character(price_input), '% change'))
}
sadvolatility2 <- function(df, tick_name) {
  
  df <- df %>% dplyr::mutate(hilow = high - low)
  df <- df %>% dplyr::mutate(hilowp = hilow/open * 100)
  df <- df %>% dplyr::mutate(openhi = (high - open)/open * 100)
  df <- df %>% dplyr::mutate(openlow = (low - open)/open * 100 * -1)
  
  plot_ly(df, x=~hilowp, type= 'histogram', cumulative = list(enabled = TRUE), histnorm = 'probability', name = 'Intraday Volatility') %>% add_histogram(x = ~openhi, name = 'Upward Movement') %>% add_histogram(x = ~openlow, name = 'Downward Movement') %>% layout(title = paste(tick_name, 'Intraday Movement Percentiles'), xaxis = list(title = 'Return in %s'))
}
sadvolatility <- function(df, tick_name) {
  
  df <- df %>% dplyr::mutate(hilow = high - low)
  df <- df %>% dplyr::mutate(hilowp = hilow/open * 100)
  df <- df %>% dplyr::mutate(openhi = (high - open)/open * 100)
  df <- df %>% dplyr::mutate(openlow = (low - open)/open * 100)
  print(summary(df$hilowp))
  print(summary(df$openhi))
  print(summary(df$openlow))
  print(paste(tick_name, 'goes up',as.character((100 *sum(df$returns > 0))/dim(df)[1]), '% of the time from market open to close'))
  
  plot_ly(df, x=~hilowp, type= 'histogram', histnorm = 'probability', name = 'Intraday Volatility') %>% 
    add_histogram(x = ~openhi, name = 'Max Upside') %>% 
    add_histogram(x = ~openlow, name = 'Max Downside') %>% 
    layout(title = paste('Return Profiles for', tick_name), xaxis = list(title = 'Return in %s'))
}


addreturns <- function (df) {
  df <- df %>% mutate(returns = 100* (close - open)/(open))
  df <- df %>% dplyr::mutate(idret(df))
  df <- df %>% dplyr::mutate(tot_ret = returns + idreturns)
  df <- df %>% dplyr::mutate(ret_to_cr(tot_ret))
  df <- df %>% dplyr::mutate(trtomr(df))
  df <- df %>% dplyr::mutate(range = high - low)
  df <- df %>% dplyr::mutate(d_ret = close - open)
  return(df)
}
addreturnsd <- function(df) {
  df <- df %>% dplyr::mutate(returnsd = close - open)
  df <- df %>% dplyr::mutate(tr3(df)) #trailing 3 wherether it is weeks, days, whatever the data unit is.
  return(df)
}
project <- function(df, tickername) {
  df <- df %>% mutate(hs =  as.numeric(timestamp - df$timestamp[1], units = 'hours')) %>%
    mutate(hs2 = hs^2) %>%
    mutate(hs3 = hs^3)
  ln1 <- lm(formula = open ~ hs, df)
  ln2 <- lm(formula = open ~ hs + hs2, df)
  ln3 <- lm(formula = open ~ hs + hs2 + hs3, df)
  ndf <- tibble((min(df$hs):(max(df$hs + 200 ))))
  colnames(ndf) <- 'hs'
  ndf <- ndf %>%
    mutate(hs2 = hs^2) %>%
    mutate(hs3 = hs^3)
  
  ndf <- ndf %>% mutate(btime = min(df$timestamp) + hours(hs))
  pr1 <- as.numeric(predict(ln1, ndf))
  pr2 <- as.numeric(predict(ln2, ndf))
  pr3 <- as.numeric(predict(ln3, ndf))
  pr4 <- (pr1 + pr2 + pr3)/3
  plot_ly(df, type = 'scatter', mode = 'lines') %>% 
    add_trace(x = ~timestamp, y = ~open, name = 'Open') %>% 
    add_trace(x = ndf$btime, y = pr1, name = 'Linear') %>% 
    add_trace(x = ndf$btime, y = pr2, name = "Quadratic") %>%
    add_trace(x = ndf$btime, y = pr3, name = 'Cubic') %>%
    add_trace(x = ndf$btime, y = pr4, name = 'Average')%>%
    layout(title = paste(tickername, 'Projections'), yaxis = list(title = 'Price in Dollars'), xaxis = list(title = 'Dates'))
}
findcor<- function (a, b, sdata) {
  ndf = merge(a, b, by = 'timestamp')
  cors <- cor(eval(parse(text = paste0('ndf$', sdata, '.x'))),eval(parse(text = paste0('ndf$', sdata, '.y' ))))
  return(cors)
}
#sdata is logged in as a string, it is the column name which to compare correlation with
ma5days <- function(df) {
  counter <- dim(df)[1]
  blank <- data.frame(rep(0,dim(df)[1]))
  i <- 0
  while (counter != 0) 
  {
    i = i + 1
    dr <- df[1:i,]
    cd <- df$timestamp[i]
    da5 <- date(df$timestamp[i]) - 5
    fd <- intersect(filter(dr, timestamp >= da5), filter(dr, timestamp <= cd))
    n = dim(fd)[1]
    blank[i,] = sum(fd$high + fd$low)/(n*2)
    counter = counter - 1 
  }
  colnames(blank) <- 'ma5days'
  print(fd)
  return(blank)
}
sma <- function(df, name) {
  df <- df %>% dplyr::mutate(ma5days(df))
  
  plot_ly(df, type = 'scatter', mode = 'lines') %>%
    add_trace(x = ~timestamp, y =~open, name = 'Open') %>%
    add_trace(x = ~timestamp, y = ~ma5days, name = '5 Day Trailing Average') %>%
    layout (title = paste(name, 'with 5 Day Moving Average'))
}
eod_price_projection = function (df, open, high, low, direction) { 
  df = df %>% dplyr::mutate(ratio = d_ret/range)
  factor1 = tr3l(df$range)
  factor2 = tr3l(df$ratio)
  ratio = as.numeric(df$ratio)
  meanra = mean(ratio[!sapply(ratio, is.nan)])
  ndf = data.frame(cbind(factor1, factor2, ratio))
  ln1 = lm(ratio ~. , ndf)
  pv1 = as.numeric(predict(ln1, ndf))
  rfactor = meanra + (abs((tail(pv1,1)- meanra ) *30))
  print(rfactor)
  
  if(direction == -1) {
    closep = open - (high - low) * rfactor
  }
  if(direction == 1) {
    closep = open + (high - low) * rfactor
  }
  return(closep)
} #direction = 1 for upday, -1 for downday



showidreturns <- function(df, name_in_string) {
  
  ub <- as.numeric(quantile(df$idreturns, probs = .95) * 3)
  lb <- as.numeric(quantile(df$idreturns, probs = .05) * 3)
  dfadd <- tibble(ifelse(df$idreturns < ub & df$idreturns >lb , df$idreturns, 0))
  colnames(dfadd) = 'idreturnsne'
  df <- df %>% dplyr::mutate(dfadd)
  # updatemenus component
  updatemenus <- list(
    list(
      active = -1,
      type= 'buttons',
      buttons = list(
        list(
          label = "All Data",
          method = "update",
          args = list(list(visible = c(FALSE, TRUE, FALSE)),
                      list(title = paste(name_in_string, "Interday Returns")))),
        list(
          label = "Cumulative Probability",
          method = "update",
          args = list(list(visible = c(TRUE, FALSE, FALSE)),
                      list(title = paste(name_in_string, "Interday Returns")))),
        list(
          label = "No Extremes",
          method = "update",
          args = list(list(visible = c(FALSE, FALSE, TRUE)),
                      list(title = paste(name_in_string, "Interday Returns"))))
      ))
    
  )
  
  fig <- df %>% plot_ly(type = 'histogram', histnorm = 'probability') %>%
    add_histogram(x = df$idreturns, name="All Returns")%>% 
    add_histogram(x = df$idreturnsne , name="No Extremes")%>%
    add_histogram(x = df$idreturns, cumulative = list(enabled = TRUE) ,name = "Cumulative Probability")
  
  
  fig <- fig %>% layout(showlegend=FALSE,
                        xaxis=list(title="Returns in %"),
                        yaxis=list(title="Decimal Probability"),
                        updatemenus=updatemenus)
  fig
  
  
}

nvi <- function(df) {
  blank <- data.frame(rep(0,dim(df)[1]))
  counter = dim(df)[1]
  i = 0
  colnames(blank) <- 'nvi'
  while (counter != 0) {
    i = i + 1
    counter = counter - 1
    j = i - 1
    if (i == 1) {
      blank[i,] = 1
      next
    }
    if(df$volume[i] < df$volume[j]){
      l <- blank[j,]
      blank[i,] <- l + l * ((df$close[i] -df$close[j])/df$close[j])
    }
    else {
      blank[i,] = blank[j,]
    }
    
  }
  return(blank * df$open[1])
}
pvi <- function(df) {
  blank <- data.frame(rep(0,dim(df)[1]))
  counter = dim(df)[1]
  i = 0
  colnames(blank) <- 'pvi'
  while (counter != 0) {
    i = i + 1
    counter = counter - 1
    j = i - 1
    if (i == 1) {
      blank[i,] = 1
      next
    }
    if(df$volume[i] > df$volume[j]){
      l <- blank[j,]
      blank[i,] <- l + l * ((df$close[i] -df$close[j])/df$close[j])
    }
    else {
      blank[i,] = blank[j,]
    }
    
  }
  return(blank * df$open[1])
}
volp <- function(df) {
  blank <- data.frame(rep(0,dim(df)[1]))
  colnames(blank) <- 'volp'
  blank = df$volume * 100 / max(df$volume)
  return(blank)
}
volanal <- function(df, nameof) {
  df <- df %>%  dplyr::mutate(nvi(df)) %>% dplyr::mutate(pvi((df))) %>% dplyr::mutate(volp(df))
  plot_ly(df, type='scatter', mode = 'lines') %>%
    add_trace(x =~timestamp, y =~open, name = 'Open') %>%
    add_trace(x =~timestamp, y =~pvi, name = 'PVI') %>%
    add_trace(x =~timestamp, y =~nvi, name = 'NVI') %>%
    add_trace(x =~timestamp, y =~volp, name = '% Max Vol.') %>%
    layout(title = paste(nameof, 'Volatility Analysis'))
}
streak = function (df) { # streak will return a positive or negative number that shows the number of times it has moved in the same direction, with positive numbers representing an upward movement and downward movements symbolized by a negative number
  counter <- dim(df)[1]
  blank <- data.frame(rep(0,dim(df)[1]))
  i <- 1
  while (counter != 0 ) 
  {
    i = i + 1
    n = i + 1
    b = i - 1
    if ( n > dim(df)[1]) {
      break
    }
    if(df$open[i] < df$open [n]){
      blank[i,] = blank[b,] + 1
    }
    if(df$open[i] > df$open [n]){
      blank[i,] = blank[b,] - 1
    }
    #If absolutes value of current value is lower than the previous value, than we should reset to 0 
    if(abs(blank[i,]) < abs(blank[b,])) {
      blank[i,] = 0
    }
    else {
      next
    }
    
    counter = counter - 1 
  }
  colnames(blank) <- 'streak'
  return(blank)
  
}
streak2 = function (df, var) { #streak2 lets you choose which variable to count the streak of 
  counter <- dim(df)[1]
  blank <- data.frame(rep(0,dim(df)[1]))
  i <- 1
  counted = eval(parse(text = paste0('df$', var)))
  while (counter != 0 ) 
  {
    i = i + 1
    n = i + 1
    b = i - 1
    if ( n > dim(df)[1]) {
      break
    }
    if(counted[i] < counted [n]){
      blank[i,] = blank[b,] + 1
    }
    if(counted[i] > counted [n]){
      blank[i,] = blank[b,] - 1
    }
    #If absolutes value of current value is lower than the previous value, than we should reset to 0 
    if(abs(blank[i,]) < abs(blank[b,])) {
      blank[i,] = 0
    }
    else {
      next
    }
    
    counter = counter - 1 
  }
  colnames(blank) <- 'streak'
  return(blank)
  
}
tr3 <- function(df){
  counter <- dim(df)[1] - 3
  blank <- data.frame(rep(0,dim(df)[1]))
  i <- 3
  while (counter != 0) 
  {
    i = i + 1
    j = i - 1
    k = i - 2
    l = i - 3
    travage = ((df$high[j] + df$low[j])/2 + (df$high[k] + df$low[k])/2 + (df$high[l] + df$low[l])/2)/3
    blank[i,] = travage
    counter = counter - 1 
  }
  colnames(blank) <- 'tr3'
  return(blank)
}
tr3l<- function(list) {
  counter <- length(list) - 3
  blank <- rep(0,length(list))
  i <- 3
  while (counter != 0) 
  {
    i = i + 1
    j = i - 1
    k = i - 2
    l = i - 3
    travage = (list[j] + list[k] + list[l])/3
    blank[i] = travage
    counter = counter - 1 
  }
  return(blank)
}


fibs = function(df, showgraph = TRUE, title = NULL) {
  fib = c(.03, .05, .08, .13, .21, .34)
  l = length(fib)
  m = 1
  lines1 = rep(0, l)
  while (l > 0) {
    line = quantile(df$open,fib[m])
    lines1[m] = line
    m = m + 1
    l = l - 1
  }
  lines2 = rep(0, l)
  ifib = 1 - fib
  l = length(fib)
  m = 1
  while (l > 0) {
    line = quantile(df$open,ifib[m])
    lines2[m] = line
    m = m + 1
    l = l - 1
  }
  
  
  avgret = mean(df$tot_ret)
  lines3 = df$close[dim(df)][1] + avgret * fib * df$close[dim(df)][1] *100
  lines = c(lines1, lines2, lines3)
  lines = sort(lines)
  linesl = round((lines - 0.15)*10)/10
  linesh = round((lines + 0.15)*10)/10
  len = length(lines)
  
  print('These are the ranges where a reversion may happen')
  counter = 1
  while(counter < len) {
    print(paste(as.character(linesl[counter]), 'to', as.character(linesh[counter])))
    counter = counter + 1
  }
  
  
  if(showgraph == TRUE) {
    fig = ggplot(df, aes(timestamp, open)) + geom_line() + ylim(min(df$low),max(df$high))
    if(is.null(title) == FALSE) {
      fig = fig + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
    }
    
    fig = ggplotly(fig + geom_hline(yintercept = lines))
    
    print('These are the potential prices for resistance and support')
    show(fig)
    
    return(lines)
    
  }
  
  if(showgraph == FALSE) {
    return(lines)
  }
} # Works on 15 minute level data
hi8 = function(his, selname) { #Takes a list of highs, stores them, and creates an average that gets updated once each time, with the element in the end being replaced by the 'new' high. Selname means selected name, it will be a string
  li8 = rep(0, 8)
  counter = length(his)
  liret = tibble(rep(0, counter))
  l = 1
  while (counter > 0) {
    li8 = li8[2:8]
    li8 = append(li8, his[l])
    liret[l] = mean(li8)
    l = l+1
    counter = counter - 1
  }
  liret = t(liret[1,])
  colnames(liret) = selname
  rownames(liret) = NULL
  return(liret)
}
candles = function(df) { #df name to be typed in string
  fig = plot_ly(df, x = ~timestamp, type="candlestick",
                open = ~open, close = ~close,
                high = ~high, low = ~low, name = deparse(substitute(df))) %>% layout(title = "Candlestick Chart",
                                                                                     xaxis = list(rangeslider = list(visible = F)))
  return(fig)
}
ATR = function(df, daysu, deRSI = FALSE, delta = NULL) { #days used for trading average Day's end RSI is used to calculate the RSI 
  atrs = list()
  highs = df$high
  lows = df$low
  closes = df$close
  for (i in 1:dim(df)[1]) {
    if (i < daysu) {
      curhighs = highs[1:i]
      curlows = lows[1:i]
      curcloses = closes[1:i]
      a1 = max(curhighs) - min(curlows)
      a2 = max(curhighs) - min(curcloses)
      a3 = max(curcloses) - min(curlows)
      atrs[i] = max(c(a1,a2,a3))
    }
    if (i >= daysu) {
      curhighs = highs[(i - daysu) : i]
      curlows = lows[(i - daysu) : i]
      curcloses = closes[(i - daysu) : i]
      a1 = max(curhighs) - min(curlows)
      a2 = max(curhighs) - min(curcloses)
      a3 = max(curcloses) - min(curlows)
      atrs[i] = max(c(a1,a2,a3))
    }
    
  }
  return(unlist(atrs))
}
RSI = function(df, periods, current = FALSE, pricechange = NULL) {
  rets = df$tot_ret
  rs1 = c()
  poslist = c()
  neglist = c()
  for (i in 1:length(rets)) {
    if(rets[i] > 0 ) {
      poslist = append(poslist, rets[i])
      neglist = append(neglist, 0)
    }
    if(rets[i] < 0){
      neglist = append(neglist, rets[i])
      poslist = append(poslist, 0)    }
  }
  neglist = abs(neglist)
  for(i in 1:length(rets)) {
    if (periods < i) {
      avggain = mean(poslist[i - periods: i]) 
      avgloss = mean(neglist[i - periods: i])
      rs1[i] = 100 - 100/(1+(avggain/avgloss))
    }
    else {
      rs1[i] = 0 
    }
  }
  avggainl = c()
  avglossl = c()
  for(i in 1:length(rets)) {
    if (periods < i) {
      avggain = mean(poslist[i - periods: i])
      avggainl = append(avggainl, avggain)
      avgloss = abs(mean(neglist[i - periods: i]))
      avglossl = append(avglossl, avgloss)
      rs1[i] = 100 - 100/(1+(avggain/avgloss))
    }
    else {
      avggain = mean(poslist[1:i])
      avggainl = append(avggainl, avggain)
      avgloss = mean(neglist[1:i])
      avglossl = append(avglossl, avgloss)
      rs1[i] = 0
    }
  }
  rs2 = c()
  for(i in 1:length(rets)) {
    if (periods < i) {
      rs2[i] = 100 - (100/(1+((avggainl[i-1] * (periods - 1))+poslist[i])/(avglossl[i-1] * (periods - 1)+neglist[i])))
      
    }
    else {
      rs2[i] = 0 
    }
  }
  
  if (current == TRUE) {
    if (pricechange > 0 ){
      rscurrent = 100 - (100/(1+((tail(avggainl, 1) * (periods - 1))+pricechange)/(tail(avglossl, 1) * (periods - 1)+0)))
      
    }
    if (pricechange < 0 ){
      rscurrent = 100 - (100/(1+((tail(avggainl, 1) * (periods - 1))+0)/(tail(avglossl, 1) * (periods - 1)+abs(pricechange))))
      
    }
    else {
      rscurrent = 100 - (100/(1+((tail(avggainl, 1) * (periods - 1))+0)/(tail(avglossl, 1) * (periods - 1)+0))) 
    }
    print(paste("The current RSI using changes on the day so far is", as.character(rscurrent)))  
  }
  
  return(rs2)
  
}
genMA = function(df, ma) { #This is a generalized function that allows you to find moving average based on the timeframe of candlestick data. Returns a list that allows for 
  rp = (df$high + df$low)/2
  d1 = dim(df)[1]
  l = list()
  for(i in 1:d1) {
    gl = c()
    for(j in 1:ma) {
      gl = sort(append(gl,abs(j - i)))
    }
    l[[i]] = gl
    
  }
  nl = list()
  for (k in 1:d1) {
    
    nl = append(nl, mean(rp[l[[k]]]))
    
  }
  return(unlist(nl))
}
ATR = function(df, daysu, current = FALSE, mrprice = NULL) { #days used for trading average Day's end RSI is used to calculate the RSI 
  atrs = list()
  highs = df$high
  lows = df$low
  closes = df$close
  for (i in 1:dim(df)[1]) {
    if (i < daysu) {
      curhighs = highs[1:i]
      curlows = lows[1:i]
      curcloses = closes[1:i]
      a1 = max(curhighs) - min(curlows)
      a2 = max(curhighs) - min(curcloses)
      a3 = max(curcloses) - min(curlows)
      atrs[i] = max(c(a1,a2,a3))
      mincur = min(curlows)
      maxcur = max(curhighs)
    }
    if (i >= daysu) {
      curhighs = highs[(i - daysu) : i]
      curlows = lows[(i - daysu) : i]
      curcloses = closes[(i - daysu) : i]
      a1 = max(curhighs) - min(curlows)
      a2 = max(curhighs) - min(curcloses)
      a3 = max(curcloses) - min(curlows)
      atrs[i] = max(c(a1,a2,a3))
      mincur = min(curlows)
      maxcur = max(curhighs)
    }
    
  }
  
  if(current == TRUE){
    print(paste("In the past", as.character(daysu), "days the low is", as.character(mincur), "while the high is", as.character(maxcur)))
    rng = maxcur - mincur
    percentile = ((mrprice - mincur)/rng) * 100
    print(paste0(as.character(mrprice), " is ", as.character(percentile), "th percentile of the above range" ))
    return()
    
  }
  return(unlist(atrs))
}
fastzoom = function (plot, x) {
  zplot = plot %>% layout(xaxis = list( range = c( (as.Date(x, '%Y%m%d')), (as.Date(x, '%Y%m%d') + 1))))
  return(zplot)
} # x will be a time element ( ex. 20200202) and it will conform to the closest points on the plot
