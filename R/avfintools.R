#' @importFrom methods show
#' @importFrom stats cor filter lm predict quantile sd
#' @importFrom utils tail timestamp
#' @importFrom alphavantager av_api_key av_get
#' @importFrom dplyr between intersect mutate
#' @importFrom ggplot2 aes element_text geom_hline geom_line ggplot ggtitle theme ylim
#' @importFrom lubridate hour month hours force_tz force_tzs
#' @importFrom plotly add_histogram add_trace ggplotly plot_ly layout
#' @importFrom dplyr "%>%"
#' @importFrom tibble tibble



#av_api_key("YOUR_AV_KEY_HERE")

load("data/SPYdaily.RData")
load("data/SPY15.RData")
load("data/GMEdaily.RData")


#' This is data to be included in my package
#'
#' @source Alpha Vantager API
#' @references \url{https://www.alphavantage.co/documentation/}
"GMEdaily"

#' This is data to be included in my package
#'
#' @source Alpha Vantager API
#' @references \url{https://www.alphavantage.co/documentation/}
"SPYdaily"


#' This is data to be included in my package
#'
#' @source Alpha Vantager API
#' @references \url{https://www.alphavantage.co/documentation/}
"SPY15"



#Get Data from Stocks


#' After-hour and Pre-market Returns
#'
#' Show returns between the close of the last trading day and the open of the current trading day
#' @param df Dataframe with daily data
#' @return A vector in dataframe format of combined after-hour, overnight, and pre-market returns in percentage
#' @examples
#' idret(SPYdaily)
#' @export
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

#' Total return to multiplicative return
#'
#' Multiplicative returns are always comparative to the earliest return
#' @param df Dataframe with daily data
#' @return A vector in dataframe format cumulative multiplicative returns
#' @examples
#' trtomr(SPYdaily)
#' @export
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

#' Total return to cumulative return
#'
#' The cumulative percentage is the addition of subsequent total daily returns.
#' @param list_of_returns Vector in dataframe showing returns
#' @return A vector in dataframe format cumulative percentage returns
#' @examples
#' ret_to_cr(SPYdaily$returns)
#' @export
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


#' Get Stock Data at the Daily Level
#'
#' Daily, as in the summary statistics for the daily movement
#' @param ticker The ticker symbol as a string
#' @return A data frame with daily data such as the high, low, open, close, and associated returns. Available in the global environment.
#' @examples
#' \dontrun{
#' getdaily("SPY")
#' }
#' @export
getdaily <- function (ticker) {
  retdata <- av_get(symbol = ticker, av_fun = 'TIME_SERIES_DAILY', outputsize = 'full')
  return(addreturns(retdata))
}



#' Get Stock Data at the 15 minute level localized to current time zone
#'
#' Summary statistics for the movements on the 15 minute level
#' @param ticker The ticker symbol as a string
#' @param truncated Option to limit output to hours closer to market open hours. Default is true.
#' @return A data frame with daily data such as the high, low, open, close, and associated returns. Available in the global environment.
#' Default is truncated to show data more relevant to active trading hours. Adjusted to local time zone.
#' @examples
#' \dontrun{
#' get15("WTI")
#' get15("SPY", truncated = FALSE)
#' }
#' @export
get15 <- function (ticker, truncated = TRUE) {
  retdata <- av_get(symbol = ticker, av_fun = 'TIME_SERIES_INTRADAY', outputsize = 'full', interval = '15min')
  retdata <- addreturns(retdata)
  retdata <- retdata %>% dplyr::mutate(timestamp = force_tzs(timestamp, tzones = c("America/New_York"), tzone_out = Sys.timezone()))
  if (truncated == TRUE) {
    retdata <- retdata[between(hour(retdata$timestamp), 6, 13),]
    return(addreturns(retdata))
  } else {
    return(addreturns(retdata))
  }

}

#' Get Stock Data at the Weekly level
#'
#' Summary statistics for the stock movements on the weekly
#' @param ticker The ticker symbol as a string
#' @return A data frame with daily data such as the high, low, open, close, and associated returns. Available in the global environment.
#' @examples
#' \dontrun{
#' getweekly("WTI")
#' }
#' @export
getweekly <- function(ticker){
  retdata <- av_get(symbol = ticker, av_fun = 'TIME_SERIES_Weekly')
  retdata <- addreturns(retdata)
  return(addreturns(retdata))
}

#' Get Stock Data at the hourly level localized to current time zone
#'
#' Summary statistics for the movements on the hourly level
#' @param ticker The ticker symbol as a string
#' @param truncated Option to limit output to hours closer to market open hours. Default is true.
#' @return A data frame with daily data such as the high, low, open, close, and associated returns. Available in the global environment.
#' Default is truncated to show data more relevant to active trading hours. Adjusted to local time zone.
#' @examples
#' \dontrun{
#' get60("WTI")
#' get60("SPY", truncated = FALSE)
#' }
#' @export
get60 <- function (ticker, truncated = TRUE) {
  retdata <- av_get(symbol = ticker, av_fun = 'TIME_SERIES_INTRADAY', outputsize = 'full', interval = '60min')
  retdata <- addreturns(retdata)
  retdata <- retdata %>% dplyr::mutate(timestamp = force_tzs(timestamp, tzones = c("America/New_York"), tzone_out = Sys.timezone()))
  if (truncated == TRUE) {
    retdata <- retdata[between(hour(retdata$timestamp), 6, 13),]
    return(addreturns(retdata))
  } else {
    return(addreturns(retdata))
  }
}

#' Get Stock Data at the 5 minute level localized to current time zone
#'
#' Summary statistics for the movements on the 5 minute level
#' @param ticker The ticker symbol as a string
#' @param truncated Option to limit output to hours closer to market open hours. Default is true.
#' @return A data frame with daily data such as the high, low, open, close, and associated returns. Available in the global environment.
#' Default is truncated to show data more relevant to active trading hours. Adjusted to local time zone.
#' @examples
#' \dontrun{
#' get5("WTI")
#' get5("SPY", truncated = FALSE)
#' }
#' @export
get5 <- function (ticker, truncated = TRUE) {
  retdata <- av_get(symbol = ticker, av_fun = 'TIME_SERIES_INTRADAY', outputsize = 'full', interval = '5min')
  retdata <- addreturns(retdata)
  retdata <- retdata %>% dplyr::mutate(timestamp = force_tzs(timestamp, tzones = c("America/New_York"), tzone_out = Sys.timezone()))
  if (truncated == TRUE) {
    retdata <- retdata[between(hour(retdata$timestamp), 6, 13),]
    return(addreturns(retdata))
  } else {
    return(addreturns(retdata))
  }
}




#Get Crypto

#' Get Cryptocurrency Data at the hourly level localized to current time zone
#'
#' Summary statistics for the movements on the hourly level
#' @param coin_name The ticker symbol for the concurrency as a string
#' @return A data frame with daily data such as the high, low, open, close, and associated returns. Available in the global environment.
#' Adjusted to local time zone.
#' @examples
#' \dontrun{
#' crypto60("BTC")
#' }
#' @export
crypto60 <- function(coin_name) {
  retdata <- av_get(symbol = coin_name, av_fun = "CRYPTO_INTRADAY", interval = "60min", outputsize = "full", market = "USD")
  retdata <- addreturns(retdata)
  retdata <- retdata %>% dplyr::mutate(timestamp = force_tzs(timestamp, tzones = c("America/New_York"), tzone_out = Sys.timezone()))
  return(addreturns(retdata))
}

#' Get Cryptocurrency Data at the Daily Level
#'
#' Daily, as in the summary statistics for the daily movement
#' @param coin_name The ticker symbol for the concurrency as a string
#' @return A data frame with daily data such as the high, low, open, close, and associated returns. Available in the global environment.
#' @examples
#' \dontrun{
#' cryptodaily("WTI")
#' }
#' @export
cryptodaily <- function (coin_name) {
  retdata <- av_get(symbol = coin_name, av_fun = 'DIGITAL_CURRENCY_DAILY', market = 'USD')
  return(addreturns(retdata))
}


#Daily data Analytics

#' Compare returns visually between two securities
#'
#' Prints a plotly graph comparing returns, cumulative nominal returns, and cumulative multiplicative returns
#' @param a The first dataframe filled with data from the function getdaily()
#' @param b The second dataframe filled with data from the function getdaily()
#' @param a_name Character string name for first dataframe
#' @param b_name Character string name for second dataframe
#' @return Returns plotly graph comparing returns, cumulative nominal returns, and cumulative multiplicative returns. Click graph name to un/toggle for better visibility.
#' @examples
#' compare_returns(GMEdaily, SPYdaily, "GME", "SPY")
#' @export
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
  return(fig2)
}


#' Frequency plot of Subsequent Returns After a Percentage Input
#'
#' Returns a frequency plot drawn from historical data based on a percentage
#' change that occurred
#' @param dataset Dataframe with daily data
#' @param price_input the price movement, in percent, of the most recent
#' (or whatever you are interested in) trading day
#' @param hideprints if set to FALSE, returns summary statistics
#' @return percentage frequency plot of the following day based on historical data
#' @examples
#' thedayafter(SPYdaily, -1.35)
#' @export
thedayafter <- function(dataset, price_input, hideprints = TRUE){
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
  colnames(ndf) <- c('returns','idreturns')
  if(hideprints == FALSE)
  {
  print(summary(ndf))
  print(paste("Mean Return:", as.character(mean(ndf$returns))))
  print((paste("Mean AH/PM Return:", as.character(mean(ndf$idreturns)))))
  print((paste("Mean Total Next Day Return:", as.character((mean(ndf$idreturns)+mean(ndf$returns))))))
  print(paste(as.character(round((sum(ndf$returns>0)/length(ndf$returns)) * 100, 2)), " % of the time, next trading day returns are positive"))
  print(paste(as.character(round((sum(ndf$idreturns>0)/length(ndf$idreturns)) * 100, 2)), " % of the time, after-hours of this day/ pre-market of next day are  positive"))
  }
  tda_fig = plot_ly(ndf, type = 'histogram', x=~returns, histnorm = 'probability', name = 'Trading Day') %>% add_histogram(x = ~idreturns, name = "AH + PM") %>% layout(title = paste('One Day Returns after', as.character(price_input), '% change'))
  return(tda_fig)
  }



#' Frequency plot of Range, as well as maximum Upward and Downward Movement
#'
#' a plot_ly plot
#' @param df Dataframe with daily data
#' @param tick_name The ticker so the graph is correct
#' @param cumulative Default is FALSE, turn to TRUE for a cumulative plot.
#' @param hideprints if set to FALSE, shows summary statistics
#' @return Frequency plot where you can find intraday volatility (range), maximum upside (Upward Movement), maximum downside (Downward Movement) on a cumulative percentile basis
#' @examples
#' volatility_freq (SPYdaily, "SPY")
#' volatility_freq (SPYdaily, "SPY", cumulative = TRUE)
#' volatility_freq (SPYdaily, "SPY", hideprints = TRUE)
#' @export
volatility_freq <- function(df, tick_name, cumulative = FALSE, hideprints = FALSE) {
  high <- NULL
  low <- NULL
  hilow <- NULL
  df <- df %>% dplyr::mutate(hilow = high - low)
  df <- df %>% dplyr::mutate(hilowp = hilow/open * 100)
  df <- df %>% dplyr::mutate(openhi = (high - open)/open * 100)
  df <- df %>% dplyr::mutate(openlow = (low - open)/open * 100)

  if(hideprints == FALSE) {
  print(summary(df$hilowp))
  print(summary(df$openhi))
  print(summary(df$openlow))
  print(paste(tick_name, 'goes up',as.character((100 *sum(df$returns > 0))/dim(df)[1]), '% of the time from market open to close'))
  }

  if(cumulative == TRUE) {
    df <- df %>% dplyr::mutate(openlow = (low - open)/open * 100 * -1)
    return(plot_ly(df, x=~hilowp, type= 'histogram', cumulative = list(enabled = TRUE), histnorm = 'probability', name = 'Intraday Volatility') %>% add_histogram(x = ~openhi, name = 'Upward Movement') %>% add_histogram(x = ~openlow, name = 'Downward Movement') %>% layout(title = paste(tick_name, 'Intraday Movement Percentiles'), xaxis = list(title = 'Return in %s')))

  }

  plot_ly(df, x=~hilowp, type= 'histogram', histnorm = 'probability', name = 'Intraday Volatility') %>%
    add_histogram(x = ~openhi, name = 'Max Upside') %>%
    add_histogram(x = ~openlow, name = 'Max Downside') %>%
    layout(title = paste('Return Profiles for', tick_name), xaxis = list(title = 'Return in %s'))
}

#' Adds various returns as well as additional statistics.
#'
#' Adds percentage returns, interday returns, total returns, cumulative returns, multiplicative returns, range, and dollar returns to the default dataframe pulled from alphavantager
#' @param df Dataframe pulled from alphavantager
#' @return A more detailed dataframe with additional return metrics and summary statistics.
#' @examples
#' addreturns(SPYdaily)
#' @export
addreturns <- function (df) {
  returns <- NULL
  idreturns <- NULL
  tot_ret <- NULL
  high <- NULL
  low <- NULL
  df <- df %>% mutate(returns = 100* (close - open)/(open))
  df <- df %>% dplyr::mutate(idret(df))
  df <- df %>% dplyr::mutate(tot_ret = returns + idreturns)
  df <- df %>% dplyr::mutate(ret_to_cr(tot_ret))
  df <- df %>% dplyr::mutate(trtomr(df))
  df <- df %>% dplyr::mutate(range = high - low)
  df <- df %>% dplyr::mutate(d_ret = close - open)
  return(df)
}

#' Projects future prices based on regression
#'
#' Works with multiple time intervals. Do not oversupply with data, regression is expensive.
#' @param df Dataframe with price data. The opening price is used for projection purposes, works for all security types
#' @param tickername The ticker or the security you are putting in.
#' @return What comes out of this function
#' @examples
#'
#' project_price(tail(SPYdaily,200), "SPY")
#' project_price(SPY15, "SPY")
#' @export
project_price <- function(df, tickername) {
  hs <- NULL
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

#' Calculate the correlation between a column shared within two dataframes.
#'
#' Two dataframes from one of the "get" functions recommended, but works with any dataframe that shares columns as well as column lengths.
#' @param a First dataframe
#' @param b Second dataframe
#' @param sdata A string of the column name to compare

#' @return The correlation as a single numeric.
#' @examples
#' findcor(SPYdaily, GMEdaily, "returns")
#' @export
findcor<- function (a, b, sdata) {
  ndf = merge(a, b, by = 'timestamp')
  cors <- cor(eval(parse(text = paste0('ndf$', sdata, '.x'))),eval(parse(text = paste0('ndf$', sdata, '.y' ))))
  return(cors)
}

#' Frequency plot based on Absolute percentage movements
#'
#' Shows the cumulative probabilities of each percentage movement
#' @param df Dataframe from the "get"
#' @param name_in_string Name of security associated with dtaframe
#' @return A plot_ly graph showing the frequency of absolute returns.
#' "All Data" - no data omitted
#' "Cumulative Probability" Cumulative frequency graph of returns
#' "No Extremes" filter out skewed data
#' @examples
#' showidreturns(SPYdaily, "SPY")
#' @export
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

#' Negative Volume Index
#'
#' Calculates the  negative volume index, uses closing price of the time period
#' @param df Dataframe with price data.
#' @return Returns a 1 x # of columns in df dataframe
#' @examples
#' nvi(tail(SPYdaily, 200))
#' @export
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

#' Positive Volume Index
#'
#' Calculates the  positive volume index, uses closing price of the time period
#' @param df Dataframe with price data
#' @return Returns a 1 x # of columns in df dataframe
#' @examples
#' pvi(tail(SPYdaily, 200))
#' @export
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

#' Relative percentage from the Maximum Value
#'
#' Returns the percent of the maximum volume movement from data
#' @param df Dataframe with price data.
#' @return Returns a 1 x # of columns in df dataframe in percentage
#' @examples
#' volp(tail(SPYdaily, 200))
#' @export
volp <- function(df) {
  blank <- data.frame(rep(0,dim(df)[1]))
  colnames(blank) <- 'volp'
  blank = df$volume * 100 / max(df$volume)
  return(blank)
}

#' Graph of Volume Indicators
#'
#' Follows Open, PVI,  NVI, and %Max Volatility
#' @param df Dataframe with price data.
#' @param name A character string to add to the title  "____ Volume Analysis"
#' @return Returns plot_ly graph with PVI, NVI, Open and % Max Volatility
#' @examples
#' volume_analysis(SPY15, "SPY")
#' @export
volume_analysis <- function(df, name) {
  df <- df %>%  dplyr::mutate(nvi(df)) %>% dplyr::mutate(pvi((df))) %>% dplyr::mutate(volp(df))
  plot_ly(df, type='scatter', mode = 'lines') %>%
    add_trace(x =~timestamp, y =~open, name = 'Open') %>%
    add_trace(x =~timestamp, y =~pvi, name = 'PVI') %>%
    add_trace(x =~timestamp, y =~nvi, name = 'NVI') %>%
    add_trace(x =~timestamp, y =~volp, name = '% Max Vol.') %>%
    layout(title = paste(name, 'Volatility Analysis'))
}

#' Streak
#'
#' Counts the number of days the open price has moved consecutively
#' Negative and positive streaks are represented by their sign
#' @param df Dataframe with price data.
#' @return Returns a 1 x # of columns in df dataframe
#' @examples
#' streak(tail(SPYdaily, 200))
#' @export
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

#' Streak (Multiple Variables)
#'
#' Counts the number of days the open price has moved consecutively
#' Negative and positive streaks are represented by their sign; only works with various types of returns or mutated vector created by diff()
#' @param df Dataframe with price data.
#' @param var String of column name you wish to see streak in
#' @return Returns a 1 x # of columns in df dataframe
#' @examples
#' streak_var(tail(SPYdaily,200), "tot_ret")
#' @export
streak_var = function (df, var) { #streak2 lets you choose which variable to count the streak of
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

#' Show Fibonacci bands
#'
#' Displays Fibonacci bands
#' @param df Dataframe with price data, works with various intervals
#' @param showgraph Whether or not you want the function to pop out the visual
#' @param title A character string for the Title of your graph
#' @param hideprints if set to FALSE, prints out the support and resistance levels
#' @return Returns graph with various levels as well as a vector
#' @examples
#' fibs(tail(SPYdaily, 200))
#' SPYdailyfibs <- fibs(tail(SPYdaily, 200))
#' fibs(SPY15)
#' @export
fibs = function(df, showgraph = TRUE, title = NULL, hideprints = FALSE) {
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

  if (hideprints == FALSE) {
  print('These are the ranges where a reversion may happen')

  counter = 1
  while(counter < len) {
    print(paste(as.character(linesl[counter]), 'to', as.character(linesh[counter])))
    counter = counter + 1
  }
  }


  if(showgraph == TRUE) {
    fig = ggplot(df, aes(timestamp, open)) + geom_line() + ylim(min(df$low),max(df$high))
    if(is.null(title) == FALSE) {
      fig = fig + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
    }

    fig = ggplotly(fig + geom_hline(yintercept = lines))

    if (hideprints == FALSE) {
    print('These are the potential prices for resistance and support')
    }
    show(fig)

    return(lines)

  }

  if(showgraph == FALSE) {
    return(lines)
  }
} # Works on 15 minute level data


#' Candlestick chart
#'
#' Returns plot_ly candlestick chart
#' @param df Dataframe with price data.
#' @return A candlestick  chart
#' @examples
#' candles(SPYdaily)
#' @export
candles = function(df) { #df name to be typed in string
  fig = plot_ly(df, x = ~timestamp, type="candlestick",
                open = ~open, close = ~close,
                high = ~high, low = ~low, name = deparse(substitute(df))) %>% plotly::layout(title = "Candlestick Chart",
                                                                                             xaxis = list(rangeslider = list(visible = F)))
  return(fig)
}

#' Relative Strength Index
#'
#' Returns the Relative Strength Index with adjustable periods
#' @param df Dataframe with price data
#' @param periods Calculation Period
#' @param current If one wants to input the latest price point before data updates, RSI uses the percentage return at the end of the market hours
#' @param pricechange Input in percentage
#' @return Returns a vector of RSI calculations in dataframe format. If current = TRUE, returns the most recent RSI.
#' @examples
#' RSI(SPY15, 14)
#' RSI(tail(SPYdaily,200), 14, current = TRUE, pricechange = 1.3)
#' @export
RSI = function(df, periods, current = FALSE, pricechange = NULL, hideprints = TRUE) {
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
    if (hideprints == FALSE) {
    print(paste("The current RSI using changes on the day so far is", as.character(rscurrent)))
    }
  }

  invisible(rs2)

}

#' Generate moving averages
#'
#' Description
#' @param df Dataframe with price data
#' @param ma # of periods for the moving average to calculate
#' @return a vector with the same number of columns as df showing the moving averages. Periods before moving
#' average should be not considered for use. Output is kept same columns for compatibility.
#' @examples
#' SPYDMA200 <- genMA(SPYdaily, 14)
#' @export
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



#' Average True Range
#'
#' Returns the average true range as well as the relative price based on the ATR as a reference
#' @param df Dataframe with price data.
#' @param period Calculation period in day for the true range
#' @param current If one wants to input the latest price point before data update
#' @param mrprice Most recent price;
#' @return Returns a vector of ATR calculations in dataframe format. If current = TRUE, returns the most recent ATR as well as where price is in the context of the ATR
#' @examples
#' ATR(SPY15, 14)
#' ATR(SPY15, 14, current = TRUE, mrprice = tail(SPYdaily$close, 1) + 2)
#' @export
ATR = function(df, period, current = FALSE, mrprice = NULL, hideprints = TRUE) {
  atrs = list()
  highs = df$high
  lows = df$low
  closes = df$close
  for (i in 1:dim(df)[1]) {
    if (i < period) {
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
    if (i >= period) {
      curhighs = highs[(i - period) : i]
      curlows = lows[(i - period) : i]
      curcloses = closes[(i - period) : i]
      a1 = max(curhighs) - min(curlows)
      a2 = max(curhighs) - min(curcloses)
      a3 = max(curcloses) - min(curlows)
      atrs[i] = max(c(a1,a2,a3))
      mincur = min(curlows)
      maxcur = max(curhighs)
    }

  }

  if(current == TRUE){
    rng = maxcur - mincur
    percentile = ((mrprice - mincur)/rng) * 100
    if (hideprints == FALSE) {
    print(paste("In the past", as.character(period), "days the low is", as.character(mincur), "while the high is", as.character(maxcur)))
    print(paste0(as.character(mrprice), " is ", as.character(percentile), "th percentile of the above range" ))
    }
    return(percentile)

  }
  invisible(unlist(atrs))
}

#' Fast Zoom
#'
#' An Easy way to get to where you want to on a candlestick chart or other plots
#' @param plot The plot to be zoomed into
#' @param x A character string in the format("YYYYMMDD") for where to zoom in
#' @return The graph but zoomed in where you want to, mostly for daily data
#' @examples
#' fastzoom(candles(SPYdaily), "20220202")
#' @export
fastzoom = function (plot, x) {
  zplot = plot %>% plotly::layout(xaxis = list( range = c( (as.Date(x, '%Y%m%d')), (as.Date(x, '%Y%m%d') + 1))))
  return(zplot)
} # x will be a time element ( ex. 20200202) and it will conform to the closest points on the plot
