#library(Rblpapi)
library(dplyr)
#blpConnect("192.168.7.14")

tickers <- read.csv("swaper/ticker_list.csv", stringsAsFactors=FALSE)
rates <- read.csv("swaper/rates.csv",stringsAsFactors=FALSE,  sep=";")
markups <- read.csv("swaper/markupy.csv", stringsAsFactors=FALSE)

## pryygotwanie tickers

bid <- lapply(length(tickers[,1]), runif)
tickers <-cbind(tickers, bid)
colnames(tickers)[4] <- "bid"
tickers$ask <-tickers$bid*1.0005

## przygotowanie rates

bid_rate <- lapply(length(rates[,1]), runif)
rates <-cbind(rates, bid_rate)
colnames(rates)[3] <- "bid_rate"
rates$bid_rate <-rates$bid_rate/10
rates$ask_rate <-rates$bid_rate*1.0003

#przygotowanie całej tabeli

rates <- left_join(rates, markups, by="currency")
rates <- select(rates, currency, bid_rate, ask_rate, markup)

tickers <- left_join(tickers, rates, by = c("base"= "currency"))
tickers <- left_join(tickers, rates, by = c("quote"= "currency"))
colnames(tickers) <- c("ticker", "base", "quote", "bid", 
                       "ask", "BBR", "ABR", "base_markup",
                       "BQR", "AQR", "quote_markup")

tickers[is.na(tickers)] <- 0

## funkcja licząca

swap_calc <- function(side,bid,ask,BBR,ABR,BQR,AQR,BM,QM,multiplier){
  if(side == "short"){
    short <- bid*((1+(BQR - multiplier*BM/2)/360)/(1+(ABR + multiplier*QM/2)/360)-1)
  }
  else{
    long <- ask*((1+(AQR + multiplier*QM/2)/360)/(1+(BBR - multiplier*BM/2)/360)-1)
  }
}

## test

tickers$short <- swap_calc("short", tickers$bid, tickers$ask, tickers$BBR, 
                           tickers$ABR, tickers$BQR, tickers$AQR, 
                           tickers$base_markup, tickers$quote_markup,1.3)

tickers$long <- swap_calc("long", tickers$bid, tickers$ask, tickers$BBR, 
                           tickers$ABR, tickers$BQR, tickers$AQR, 
                           tickers$base_markup, tickers$quote_markup,1.3)

##

swaps_table <- select(tickers, ticker, long, short)
