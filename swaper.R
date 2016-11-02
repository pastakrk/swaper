library(Rblpapi)
library(plyr)
blpConnect("192.168.7.14")
tickers <- read.csv("swaps_generator/ticker_list.csv", stringsAsFactors=FALSE)
rates <- read.csv("swaps_generator/rates.csv",stringsAsFactors=FALSE,  sep=";")
markups <- read.csv("swaps_generator/markupy.csv", stringsAsFactors=FALSE)
tickers_temp <- tickers
rates_temp <- rates
tickers <- bdp(tickers$ticker, c("BID", "ASK", "PX_LAST"))
rates <- bdp(rates$ticker, c("BID", "ASK", "PX_LAST"))
row_names1 <- rownames(tickers)
row_names2 <- rownames(rates)
tickers <- cbind(tickers, row_names1)
rates <- cbind(rates, row_names2)
colnames(tickers)[4] <- "ticker"
colnames(rates)[4] <- "ticker"
tickers <- merge(tickers, tickers_temp, by.x="ticker")
rates <- merge(rates, rates_temp, by.x="ticker")
colnames(tickers)[4] <- "ticker"
colnames(rates)[4] <- "ticker"
tickers <- merge(tickers, rates[4:5], by.x="base", by.y="currency", all.x = TRUE)
colnames(tickers)[7] <- "base_IR"
tickers <- merge(tickers, rates[4:5], by.x="quote", by.y="currency", all.x = TRUE)
colnames(tickers)[8] <- "quote_IR"
tickers <- merge(tickers, markups, by.x="base", by.y="currency", all.x = TRUE)
tickers <- merge(tickers, markups, by.x="quote", by.y="currency", all.x = TRUE)
colnames(tickers) <- c("quote", "base", "ticker", "bid", 
                       "ask", "last", "base_ir", 
                       "quote_ir", "base_markup", "quote_markup")
tickers[, 4:10] <- sapply(tickers[, 4:10], as.numeric)
tickers[is.na(tickers)] <- 0

swap_calc <- function(side,pricebid,priceask,RQB,RQA,RBB,RBA,BM,QM){
  if(side == "short"){
    short <- pricebid*((1+(RQB - BM/2)/360)/(1+(RBA + QM/2)/360)-1)
    }
    else{
    long <- priceask*((1+(RQA - QM/2)/360)/(1+(RBB + BM/2)/360)-1)
  }
}

tickers$short <- swap_calc("short", tickers$bid, tickers$ask, tickers$quote_ir, 
                           tickers$quote_ir, tickers$base_ir, tickers$base_ir, 
                           tickers$base_markup, tickers$quote_markup)

tickers$long <- swap_calc("long", tickers$bid, tickers$ask, tickers$quote_ir, 
                          tickers$quote_ir, tickers$base_ir, tickers$base_ir, 
                          tickers$base_markup, tickers$quote_markup)
