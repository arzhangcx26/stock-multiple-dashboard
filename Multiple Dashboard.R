##-------------------------------
## Application Documentation
##-------------------------------
# This application is used to obtain live valuation
# multiples from any selected basket of stocks for the
# purposes of comparable analysis. Data is sourced from
# Yahoo Finance.
# 
# multipleGraph(): Plots TTM EV/Sales and TTM/FWD P/Es
# for chosen peer group
#
# ptGraph(): Plots TTM EV/Sales and TTM/FWD P/Es
# for chosen peer group w/ fair multiples
# along w/ price target and time series data
#
# nominalPremiumSales(): Displays nominal EV/Sales
# multiple premium for ticker of interest
#
# nominalPremiumTTM(): Displays nominal TTM P/E
# multiple premium for ticker of interest
#
# nominalPremiumFWD(): Displays nominal FWD P/E
# multiple premium for ticker of interest
#
# percentPremiumSales(): Displays percent EV/Sales
# multiple premium for ticker of interest
#
# percentPremiumTTM(): Displays percent TTM P/E
# multiple premium for ticker of interest
#
# percentPremiumFWD(): Displays percent FWD P/E
# multiple premium for ticker of interest
#
##-------------------------------
## Application Settings
##-------------------------------

# Define the peer group you want to examine
Symbols <- c("AMD","NVDA","MU","MRVL",
           "QCOM","AVGO","INTC","TXN",
           "ADI","NXPI","MCHP","SWKS",
           "QRVO","MPWR","ASML")

# Define the ticker you want to value out of the peer group
mainTicker <- 1

# Define the premiums we assign to ticker of interest for price targets
choosePrem1 <- 2.5 #EV/Sales Premium
choosePrem2 <- 10 #TTM P/E Premium
choosePrem3 <- 5 #FWD P/E Premium

##-------------------------------
## Load Required Libraries 
##-------------------------------
library("rjson")
library(data.table)
library(tidyr)
library(tidyverse)
library(XML)
library(xml2)
library(rvest)
library(httr)
library(quantmod)
library(plyr)
library(Quandl)
Quandl.api_key("WekW5rCDzEBzSegaydFu")
n <- length(Symbols)
factor <- 1000000

##-------------------------------
## Get Historical Time Series
##-------------------------------

# Set Time Frame for Live Price Data
t1 <- ISOdate(2021,5,30,hour=0)
t2 <- ISOdate(2022,5,30,hour=0)

# Get Price Data for All Tickers
stock <- Symbols[mainTicker]
url <- paste("https://query1.finance.yahoo.com/v7/finance/download/",
             stock,
             "?period1=",
             as.integer(t1),
             "&period2=",
             as.integer(t2),
             "&interval=1d&events=history",
             sep="")

fileName <- "my_dataset.csv"
download.file(url, fileName)
timeSeries <- read.csv(fileName)
timeSeries <- timeSeries[,5]

par(mfrow=c(1,1))
plot(ts(timeSeries),main="Historical Prices",
     ylab="Share Price",col="black",type="l")

##-------------------------------
## Get Shares O/S and Last Price
##-------------------------------

what_metrics <- yahooQF(c("Shares Outstanding", 
                          "Last Trade (Price Only)",
                          "EPS Forward"))

metrics <- getQuote(paste(Symbols, sep="", collapse=";"), what=what_metrics)
metrics[2] <- metrics[2]/factor

##-------------------------------
## Get Quarterly Revenue Data
##-------------------------------

qRev <- rep(NA, 4)
metrics$Revenue <- rep(NA, n)
for(i in 1:n) {
  symbol <- Symbols[i]
  url <- paste0('https://finance.yahoo.com/quote/', symbol, '/financials?p=', symbol)
  html_M <- read_html(url) %>% html_node('body') %>% html_text() %>% toString()
  
  fin_cur <- sub(".*\"financialCurrency\":*(.*?) *[\n|\r\n|\r]{2}", "\\1", html_M)
  fin_cur <- head(stringr::str_match_all(fin_cur, "(.*?)\\}")[[1]][, 2],1)
  fin_cur=gsub("\"", "", fin_cur, fixed=T)
  
  Q_results <- sub(".*\"quarterly\":*(.*?) *[\n|\r\n|\r]{2}", "\\1", html_M)
  Q_results <- head(stringr::str_match_all(Q_results, "\\[(.*?)\\]")[[1]][, 2],1)
  splitQ <- str_split(Q_results, "\\{\"date\":")
  splitQ <- splitQ[[1]]
  splitQ<- paste("\\{\"date\":", splitQ, sep="")
  if(length(splitQ)>0){
    tot_rev_df <- data.frame(curr = fin_cur,
                             key=str_extract(splitQ, "\"date\":\"*(.*?) *\""),
                             value=str_extract(splitQ, "\"revenue\":\\{\"raw\":*(.*?) *,"))
    tot_rev_df <- tot_rev_df[complete.cases(tot_rev_df), ]
    tot_rev_df <- data.frame(lapply(tot_rev_df, as.character), stringsAsFactors=FALSE)
    tot_rev_df <- tot_rev_df %>%
      separate(key, c("first", "key"), sep=":") %>% 
      select(-first)
    tot_rev_df <- tot_rev_df %>%
      separate(value, c("first", "second", "value"), sep=":") %>%
      select(-first, -second)
    tot_rev_df <- tot_rev_df %>%
      mutate(key=gsub("\"", "", key, fixed=T),
             value=gsub(",", "", value, fixed=T))
  } 
  qRev <- as.numeric(tot_rev_df[,3])/factor
  metrics$Revenue[i] <- sum(qRev)
}

##-------------------------------
## Get Quarterly Earnings Data
##-------------------------------

qEar <- rep(NA, 4)
metrics$Earnings <- rep(NA, n)
for(i in 1:n) {
  symbol <- Symbols[i]
  url <- paste0('https://finance.yahoo.com/quote/', symbol, '/financials?p=', symbol)
  html_M <- read_html(url) %>% html_node('body') %>% html_text() %>% toString()
  
  fin_cur <- sub(".*\"financialCurrency\":*(.*?) *[\n|\r\n|\r]{2}", "\\1", html_M)
  fin_cur <- head(stringr::str_match_all(fin_cur, "(.*?)\\}")[[1]][, 2],1)
  fin_cur=gsub("\"", "", fin_cur, fixed=T)
  
  Q_results <- sub(".*\"quarterly\":*(.*?) *[\n|\r\n|\r]{2}", "\\1", html_M)
  Q_results <- head(stringr::str_match_all(Q_results, "\\[(.*?)\\]")[[1]][, 2],1)
  splitQ <- str_split(Q_results, "\\{\"date\":")
  splitQ <- splitQ[[1]]
  splitQ<- paste("\\{\"date\":", splitQ, sep="")
  if(length(splitQ)>0){
    tot_ear_df <- data.frame(curr = fin_cur,
                             key=str_extract(splitQ, "\"date\":\"*(.*?) *\""),
                             value=str_extract(splitQ, "\"earnings\":\\{\"raw\":*(.*?) *,"))
    tot_ear_df <- tot_ear_df[complete.cases(tot_ear_df), ]
    tot_ear_df <- data.frame(lapply(tot_ear_df, as.character), stringsAsFactors=FALSE)
    tot_ear_df <- tot_ear_df %>%
      separate(key, c("first", "key"), sep=":") %>% 
      select(-first)
    tot_ear_df <- tot_ear_df %>%
      separate(value, c("first", "second", "value"), sep=":") %>%
      select(-first, -second)
    tot_ear_df <- tot_ear_df %>%
      mutate(key=gsub("\"", "", key, fixed=T),
             value=gsub(",", "", value, fixed=T))
  } 
  qEar <- as.numeric(tot_ear_df[,3])/factor
  metrics$Earnings[i] <- sum(qEar)
}


##-------------------------------
## Financial Data Wrangling
##-------------------------------

# assign metrics to variables
shares <- metrics[2]
prices <- metrics[3]
fwdEps <- metrics[4]
revenue <- metrics[5]
earnings <- metrics[6]
ttmEps <- earnings / shares
marketCap <- shares*prices

# count all observations w/ negative EPS
countFwd <- rep(0, n)
countTtm <- rep(0, n)
for(j in 1:n) {
  if(fwdEps[j,] <= 0) {
    countFwd[j] <- 1
  }
  if(ttmEps[j,] <= 0) {
    countTtm[j] <- 1
  }
}

# FWD EPS Filter
newEps <- rep(NA, (n-sum(countFwd)))
newPrices <- rep(NA, (n-sum(countFwd)))
newSymbols <- vector(mode="character", length=(n-sum(countFwd)))
counter <- 0
for(k in 1:n) {
  if(countFwd[k] == 0) {
    counter <- counter + 1
    newEps[counter] <- fwdEps[k,]
    newPrices[counter] <- prices[k,]
    newSymbols[counter] <- Symbols[k]
  } 
}
fwdEps <- newEps
prices2 <- newPrices
Symbols2 <- newSymbols
metrics2 <- matrix(c(Symbols2,prices2,fwdEps),nrow=length(fwdEps),ncol=3)

#TTM EPS Filter
newEps <- rep(NA, (n-sum(countTtm)))
newPrices <- rep(NA, (n-sum(countTtm)))
newSymbols <- vector(mode="character", length=(n-sum(countTtm)))
counter <- 0
for(p in 1:n) {
  if(countTtm[p] == 0) {
    counter <- counter + 1
    newEps[counter] <- ttmEps[p,]
    newPrices[counter] <- prices[p,]
    newSymbols[counter] <- Symbols[p]
  } 
}
ttmEps <- newEps
prices3 <- newPrices
Symbols3 <- newSymbols
metrics3 <- matrix(c(Symbols3,prices3,ttmEps),nrow=length(ttmEps),ncol=3)

##-------------------------------
## Get Valuation Multiples
##-------------------------------

# calculate enterprise value
#cash <- 17505
#ltDebt <- 2253
#stDebt <- 1168
cash <- 0
ltDebt <- 0
stDebt <- 0
netDebt <- stDebt + ltDebt - cash
enterprise <- marketCap + netDebt

# calculate multiples
salesMultiple <- enterprise / revenue
salesMultiple <- as.matrix(salesMultiple)
fwdPE <- prices2 / fwdEps
ttmPE <- prices3 / ttmEps

# ticker of interest multiples
subjectSales <- salesMultiple[mainTicker]
subjectTtmPE <- ttmPE[mainTicker]
subjectFwdPE <- fwdPE[mainTicker]

# peer group multiple
peerSales <- median(salesMultiple)
peerTtmPE <- median(ttmPE)
peerFwdPE <- median(fwdPE)
paste("Peer EV/Sales: ", peerSales, sep="")
paste("Peer TTM P/E: ", peerTtmPE, sep="")
paste("Peer FWD P/E: ", peerFwdPE, sep="")

# peer group graphical comparison
multipleGraph <- function() {
  par(mfrow=c(1,3))
  plot(salesMultiple,main="EV/Sales")
  abline(h=peerSales,col="red")
  abline(h=subjectSales,col="blue")
  plot(ttmPE,main="TTM P/E")
  abline(h=peerTtmPE,col="red")
  abline(h=subjectTtmPE,col="blue")
  plot(fwdPE,main="FWD P/E")
  abline(h=peerFwdPE,col="red")
  abline(h=subjectFwdPE,col="blue") 
}

##-------------------------------
## Get Valuation Premium
##-------------------------------

# calculate multiple premium
salesPrem <- subjectSales - peerSales
ttmPrem <- subjectTtmPE - peerTtmPE
fwdPrem <- subjectFwdPE - peerFwdPE
salesProp <- salesPrem/peerSales*100
ttmProp <- ttmPrem/peerTtmPE*100
fwdProp <- fwdPrem/peerFwdPE*100

nominalPremiumSales <- function() {
  paste("EV/Sales Premium: ", salesPrem, sep="")
}
nominalPremiumTTM <- function() {
  paste("TTM P/E Premium: ", ttmPrem, sep="")
}
nominalPremiumFWD <- function() {
  paste("FWD P/E Premium: ", fwdPrem, sep="")
}

percentPremiumSales <- function() {
  paste("EV/Sales Premium: ", salesProp, sep="")
}
percentPremiumTTM <- function() {
  paste("TTM P/E Premium: ", ttmProp, sep="") 
}
percentPremiumFWD <- function() {
  paste("FWD P/E Premium: ", fwdProp, sep="")
}

##-------------------------------
## Get Price Target
##-------------------------------
currPrice <- prices[mainTicker,]
currShares <- shares[mainTicker,]
currRev <- revenue[mainTicker,]

# price targets w/ premiums
priceTarget1 <- ((peerSales + choosePrem1)*currRev-netDebt)/currShares
priceTarget2 <- (peerTtmPE + choosePrem2)/subjectTtmPE*currPrice
priceTarget3 <- (peerFwdPE + choosePrem3)/subjectFwdPE*currPrice
finalTarget <- mean(c(priceTarget1,priceTarget2,priceTarget3))

# price targets assuming peer multiple
peerTarget1 <- ((peerSales)*currRev-netDebt)/currShares
peerTarget2 <- (peerTtmPE)/subjectTtmPE*currPrice
peerTarget3 <- (peerFwdPE)/subjectFwdPE*currPrice
peerTarget <- mean(c(peerTarget1,peerTarget2,peerTarget3))


# price target graphical comparison
ptGraph <- function() {
  par(mfrow=c(2,2))
  plot(salesMultiple,main="EV/Sales",ylab="EV/Sales")
  abline(h=peerSales,col="red")
  abline(h=subjectSales,col="blue")
  abline(h=peerSales + choosePrem1,col="green")
  plot(ttmPE,main="TTM P/E",ylab="TTM P/E")
  abline(h=peerTtmPE,col="red")
  abline(h=subjectTtmPE,col="blue")
  abline(h=peerTtmPE + choosePrem2,col="green")
  plot(fwdPE,main="FWD P/E",ylab="FWD P/E Ratio")
  abline(h=peerFwdPE,col="red")
  abline(h=subjectFwdPE,col="blue") 
  abline(h=peerFwdPE + choosePrem3,col="green")
  ymin <- min(finalTarget,peerTarget,min(timeSeries))*.95
  ymax <- max(finalTarget,peerTarget,max(timeSeries))*1.05
  plot(timeSeries,main="Share Price",
       ylab="Share Price",type="l",
       ylim=c(ymin,ymax))
  abline(h=peerTarget,col="red")
  abline(h=currPrice,col="blue") 
  abline(h=finalTarget,col="green")
}
