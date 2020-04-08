source('financial.R')
library(quantmod)

invest <- 100

too.high <- c(5,  20, 15, 15)
high     <- c(10, 30, 20, 10)
midlevel <- c(20, 50, 25, 5 )
low      <- c(25, 45, 20, 2 )
too.low  <- c(35, 40, 25, 0 )


pos <- new("PositionsTable", "input.csv")
data <- pos@.data[
    pos@.data[,c("symbol")] %in% c("SCHE", "SCHV", "SCHA", "SCHP"),
    c("symbol", "quantity")]
data[,c("target.pct")] <- midlevel
data[,c("stock.price")] <- getQuote(data[,c("symbol")])$Last
data[,c("value")] <- data[,c("quantity")] * data[,c("stock.price")]
total <- sum(data[,c("value")])
data[,c("current.pct")] <- data[,c("value")]/total*100
data[,c("delta.pct")] <- data[,c("target.pct")] - data[,c("current.pct")]
invest <- (data[,c("target.pct")]/100)*invest +
    (data[,c("delta.pct")]/100)*invest
invest
sum(invest)
