source('financial.R')
library(quantmod)

pos <- new("PositionsTable", "input.csv")
#normalize(pos)
#show(pos)
#print(pos)

#table <- new("TransactionTable", "transactiondatabase.csv")
#table <- read.transactions(table, 'transaction_data.csv')
#table <- backup.transactions(table, 'transactiondatabase.csv')
#show(table)
#print(table)

pos@.data[,c("price")] <- getQuote(pos@.data[,c("symbol")])$Last
pos@.data[,c("value")] <- pos@.data[,c("price")] * pos@.data[,c("quantity")]
pos@.data[,c("gain-loss.pct")] <- (pos@.data[,c("value")] - pos@.data[,c("cost.basis")])/
    pos@.data[,c("cost.basis")] * 100
pos@.data[,c("symbol","quantity","price","value","cost.basis","gain-loss.pct")]
