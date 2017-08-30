
source('financial.R')

pos <- new("PositionsData", "positionsdata.csv")
normalize(pos)
show(pos)
print(pos)

table <- new("TransactionData", "transactiondatabase.csv")
table <- read.trans(table, 'more_entries.csv')
table <- backup.trans(table, 'transactiondatabase.csv')
show(table)
print(table)

stock <- new("ExchangeStockData", "SSW", "NYSE", "Seaside Windows", 22.34, 234332, .12, "12/1/2016")
print(stock)
