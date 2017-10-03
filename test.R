source('financial.R')

pos <- new("PositionsTable", "positions_data.csv")
normalize(pos)
show(pos)
print(pos)

table <- new("TransactionTable", "transactiondatabase.csv")
table <- read.transactions(table, 'transaction_data.csv')
table <- backup.transactions(table, 'transactiondatabase.csv')
show(table)
print(table)

stock <- new("StockData", "SSW", "NYSE", "Seaside Windows", 22.34, 234332, .12, "12/1/2016")
print(stock)
