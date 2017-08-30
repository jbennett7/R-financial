
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
