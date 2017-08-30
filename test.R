
source('financial.R')

#pos <- new("PositionsData")
#pos <- rd.pos(pos, 'Individual-Positions-2017-08-29-115357.CSV')
#normalize.data(pos)
#sum(normalize.data(pos)$norm.quantity)

table <- new("TransactionData")
table <- initialize.trans(table, 'transactiondatabase.csv')
table <- read.trans(table, 'more_entries.csv')
table
table <- backup.trans(table, 'transactiondatabase.csv')
