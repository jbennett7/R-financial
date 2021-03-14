source('financial.R')

test.read.s.data <- function(){
    input <- './input_all.csv'
    create.s.df(input)
}
#test.read.s.data()

test.read.f.data <- function(){
    input <- './input.csv'
    create.f.df(input)
}
#test.read.f.data()

test.positionTable.initialize <- function(){
    input <- './input_all.csv'
    PT <- new("PositionsTable", input)
    d <- PT@.data[order(PT@.data$symbol),c("symbol", "quantity", "price", "pct.of.account","security.type")]
    d
}
#test.positionTable.initialize()
