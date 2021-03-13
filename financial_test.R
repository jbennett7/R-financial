source('financial.R')

test.read.data <- function() {
    input <- './input.csv'
    raw_data <- read.data(input)
    create.df(raw_data)
}
#test.read.data()

test.positionTable.initialize <- function() {
    input <- './input.csv'
    PT <- new("PositionsTable", input)
    print(PT@.data$pct.of.account)
}
#test.positionTable.initialize()

