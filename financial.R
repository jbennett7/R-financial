setClass("Data")
setClass(
    "NormalizedData", 
    representation( 
        .data = "data.frame",
        .data.file = "character"
    ),
    contains = "Data"
)
setGeneric("normalize.data", function(object, file) standardGeneric("normalize.data"))
setMethod("normalize.data", signature("NormalizedData", "character"), function(object, file){
    df <- read.csv(file)
    df <- df[df$Symbol != 'Total',]
    df$Cost.Basis <- as.double(sub('\\$','',df$Cost.Basis))
    norm.symbol <- df$Symbol
    norm.quantity <- df$Quantity/sum(df$Quantity)*100
    norm.cost.basis <- round(round(df$Cost.Basis/df$Quantity,2)*norm.quantity,2)
    normalized <- data.frame(norm.symbol, norm.quantity, norm.cost.basis)
    object@.data <- normalized
    return(object)
})
#ndata <- new("NormalizedData", .data.file = 'sanatized.csv')
#ndata <- normalize.data(ndata, 'sanatized.csv')
#ndata@.data

setClass(
    "TransactionData",
    representation(
        .table = "data.frame"
    ),
    contains = "Data"
)
setGeneric("add.entries", function(object, file) standardGeneric("add.entries"))
setMethod("add.entries", signature("TransactionData", "character"), function(object, file){
    df <- read.csv(file)
    df[,c('Date')] <- gsub('^(([0-9][0-9]/){2}[0-9]{4}) .*$','\\1', df[,c('Date')])
    df <- df[, !(names(df) %in% c('X'))]
    trasaction.database <- object@.table
    transaction.database <- rbind(transaction.database, df)
    transaction.database <- transaction.database[!duplicated(transaction.database),]
    object@.table <- transaction.database
    return(object)
})
#table <- new("TransactionData")
#table@.table <- read.csv('transactiondatabase.csv')
#table@.table
