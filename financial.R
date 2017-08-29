read.data <- function(filepath){
    conn <- file(filepath, 'r')
    data <- readLines(conn)
    close(conn)
    for (i in 1:length(data)){
        # To distinguish between misc lines (not column names) and data use the number of commas as an indicator.
        if (lengths(regmatches(data[i],gregexpr(',',data[i]))) > 5){
            # This is a column header or its data.
            ## This uses multiple substitutions in order to eliminate commas inside values.
            data[i] <- gsub('\\|',',',gsub(',','',gsub('\\\"','',gsub('\\\",\\\"','|',data[i]))))
        } else {
            # This is just miscellaneous text.
            data[i] <- gsub('\\\"','',data[i])
        }
    }
    return(data)
}

setClass("Data")

setClass(
    "PositionsData",
    representation(
        .headline = "character",
        .data = "data.frame"
    ),
    contains = "Data"
)
setGeneric("rd.pos", function(object, filepath) standardGeneric("rd.pos"))
setMethod("rd.pos", signature("PositionsData", "character"), function(object, filepath){
    data <- read.data(filepath)
    object@.headline <- data[1]
    object@.data <- data.frame(do.call(rbind, strsplit(data[4:(length(data)-2)], ',', fixed=TRUE)))
#   colnames(object@.data) <- unlist(data[3])
    colnames(object@.data) <- c('symbol','description','quantity','price',
        'price.change.dol','price.change.pct','market.value','day.change.dol',
        'day.change.pct','cost.basis','gain.loss.dol','gain.loss.pct','reinvest.dividends',
        'capital.gains','pct.of.account','security.type')
    object@.data$price <- as.double(sub('\\$','',object@.data$price))
    object@.data$price.change.dol <- as.double(sub('\\$','',object@.data$price.change.dol))
    object@.data$price.change.pct <- as.double(sub('\\%','',object@.data$price.change.pct))
    object@.data$market.value <- as.double(sub('\\$','',object@.data$market.value))
    object@.data$day.change.dol <- as.double(sub('\\$','',object@.data$day.change.dol))
    object@.data$day.change.pct <- as.double(sub('\\%','',object@.data$day.change.pct))
    object@.data$cost.basis <- as.double(sub('\\$','',object@.data$cost.basis))
    object@.data$gain.loss.dol <- as.double(sub('\\$','',object@.data$gain.loss.dol))
    object@.data$gain.loss.pct <- as.double(sub('\\%','',object@.data$gain.loss.pct))
    object@.data$pct.of.account <- as.double(sub('\\%','',object@.data$pct.of.account))
    return(object)
})

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

pos <- new("PositionsData")
pos <- rd.pos(pos,'Individual-Positions-2017-08-29-115357.CSV')
pos@.data
