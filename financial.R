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
setGeneric("read.pos", function(object, filepath) standareadGeneric("read.pos"))
setMethod("read.pos", signature("PositionsData", "character"), function(object, filepath){
    data <- read.data(filepath)
    object@.headline <- data[1]
    object@.data <- data.frame(do.call(rbind, strsplit(data[4:(length(data)-2)], ',', fixed=TRUE)), stringsAsFactors=FALSE)
    colnames(object@.data) <- c('symbol','description','quantity','price',
        'price.change.dol','price.change.pct','market.value','day.change.dol',
        'day.change.pct','cost.basis','gain.loss.dol','gain.loss.pct','reinvest.dividends',
        'capital.gains','pct.of.account','security.type')
    object@.data$quantity <- as.double(object@.data$quantity)
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
setGeneric("normalize.data", function(object) standardGeneric("normalize.data"))
setMethod("normalize.data", signature("PositionsData"), function(object){
    df <- object@.data
    norm.symbol <- df$symbol
    norm.quantity <- df$quantity/sum(df$quantity)*100
    norm.cost.basis <- round(round(df$cost.basis/df$quantity,2)*norm.quantity,2)
    normalized <- data.frame(norm.symbol, norm.quantity, norm.cost.basis)
    return(normalized)
})

setClass(
    "TransactionData",
    representation(
        .headline = "character",
        .data = "data.frame"
    ),
    contains = "Data"
)
setGeneric("initialize.trans", function(object, filepath) standardGeneric("initialize.trans"))
setMethod("initialize.trans", signature("TransactionData", "character"), function(object, filepath){
    object@.data <- read.csv(filepath)
    return(object)
})
setGeneric("backup.trans", function(object, filepath) standardGeneric("backup.trans"))
setMethod("backup.trans", signature("TransactionData", "character"), function(object, filepath){
    write.csv(object@.data, filepath, row.names=FALSE)
    return(object)
})
setGeneric("read.trans", function(object, filepath) standareadGeneric("read.trans"))
setMethod("read.trans", signature("TransactionData", "character"), function(object, filepath){
    data <- read.data(filepath)
    object@.headline <- data[1]
    object@.data <- data.frame(do.call(rbind, strsplit(data[3:(length(data)-2)], ',', fixed=TRUE)), stringsAsFactors=FALSE)
    colnames(object@.data) <- c('date','action','symbol','description','quantity','price','fees.comm','amount')
    object@.data$date <- as.Date(sub('^(([0-9][0-9]/){2}[0-9]{4}) .*$','\\1', object@.data$date), "%m/%d/%Y")
    object@.data$quantity <- as.double(object@.data$quantity)
    object@.data$price <- as.double(sub('\\$','',object@.data$price))
    object@.data$fees.comm <- as.double(sub('\\$','',object@.data$fees.comm))
    object@.data$amount <- as.double(sub('\\$','',object@.data$amount))
    object@.data <- object@.data[ !duplicated(object@.data),]
    return(object)
})
