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

setClass("PositionsData",
    representation(
        .headline = "character",
        .data = "data.frame"
    ),
    contains = "Data"
)
setMethod("initialize","PositionsData",
    function(.Object, filepath){
        data <- read.data(filepath)
        .Object@.headline <- data[1]
        .Object@.data <- data.frame(do.call(rbind, strsplit(data[4:(length(data)-2)], ',', fixed=TRUE)), stringsAsFactors=FALSE)
        colnames(.Object@.data) <- c('symbol','description','quantity','price',
            'price.change.dol','price.change.pct','market.value','day.change.dol',
            'day.change.pct','cost.basis','gain.loss.dol','gain.loss.pct','reinvest.dividends',
            'capital.gains','pct.of.account','security.type')
        .Object@.data$quantity <- as.double(.Object@.data$quantity)
        .Object@.data$price <- as.double(sub('\\$','',.Object@.data$price))
        .Object@.data$price.change.dol <- as.double(sub('\\$','',.Object@.data$price.change.dol))
        .Object@.data$price.change.pct <- as.double(sub('\\%','',.Object@.data$price.change.pct))
        .Object@.data$market.value <- as.double(sub('\\$','',.Object@.data$market.value))
        .Object@.data$day.change.dol <- as.double(sub('\\$','',.Object@.data$day.change.dol))
        .Object@.data$day.change.pct <- as.double(sub('\\%','',.Object@.data$day.change.pct))
        .Object@.data$cost.basis <- as.double(sub('\\$','',.Object@.data$cost.basis))
        .Object@.data$gain.loss.dol <- as.double(sub('\\$','',.Object@.data$gain.loss.dol))
        .Object@.data$gain.loss.pct <- as.double(sub('\\%','',.Object@.data$gain.loss.pct))
        .Object@.data$pct.of.account <- as.double(sub('\\%','',.Object@.data$pct.of.account))
        return(.Object)
    }
)
setGeneric("normalize", function(object) standardGeneric("normalize"))
setMethod("normalize","PositionsData",
    function(object){
        df <- object@.data
        norm.symbol <- df$symbol
        norm.quantity <- df$quantity/sum(df$quantity)*100
        norm.cost.basis <- round(round(df$cost.basis/df$quantity,2)*norm.quantity,2)
        normalized <- data.frame(norm.symbol, norm.quantity, norm.cost.basis)
        return(normalized)
    }
)

setClass(
    "TransactionData",
    representation(
        .headline = "character",
        .data = "data.frame"
    ),
    contains = "Data"
)
setMethod("initialize","TransactionData",
    function(.Object, filepath){
        .Object@.data <- read.csv(filepath)
        return(.Object)
    }
)
setMethod("show", "TransactionData",
    function(object){
        print(object@.data)
    }
)
setGeneric("backup.trans", function(object, filepath) standardGeneric("backup.trans"))
setMethod("backup.trans","TransactionData",
    function(object, filepath){
        write.csv(object@.data, filepath, row.names=FALSE)
        return(object)
    }
)
setGeneric("read.trans", function(object, filepath) standareadGeneric("read.trans"))
setMethod("read.trans","TransactionData",
    function(object, filepath){
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
    }
)
