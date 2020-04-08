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
setClass("StockData",
    representation(
        .symbol = "character",
        .exchange = "character",
        .name = "character",
        .price = "numeric",
        .outstanding = "numeric",
        .dividend = "numeric",
        .update = "Date"
    ),
    contains = "Data"
)
setMethod("initialize","StockData",
    function(.Object, symbol, exchange, name, price, outstanding, dividend, update){
        .Object@.symbol <- symbol
        .Object@.exchange <- exchange
        .Object@.name <- name
        .Object@.price <- price
        .Object@.outstanding <- outstanding
        .Object@.dividend <- dividend
        .Object@.update <- as.Date(update, "%m/%d/%Y")
        return(.Object)
    }
)
setMethod("show","StockData",
    function(object){
        cat("----------------------------\n")
        cat(object@.exchange,":",object@.symbol,"\n", sep="")
        cat(object@.name, "\n", sep="")
        cat("Price: $", object@.price, "\n", sep="")
        cat("Shares Outstanding: ", object@.outstanding, "\n", sep="")
        cat("Dividend per quarter: ", object@.dividend, "\n", sep="")
        cat("Last update: ", object@.update, "\n", sep="")
        cat("----------------------------\n")
    }
)
setMethod("print","StockData",
    function(x,...){
        cat("----------------------------\n")
        cat(x@.exchange,":",x@.symbol,"\n", sep="")
        cat(x@.name, "\n", sep="")
        cat("Price: $", x@.price, "\n", sep="")
        cat("Shares Outstanding: ", x@.outstanding, "\n", sep="")
        cat("Dividend per quarter: ", x@.dividend, "\n", sep="")
        cat("Last update: ", x@.update, "\n", sep="")
        cat("----------------------------\n")
    }
)

setClass("Table")
setClass("PositionsTable",
    representation(
        .headline = "character",
        .data = "data.frame"
    ),
    contains = "Table"
)
setMethod("initialize","PositionsTable",
    function(.Object, filepath){
        data <- read.data(filepath)
        .Object@.headline <- data[1]
        .Object@.data <- data.frame(
            do.call(rbind, strsplit(data[6:(length(data)-2)], ',', fixed=TRUE)),
            stringsAsFactors=FALSE)
        colnames(.Object@.data) <- strsplit(tolower(
            gsub("\\s", ".",
                gsub("[/]", "-",
                    gsub("[%]", "pct",
                        gsub("[$]", "dol", data[3]))))),
            ",",
            fixed=TRUE)[[1]]
        for(item in colnames(.Object@.data)){
            .Object@.data[,c(item)] <- gsub("[+$%]", "",
                gsub("N/A", NA, .Object@.data[,c(item)]))
            tryCatch(
                .Object@.data[,c(item)] <- as.double(.Object@.data[,c(item)]),
                warning = function(e) e)
        }
        return(.Object)
    }
)
setMethod("show","PositionsTable",
    function(object){
        cat("PositionsTable: "); print(object@.headline)
        print(object@.data)
    }
)
setMethod("print","PositionsTable",
    function(x,...){
        cat("PositionsTable: "); print(x@.headline)
        print(x@.data)
    }
)
setGeneric("normalize", function(object) standardGeneric("normalize"))
setMethod("normalize","PositionsTable",
    function(object){
        df <- object@.data
        norm.symbol <- df$symbol
        norm.quantity <- df$quantity/sum(df$quantity)*100
        norm.cost.basis <- round(round(df$cost.basis/df$quantity,2)*norm.quantity,2)
        normalized <- data.frame(norm.symbol, norm.quantity, norm.cost.basis)
        return(normalized)
    }
)
setGeneric("getData", function(object) standardGeneric("getData"))
setMethod("getData", "PositionsTable",
    function(object){
        return(object@.data)
    }
)

setClass(
    "TransactionTable",
    representation(
        .data = "data.frame"
    ),
    contains = "Data"
)
setMethod("initialize","TransactionTable",
    function(.Object, filepath=NULL){
        if( ! is.null(filepath) & file.exists(filepath) ){
            .Object@.data <- read.csv(filepath)
        }
        return(.Object)
    }
)
setMethod("show", "TransactionTable",
    function(object){
        cat("TransactionTable\n")
        print(object@.data)
    }
)
setMethod("print", "TransactionTable",
    function(x,...){
        cat("TransactionTable\n")
        print(x@.data)
    }
)
setGeneric("backup.transactions", function(object, filepath) standardGeneric("backup.transactions"))
setMethod("backup.transactions","TransactionTable",
    function(object, filepath){
        write.csv(object@.data, filepath, row.names=FALSE)
        return(object)
    }
)
setGeneric("read.transactions", function(object, filepath) standardGeneric("read.transactions"))
setMethod("read.transactions","TransactionTable",
    function(object, filepath){
        data <- read.data(filepath)
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
