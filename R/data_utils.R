#' @import utils

changeCsStringToDoubleVectorOrLeaveNull <- function(cs.string) {
    string.to.vector <- cs.string
    if (nchar(string.to.vector) > 0) {
        string.as.double <- c(as.double(strsplit(string.to.vector, ",")[[1]]))
        return(string.as.double)
    } else {
        return(NULL)
    }
}

changeCsStringToVectorOrLeaveNull <- function(cs.string) {
    string.to.vector <- cs.string
    if (nchar(string.to.vector) > 0) {
        string.as.string.vec <- c(strsplit(string.to.vector, ",")[[1]])
        return(string.as.string.vec)
    } else {
        return(NULL)
    }
}

changeEmptyStringToNull <- function(possiblyEmptyString) {
    if (possiblyEmptyString == "") {
        return(NULL)
    } else {
        return(possiblyEmptyString)
    }
}

changeStringToLogical <- function(logicalString) {
    if (logicalString) {
        return(T)
    } else {
        return(F)
    }
}

changeStringNullToNull <- function(nullStr) {
    if (nullStr == "NULL") {
        return(NULL)
    }
    return(nullStr)
}

errorForUi <- function(errorMessage) {
    return(paste("ERROR:", errorMessage))
}
