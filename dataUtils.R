changeCsStringToDoubleVectorOrLeaveNull <- function(csString) {
  stringToVector <- csString
  if (nchar(stringToVector) > 0) {
    stringToVector <- c(as.double(strsplit(stringToVector, ",")[[1]]))
    return(stringToVector)
  }
  else {
    return(NULL)
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
}

errorForUi <- function(errorMessage) {
  return(paste("ERROR:", errorMessage))
}