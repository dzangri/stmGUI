changeCsStringToDoubleVectorOrLeaveNull <- function(cs.string) {
  string.to.vector <- cs.string
  if (nchar(string.to.vector) > 0) {
    string.as.double <- c(as.double(strsplit(string.to.vector, ",")[[1]]))
    return(string.as.double)
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

load.packages <- function(packages){
  for(package in packages){
    if(!require(package, character.only=T)){
      install.packages(package, dependencies=T)
      require(package, character.only=T)
    }
  }
}