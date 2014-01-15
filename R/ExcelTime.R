#' Excel Time conversions
#' 
#' Tools to convert to/from Excel time data.
#' 
#' @rdname ExcelTime
#' @param x a numeric vector
#' @param debug a logical
#' @return a numeric or character vector

excelTime2char <- function(x, debug=FALSE) {
  x <- as.numeric(as.character(x))
  minutes <- round( x * 24 * 60 )
  hour <- as.integer(trunc( minutes / 60 ))
  minute <- as.integer( minutes - 60 * hour )
  result <- paste( sprintf("%02d",hour), sprintf("%02d",minute), sep=":") 
  if (debug) return ( paste(x, hour, minute, result, sep="->") )
  return(result)
}

#' @rdname ExcelTime
excelTime2num<- function(x, debug=FALSE) {
  x <- as.numeric(as.character(x))
  minutes <- round( x * 24 * 60 )
  hour <- as.integer(trunc( minutes / 60 ))
  minute <- as.integer( minutes - 60 * hour )
  result <- hour + minute/60
  if (debug) return ( paste(x, hour, minute, result, sep="->") )
  return(result)
}
