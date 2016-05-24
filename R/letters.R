#' Turn letter grades into ordered factor
#' 
#' Turn letter grades into ordered factor
#' 
#' @param x a character vector or factor with values that are possible letter grades.
#' @examples
#' factorLetters( c("A", "B+", "A-", "B ") )
#' @export

factorLetters <- function(x) {
  x <- as.character(x)
  x[nchar(x) == 1] <- paste0(x[nchar(x) == 1], " ")
  factor(x, ordered = TRUE,
         levels = as.vector( 
           outer( c("-", " ", "+"), c("F", "D", "C", "B", "A"), 
                  function(x, y) paste0(y, x) ) )
  )
}
