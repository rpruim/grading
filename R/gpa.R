
#' Convert between Letter and Grade Point scales
#' 
#' Convert between Letter and Grade Point scales
#' 
#' @rdname letterGrades
#' @param x a character vector of letter grades or a numeric vector of grade points
#' @return a numeric vector of grade point values
#' @export
#' @examples
#' letter2gp( c("A", "B-", "HB+", "AU", "IB") )
#' 
letter2gp <- function(x) {
  if (length(x) < 1) return (numeric(0))
  sapply(x, function(g) {
    if (is.na(g)) return(NA)
    switch(g, 
           "A" = 4,
           "HA" = 4,
           "IA" = 4,
           "A-"= 3.7,
           "HA-"= 3.7,
           "IA-"= 3.7,
           "B+" = 3.3,
           "HB+" = 3.3,
           "IB+" = 3.3,
           "B" = 3.0,
           "HB" = 3.0,
           "IB" = 3.0,
           "B-" = 2.7,
           "HB-" = 2.7,
           "IB-" = 2.7,
           "C+" = 2.3,
           "HC+" = 2.3,
           "IC+" = 2.3,
           "C" = 2.0,
           "HC" = 2.0,
           "IC" = 2.0,
           "C-" = 1.7,
           "HC-" = 1.7,
           "IC-" = 1.7,
           "D+" = 1.3,
           "D" = 1.0,
           "D-" = 0.7,
           "F" = 0.0,
           "ID+" = 1.3,
           "ID" = 1.0,
           "ID-" = 0.7,
           "IF" = 0.0,
           "N" = 0.0,
           "IN" = 0.0,
           NA
    )
  }
  )
}

#' @rdname letterGrades
#' @export

gp2letter <- function(x) {
	standards <- as.vector( outer( c(-.3,0,.3), 0:4, "+") )
	letterGrades  <- as.vector( outer( c("-"," ","+"), c("F","D","C","B","A"), 
									  function(x,y) paste0(y,x) ) )
	nas <- is.na(x)
	x[nas] <- standards[1]
	res <- factor(
	  letterGrades [ sapply(x, function(x) which.min(abs(x-standards)) ) ],
	  levels = tail(letterGrades, -1)
	)
	res[nas] <- NA
  return(res)
}
