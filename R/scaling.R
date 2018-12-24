
#' Scale raw grades to gp scores
#' 
#' Scale raw grades to gp scores
#' 
#' @param x raw scores to recode
#' @param breaks vector of break points between benchmarks
#' @param values vector of values to convert break points to.  Interpolation is
#'   used for other values.
#' 
#' @details  
#' Both `breaks` and `values` are sorted into decreasing order 
#' before matching them up.  If the lengths of `breaks` and `values`
#' differ, the longer is truncated to make them equal length.
#' 
#' @export
#' @examples
#' x <- c(50, 63, 78, 91, 54, 17)
#' raw2gp(x, breaks = seq(50, 100, by = 10))
#' 
raw2gp <- 
  function(
    x, breaks, 
    values = c(4.5, 3.5, 2.5, 1.5, 0.5, 0)[1:length(breaks)]) {
   
    breaks <- sort(breaks, decreasing = TRUE)
    values <- sort(values, decreasing = TRUE)
    
    if (length(breaks) < length(values)){
      values <- values[length(breaks)]
    }
    if (length(breaks) > length(values)){
      breaks <- breaks[length(values)]
    }
   
    x_beyond <- x[x < min(breaks) | x > max(breaks)] 
    n_beyond <- length(x_beyond)
    if (n_beyond > 0) {
      if (n_beyond < 10) {
        msg <- paste(n_beyond, "scores were outside the range of breaks:")
        msg <- paste(msg, paste(x_beyond, collapse = ", "), "\n")
      } else {
        msg <- paste(n_beyond, "scores were outside the range of breaks.")
      }
      warning(msg)
    }
    f <- approxfun(sort(breaks), sort(values), method = "linear", rule = 2)
    f(x)
  }
