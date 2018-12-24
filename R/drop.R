
#' @importFrom stats uniroot
#' @importFrom utils head tail
 
NA

#' Compute aggregate score after dropping low components
#'
#' Given a set of scores, max possible scores, and number of scores that may be dropped,
#' this computes the best set of scores to drop, where best means that it produces the
#' largest average of remaining scores.
#'
#' @details The algorithm is based on ideas in a paper by Daniel M. Kane
#' and Jonathan M. Kane available at \url{http://cseweb.ucsd.edu/~dakane/droplowest.pdf}.
#'
#' @param score a vector or matrix of item scores
#' @param possible a vector or matrix of maximum possible scores for each item
#' @param drop the number of items which may be dropped
#' @param value the type of return value desired.
#' @return either the post-drop average (percent or proportion) or a vector of indices
#' indicating which items are to be dropped depending the value of \code{value}.
#' @export
#' @examples
#' score <- c(80, 30, 2)
#' possible <- c(100, 100, 20)
#' dropScores( score, possible, drop = 1)
#' dropScores( score, possible, drop = 1, value = "percent")
#' # Note: second score is dropped, not the third.
#' dropScores( score, possible, drop = 1, value = "drops")
#' sum( score[c(1,3)] ) / sum( possible[c(1,3)] )
#' sum( score[c(1,2)] ) / sum( possible[c(1,2)] )
#
dropScores <- function( score, possible, drop=0, value=c("proportion","percent","drops")) {
	value <- match.arg(value)
	score <- unlist(score)
	possible <- unlist(possible)
	keep <- length(score) - drop
	score[ is.na(score) ] <- 0
	Flocal <- function( q ) {
		fvals <- score - q * possible
		fvals <- sort(fvals)
		base::sum( tail(fvals, keep) )  # sum of biggest
	}
	eps <- 1e-4
	a <- min(score/possible) - eps
	b <- max(score/possible) + eps
	res <- uniroot( Flocal, c(a, b) )$root
	drops <- head( order(score - res * possible ), drop )
	prop <- base::sum( score[-drops]) / sum(possible[-drops])
	return( switch( value,
				   "proportion" = prop,
				   "percent" = 100 * prop,
				   "drops" = drops
				   ) )
}

#' Collect a series of scores (with drops)
#' 
#' Aggregate scores from matching columns, possibly after dropping some of the lowest scores.
#' 
#' @param data A data frame containing the gradebook.
#' @param pattern A regular expression matched against the names in `data` for selecting
#'   the columns to be aggregated.
#' @param as A string naming the new variable to be computed.
#' @param format One of `"percent"`, "`proportion"`.
#' @param ignore.case A logical indicating whether case should be ignored when matching
#'   `pattern`.
#' @return A data frame
#' @export  
process_scores <- 
  function(data, pattern, 
           as = "hw", drop = 0, 
           format = c("percent", "proportion"), ignore.case = TRUE) {
   
    format = match.arg(format)
    
    data$.id <- 1:nrow(data)
    
    which = grep(pattern, names(data), ignore.case = ignore.case)
    
    cols <- union(which, grep(".id", names(data)))
    
    which = grep(pattern, names(data[, cols]), ignore.case = ignore.case)
    
    data2 <- 
      data[, cols] %>% 
      tidyr::gather(key = ".item", value = ".score", which) %>% 
      # select(.id, .item, .score) %>%
      arrange(.item)
    
    data2$.score[is.na(data2$.score)] <- 0
    
    MaxHW <- data2 %>%
      group_by(.item) %>%
      summarise(max = max(.score, na.rm = TRUE))
    
    res <-
      data2 %>%
      group_by(.id) %>%
      arrange(.item) %>%
      summarise(
        # ..hw..sum = sum(.score, na.rm = TRUE),
        ..hw.. = grading::dropScores(.score, MaxHW$max, drop = drop, value = "percent")) %>%
      inner_join(data, by = ".id") 
    
    names(res) <- gsub("..hw..", as, names(res), fixed = TRUE)
    res
  }