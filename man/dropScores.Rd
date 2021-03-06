% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drop.R
\name{dropScores}
\alias{dropScores}
\title{Compute aggregate score after dropping low components}
\usage{
dropScores(score, possible, drop = 0, value = c("proportion",
  "percent", "drops"))
}
\arguments{
\item{score}{a vector or matrix of item scores}

\item{possible}{a vector or matrix of maximum possible scores for each item}

\item{drop}{the number of items which may be dropped}

\item{value}{the type of return value desired.}
}
\value{
either the post-drop average (percent or proportion) or a vector of indices
indicating which items are to be dropped depending the value of \code{value}.
}
\description{
Given a set of scores, max possible scores, and number of scores that may be dropped,
this computes the best set of scores to drop, where best means that it produces the
largest average of remaining scores.
}
\details{
The algorithm is based on ideas in a paper by Daniel M. Kane
and Jonathan M. Kane available at \url{http://cseweb.ucsd.edu/~dakane/droplowest.pdf}.
}
\examples{
score <- c(80, 30, 2)
possible <- c(100, 100, 20)
dropScores( score, possible, drop = 1)
dropScores( score, possible, drop = 1, value = "percent")
# Note: second score is dropped, not the third.
dropScores( score, possible, drop = 1, value = "drops")
sum( score[c(1,3)] ) / sum( possible[c(1,3)] )
sum( score[c(1,2)] ) / sum( possible[c(1,2)] )
}
