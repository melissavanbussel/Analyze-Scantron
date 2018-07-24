#' Convert a Letter to a Number
#'
#' Finds the numeric position in the alphabet of a letter. For example, "A" = 1, "B" = 2, and so on.
#'
#'@param x An uppercase letter.
#'
#'@return Returns a numeric corresponding to the position in the alphabet of the letter given.
#'
#'
#'@export
#'

letter2num <- function(x) {
  stopifnot(x %in% LETTERS[1:26])
  utf8ToInt(x) - utf8ToInt("A") + 1L
}
