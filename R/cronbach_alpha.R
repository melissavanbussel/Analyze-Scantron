#' Cronbach Alpha
#'
#' Finds the Cronbach Alpha of the test.
#'
#'@param exam A data.frame object containing a test that follows the formatting of a typical Scantron file.
#
#'@param check_format a logical value (default = TRUE) indicating whether the "exam" object should be tested
#'for correct formatting.
#'
#'@details Cronbach Alpha is a measure of the reliability of a test. Its value can range from 0 to 1. Higher values are better. It should be noted,
#'however, that increasing the number of questions on a test will always increase the reliability of the test.
#'
#'@note The argument "exam" must be in the format of a typical Scantron results file.
#'Column 1 should correspond to ID (e.g., student number); column  2 should correspond to
#'DEPT (e.g., MATH); column 3 should correspond to COURSE CODE (e.g., 1051); the remaining
#'columns should each correspond to one of the questions on the test. The header of the data
#'frame should contain the column names, and row 1 of the data frame should contain the answer
#'key for the test. For example, if you had an exam with 25 students and 40 questions,
#'the data.frame object should have 26 rows and 43 columns.
#'
#'@note The "check_format" argument defaults to null. If this is left as null, the function will call
#'the \code{\link{num_choices_per_item}} function, which will do its best to guess the number of options
#'for each question. This is done by looking at the student answers and finding the
#'"largest" answer for each question. For example, if at least one student answered "E", but no students
#'answered "F", the function would guess that there were 5 options for that question.
#'
#'@return Returns a numeric value corresponding to the adjusted Cronbach Alpha of the test.
#'
#'@seealso \code{\link{adjusted_cronbach_alpha}}
#'
#'@export

cronbach_alpha <- function(exam, check_format = TRUE) {
  if(check_format == TRUE){
    stopifnot(correct_format(exam) == TRUE)
  }
  num_items <- num_items(exam, check_format = FALSE)
  p1 <- difficulty_indices(exam, check_format = FALSE)
  p2 <- 1 - p1
  student_scores_item_included <- student_scores_item_included(exam, check_format = FALSE)
  return( num_items*(1-sum(p1*p2)/var(student_scores_item_included))/(num_items-1)  )
}
