#' Test Statistic Value of Edge Avoidance Hypothesis Test (Answer Key)
#'
#' Finds the test statistic value of a simple hypothesis test, testing whether there is significant
#' statistical evidence of the answer key avoiding edge options.
#'
#'@param exam A data.frame object containing a test that follows the formatting of a typical Scantron file.
#'@param numchoicesperitem A vector which has the same length as the number of questions on the test, where each element in
#'the vector corresponds to the number of choices there were for each of the questions on the test. For example, if your
#'test had 35 questions which each had 5 options, numchoicesperitem = rep(5, 35).
#'@param check_format a logical value (default = TRUE) indicating whether the "exam" object should be tested
#'for correct formatting.
#'
#'@details The null hypothesis is that the answer key does not avoid edge options. That is, the true proportion of
#'edge options in the answer key is the expected proportion of edge options in the answer key. For example, if every question
#'on the test had 5 options, the expected proportion of edge options in the answer key would be 0.4. The alternative hypothesis
#'is that the answer key does avoid edge options (that is, the true proportion of edge options in the answer key is less than
#'the expected proportion of edge options in the answer key.)
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
#'@return Returns a numeric value which corresponds to the test statistic value for the hypothesis test.
#'
#'@export

answer_key_test_statistic_value <- function(exam, numchoicesperitem = NULL, check_format = TRUE) {
  if (check_format == TRUE) {
    stopifnot(correct_format(exam) == TRUE)
  }
  answer_key_edge_or_not <- answer_key_edge_or_not(exam, numchoicesperitem, check_format = FALSE)
  num_choices_per_item <- num_choices_per_item(exam, numchoicesperitem, check_format = FALSE)
  num_items <- num_items(exam, check_format = FALSE)
  proportion_middle_answer_key <- length(which(answer_key_edge_or_not == "MIDDLE")) / length(answer_key_edge_or_not)
  expected_proportion_middle_answer_key <- (mean(num_choices_per_item) - 2) / mean(num_choices_per_item)
  # now do a hypothesis test; H0: true proportion of middle answer key = expected proportion of middle answer key; HA: true proportion > expected proportion (AKA, edge avoidance)
  return( (expected_proportion_middle_answer_key - proportion_middle_answer_key) / sqrt(expected_proportion_middle_answer_key * (1 - expected_proportion_middle_answer_key) / num_items) )
}
