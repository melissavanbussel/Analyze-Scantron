#' Non-Functional Distractors (Less than 5% of Students Chose Distractor)
#'
#' Finds the non-functional distractors on the test for which less than 5% of students chose the distractor.
#'
#'@param exam A data.frame object containing a test that follows the formatting of a typical Scantron file.
#'@param numchoicesperitem A vector which has the same length as the number of questions on the test, where each element in
#'the vector corresponds to the number of choices there were for each of the questions on the test. For example, if your
#'test had 35 questions which each had 5 options, numchoicesperitem = rep(5, 35).
#'@param check_format a logical value (default = TRUE) indicating whether the "exam" object should be tested
#'for correct formatting.
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
#'@return Returns a list object which is the same length as the number of questions on the test. Each element in
#'the list is a vector corresponding to one of the questions on the test, where this vector contains the
#'non-functional distractors (for which less than 5% of students chose the distractor) for that question.
#'
#'@export



non_functional_distractors_less_5 <- function(exam, numchoicesperitem = NULL, check_format = TRUE) {
  if (check_format == TRUE) {
    stopifnot(correct_format(exam) == TRUE)
  }
  num_items <- num_items(exam, check_format = FALSE)
  distractors <- distractors(exam, numchoicesperitem, check_format = FALSE)
  non_functional_distractors_less_5 <- list(length = num_items)
  for (i in 1:num_items) {
    non_functional_distractors_less_5[[i]] <- distractors[[i]][distractors[[i]] < 0.05]
  }
  return(non_functional_distractors_less_5)
}
