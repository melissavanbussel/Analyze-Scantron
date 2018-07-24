#' Proportion of Distractors Less than a Given Proportion
#'
#' Finds the proportion of the test's distractors which were selected by less than a given proportion of the students.
#'
#'@param exam A data.frame object containing a test that follows the formatting of a typical Scantron file.
#
#'@param check_format a logical value (default = TRUE) indicating whether the "exam" object should be tested
#'for correct formatting.
#'
#'@param numchoicesperitem A vector which has the same length as the number of questions on the test, where each element in
#'the vector corresponds to the number of choices there were for each of the questions on the test. For example, if your
#'test had 35 questions which each had 5 options, numchoicesperitem = rep(5, 35).
#'
#'@param givenproportion The proportion for which you would like to calculate the proportion of the test's distractors
#'that are less than this value. For example, one definition of a non-functional distractor is a distractor which
#'less than 5% of students chose. In this case, you would set givenproportion = 0.05.
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
#'@return Returns a numerical value from 0 to 1 corresponding to the proportion of distractors on the test for which
#'less than "givenproportion" of students chose the distractor. For example, if givenproportion = 0.05, and there were
#'200 total distractors on the test, and 30 of these distractors were chosen by less than 5% of students, then this
#'function would return 0.15 (which is 30/200).
#'
#'
#'@export

proportion_distractors_less_than_given_proportion <- function(exam, numchoicesperitem = NULL, givenproportion = 0.05, check_format = TRUE){
  if(check_format == TRUE){
    stopifnot(correct_format(exam) == TRUE)
  }
  # Check if the user's numchoicesperitem makes sense, if they chose to include it
  if (!is.null(numchoicesperitem) == TRUE) {
    stopifnot(is.vector(numchoicesperitem), is.numeric(numchoicesperitem), length(numchoicesperitem) == num_items(exam, check_format = FALSE))
  }
  # Check if the user's givenproportion makes sense, if they chose to include it
  if (!is.null(givenproportion) == TRUE) {
    stopifnot(is.numeric(givenproportion), length(givenproportion) == 1, givenproportion >= 0 & givenproportion <= 1)
  }
  distractors_less_than_given_proportion <- distractors_less_than_given_proportion(exam, numchoicesperitem, givenproportion, check_format = FALSE)
  total_number_of_distractors <- total_number_of_distractors(exam, numchoicesperitem, check_format = FALSE)
  return( sum(unlist(distractors_less_than_given_proportion) < givenproportion) / total_number_of_distractors )
}
