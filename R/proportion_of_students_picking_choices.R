#' Proportion of Students who Picked Each Option on a Test
#'
#' Finds the proportion of students who picked each option for each question on a test.
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
#'the list is a vector corresponding to one of the questions on the test, where this vector contains all of
#'the options for the given question, along with the proportion of students who chose each of the options
#'for the given question.
#'
#'
#'@export
#'

proportion_of_students_picking_choices <- function(exam, numchoicesperitem = NULL, check_format = TRUE){
  if(check_format == TRUE){
    stopifnot(correct_format(exam) == TRUE)
  }
  # Check if the user's numchoicesperitem makes sense, if they chose to include it
  if (!is.null(numchoicesperitem) == TRUE) {
    stopifnot(is.vector(numchoicesperitem), is.numeric(numchoicesperitem), length(numchoicesperitem) == num_items(exam, check_format = FALSE))
  }
  num_items <- num_items(exam, check_format = TRUE)
  num_students <- num_students(exam, check_format = TRUE)
  num_choices_per_item <- num_choices_per_item(exam, numchoicesperitem, check_format = TRUE)
  student_data <- student_data(exam, check_format = FALSE)
  letters_of_alphabet <- LETTERS[1:26]
  proportion_of_students_picking_choices <- list(length = num_items)
  for (j in 1:num_items) {
    results <- vector(length = num_choices_per_item[j])
    for (i in 1:num_choices_per_item[j]) {
      selected <- vector(length = num_students)
      for (k in 1:num_students) {
        selected[k] <- match(student_data[k, j], letters_of_alphabet) == i
      }
      results[i] <- length(which(selected == TRUE)) / num_students
    }
    proportion_of_students_picking_choices[[j]] <- results
    names(proportion_of_students_picking_choices[[j]]) <- LETTERS[1:num_choices_per_item[j]]
  }
  return(proportion_of_students_picking_choices)
}
