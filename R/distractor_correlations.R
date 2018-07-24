#' Distractor Correlations
#'
#' Finds the correlations between each of the distractors on the test and student scores.
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
#'correlations between each distractor on that question, and the overall student scores.
#'
#'@export


distractor_correlations <- function(exam, numchoicesperitem = NULL, check_format = TRUE) {
  if (check_format == TRUE) {
    stopifnot(correct_format(exam) == TRUE)
  }
  num_items <- num_items(exam, check_format = FALSE)
  num_choices_per_item <- num_choices_per_item(exam, numchoicesperitem, check_format = FALSE)
  num_students <- num_students(exam, check_format = FALSE)
  student_data <- student_data(exam, check_format = FALSE)
  answer_key <- answer_key(exam, check_format = FALSE)
  distractors <- distractors(exam, check_format = FALSE)
  student_scores_item_included <- student_scores_item_included(exam, check_format = FALSE)
  binary_student_answers <- vector(length = num_items, mode = "list")
  for (k in 1:num_items) {
    my_letters <- LETTERS[1:num_choices_per_item[k]]
    binary_student_answers[[k]] <- data.frame("Student" = 1:num_students)
    for (i in 1:num_choices_per_item[k]) {
      binary_student_answers[[k]] <- cbind(binary_student_answers[[k]], rep(0, num_students))
    }
    colnames(binary_student_answers[[k]]) <- c("Student", my_letters)
    for (i in 1:num_choices_per_item[k]) {
      for (j in 1:num_students) {
        if (student_data[j,k] == LETTERS[i]) {
          binary_student_answers[[k]][j,i+1] <- 1
        }
      }
    }
  }

  for (i in 1:length(binary_student_answers)) {
    binary_student_answers[[i]] <- binary_student_answers[[i]][,-(letter2num(as.character(answer_key[i])) + 1)]
  }

  # correlations: note that some will have NA, this happens because literally 0 people chose that distractor so there is 0 variance for that distractor, correlation cannot be calculated
  distractor_correlations <- vector(length = num_items, mode = "list")
  for (j in 1:num_items) {
    distractor_correlations[[j]] <- vector(length = (ncol(binary_student_answers[[j]]) - 1) )
    for (i in 2:ncol(binary_student_answers[[j]])) {
      distractor_correlations[[j]][i - 1] <- cor(binary_student_answers[[j]][,i], student_scores_item_included)
      names(distractor_correlations[[j]]) <- names(distractors[[j]])
    }
  }
  return(distractor_correlations)
}
