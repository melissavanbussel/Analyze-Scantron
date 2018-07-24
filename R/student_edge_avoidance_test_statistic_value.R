#' Test Statistic Value of Edge Avoidance Hypothesis Test (Student Answers)
#'
#' Finds the test statistic value of a simple hypothesis test, testing whether there is significant
#' statistical evidence of students avoiding edge options.
#'
#'@param exam A data.frame object containing a test that follows the formatting of a typical Scantron file.
#'@param numchoicesperitem A vector which has the same length as the number of questions on the test, where each element in
#'the vector corresponds to the number of choices there were for each of the questions on the test. For example, if your
#'test had 35 questions which each had 5 options, numchoicesperitem = rep(5, 35).
#'@param check_format a logical value (default = TRUE) indicating whether the "exam" object should be tested
#'for correct formatting.
#'
#'@details The null hypothesis is that students are not avoiding edge options. That is, the true proportion of
#'students choosing edge options is the expected proportion of students choosing edge options. For example, consider a question
#'on a test which has options A, B, C, D and E, where the correct answer is "B". Say that 70% of students chose B; then we would
#'expect that the remaining 30% of students would choose options A, C, D, and E in similar proportions. In fact, we'd expect 15% of students
#'to choose edge options (A or E), and 15% of students to choose middle options (C or D). If 15% of students chose A or E, then
#'our null hypothesis would be true. The alternative hypothesis is that students are avoiding guessing edge options. That is,
#'the true proportion of students choosing edge options is smaller than the expected proportion of students choosing edge options.
#'For example, if only 2% of students chose A or E, it appears as though students are avoiding the edges. This function outputs the
#'test statistic value for the described hypothesis test.
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

student_edge_avoidance_test_statistic_value <- function(exam, numchoicesperitem = NULL, check_format = FALSE) {
  if (check_format == TRUE) {
    stopifnot(correct_format(exam) == TRUE)
  }
  num_items <- num_items(exam, check_format = FALSE)
  difficulty_indices <- difficulty_indices(exam, check_format = FALSE)
  answer_key <- answer_key(exam, check_format = FALSE)
  distractors <- distractors(exam, numchoicesperitem, check_format = FALSE)
  proportion_of_students_picking_choices <- proportion_of_students_picking_choices(exam, numchoicesperitem, check_format = FALSE)
  edge_or_not <- edge_or_not(exam, numchoicesperitem, check_format = FALSE)
  student_edge_avoidance_test_statistic_value <- vector(length = num_items)
  for (i in 1:num_items) {
    # what proportion of students should have picked edge cases, given that they got the question wrong?
    # almost like an expected conditional probability
    first_term_expected_proportion <- 1 - difficulty_indices[i]
    second_term_expected_proportion <- length(which(edge_or_not[[i]][-letter2num(unlist(unname(answer_key[i])))] == "EDGE")) / length(distractors[[i]])
    # expected proportion is "p" in the formula
    expected_proportion <- first_term_expected_proportion * second_term_expected_proportion
    # what proportion of students actually chose edge cases, given that they got the question wrong?
    # conditional probability: P(chose edge | got it wrong)
    # observed proportion is "phat" in the formula
    observed_proportion <- sum(proportion_of_students_picking_choices[[i]][edge_or_not[[i]] == "EDGE"][-letter2num(unname(unlist(answer_key[i])))])
    # hypothesis test on a single proportion. H0: phat = p, HA: phat < p (students are avoiding the edges)
    # test statistic value for the hypothesis test: z = (p-phat)/(sqrt(p*(1-p)/n))
    student_edge_avoidance_test_statistic_value[i] <- (expected_proportion - observed_proportion) / sqrt(expected_proportion * (1 - expected_proportion) / num_students(exam, check_format = FALSE))
  }
  return(student_edge_avoidance_test_statistic_value)
}
