% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proportion_non_functional_distractors.R
\name{proportion_non_functional_distractors}
\alias{proportion_non_functional_distractors}
\title{Proportion of Non-Functional Distractors on a Test}
\usage{
proportion_non_functional_distractors(exam, numchoicesperitem = NULL,
  check_format = TRUE)
}
\arguments{
\item{exam}{A data.frame object containing a test that follows the formatting of a typical Scantron file.}

\item{numchoicesperitem}{A vector which has the same length as the number of questions on the test, where each element in
the vector corresponds to the number of choices there were for each of the questions on the test. For example, if your
test had 35 questions which each had 5 options, numchoicesperitem = rep(5, 35).}

\item{check_format}{a logical value (default = TRUE) indicating whether the "exam" object should be tested
for correct formatting.}
}
\value{
Returns a proportion between 0 and 1 representing the proportion od total distractors on the
test which were non-functional.
}
\description{
Find the proportion of non-functional distractors on a test, where "non-functional" is
defined as a distractor which either has a positive correlation with student scores or
had less than 5% of students choosing it.
}
\note{
"Non-functional" is
defined as a distractor which either has a positive correlation with student scores or
had less than 5% of students choosing it.

The argument "exam" must be in the format of a typical Scantron results file.
Column 1 should correspond to ID (e.g., student number); column  2 should correspond to
DEPT (e.g., MATH); column 3 should correspond to COURSE CODE (e.g., 1051); the remaining
columns should each correspond to one of the questions on the test. The header of the data
frame should contain the column names, and row 1 of the data frame should contain the answer
key for the test. For example, if you had an exam with 25 students and 40 questions,
the data.frame object should have 26 rows and 43 columns.

The "check_format" argument defaults to null. If this is left as null, the function will call
the \code{\link{num_choices_per_item}} function, which will do its best to guess the number of options
for each question. This is done by looking at the student answers and finding the
"largest" answer for each question. For example, if at least one student answered "E", but no students
answered "F", the function would guess that there were 5 options for that question.
}
