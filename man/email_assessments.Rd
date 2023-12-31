% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/email_assessments.R
\name{email_assessments}
\alias{email_assessments}
\title{Pull student index}
\usage{
email_assessments(sender)
}
\arguments{
\item{project_url}{A URL to the Google Sheet where student interest survey responses are stored}
}
\value{
A data frame of all submitted student applications, iterating each time to account for who has been sent assessment emails
}
\description{
Pull student index
}
\examples{
s_mat <- get_student_index(student_url)

}
