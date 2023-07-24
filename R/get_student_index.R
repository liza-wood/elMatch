#' Pull student index
#'
#' @return A data frame of all submitted student applications
#'
#' @param project_url A URL to the Google Sheet where student interest survey responses are stored
#' @examples s_mat <- get_student_index(student_url)
#'
#' @import googlesheets4
#'
#' @export
#'


get_student_index <- function(student_url){
  s <- googlesheets4::read_sheet(student_url)
  s$id <- s$`Please enter your name in the following format: Last, First`
  index <- subset(s, select = c(id,
                                `Email Address`, Timestamp))
  colnames(index) <- c('id', 'Email', 'Time')
  return(index)
}
