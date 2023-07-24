#' Pull student index
#'
#' @return A data frame of all submitted student applications, iterating each time to account for who has been sent assessment emails
#'
#' @param project_url A URL to the Google Sheet where student interest survey responses are stored
#' @examples s_mat <- get_student_index(student_url)
#'
#' @import googlesheets4
#'
#' @export
#'


get_student_index <- function(student_url){
  data("s_index")
  s <- googlesheets4::read_sheet(student_url)
  # Create index for the fist time, no emails have been sent
  empty <- s_index$id[is.na(s_index$id)]
  if(length(empty) == nrow(s_index)) {
    s$id <- s$`Please enter your name in the following format: Last, First`
    index <- subset(s, select = c(id,
                                  `Email Address`, Timestamp))
    colnames(index) <- c('id', 'Email', 'Time')
    index$Sent_assessment <- NA
    s_index <- index
    usethis::use_data(s_index, overwrite = T)
    return(s_index)
    # If nothing has been updated
  } else if (nrow(s_index) == nrow(s)) {
    return(s_index)
    # If they are different lengths, add on
  } else {
    s$id <- s$`Please enter your name in the following format: Last, First`
    new_s <- s[!(s$id %in% s_index$id), ]
    new_index <- subset(new_s, select = c(id,
                                  `Email Address`, Timestamp))
    colnames(new_index) <- c('id', 'Email', 'Time')
    new_index$Sent_assessment <- NA
    s_index <- rbind(s_index, new_index)
    usethis::use_data(s_index, overwrite = T)
    return(s_index)

  }
}
