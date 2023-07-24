#' Pull researcher index
#'
#' @return A data frame of all submitted research projects
#'
#' @param project_url A URL to the Google Sheet where research project survey responses are stored
#' @examples p_mat <- get_project_index(project_url)
#'
#' @import googlesheets4
#'
#' @export
#'
get_project_index <- function(project_url){
  p <- googlesheets4::read_sheet(project_url)
  p$id <- paste(p$`Please provide a brief title (3-5 words) of your research project/idea`,
                p$`Please enter your name in the following format: Last, First`,
                sep = " - ")
  index <- subset(p, select = c(id,
                                `Please provide a brief title (3-5 words) of your research project/idea`,
                                `Please enter your name in the following format: Last, First`,
                                `Email Address`, Timestamp))
  colnames(index) <- c('id', 'Title', 'Name', 'Email', 'Time')
  return(index)
}

