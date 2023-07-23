factor_experience_levels <- function(x){
  x <- as.numeric(factor(x, c('Not interested',
                   'No experience but would like to learn',
                   'Some experience (e.g. used in class)',
                   'Experience (e.g. multiple classes/applied skills to projects)')))
  return(x-1)
}

factor_grades <- function(x){
  x <- stringr::str_remove_all(x, '\\+|\\-')
  x <- ifelse(x == "S", "B", x)
  x <- ifelse(x == "U", NA, x)
  x <- ifelse(x == "D", "F", x)
  x <- as.numeric(factor(x, c('F', 'C', 'B', 'A')))
  return(x-1)
}

factor_needs <- function(x){
  x <- as.numeric(factor(x, c('Not necessary',
                              'Useful but not necessary',
                              'Necessary')))
  x <- ifelse(x == 1, 0, x)
  return(x)
}

make_empty_course_matrix <- function(x){
  # Add in matrix of all course codes
  data("UCDdept", envir=environment())
  dept <- UCDdept
  c_mat <- matrix(0, nrow = nrow(x), ncol = nrow(dept))
  # This is empty
  colnames(c_mat) <- dept$Subject.Code
  return(c_mat)
}
