#' Pull research project description
#'
#' @return A rectangular matrix of researcher responses and scores
#'
#' @examples r_mat <- pull_research_needs()
#'
#' @import stringr
#' @import googlesheets4
#' @import tidyr
#'
#' @export

pull_research_needs <- function(project_url, student_url){
  # Read in researcher project needs
  r <- googlesheets4::read_sheet(project_url)
  # But also student needs because this sheet has the whole list of data types
  s <- googlesheets4::read_sheet(student_url)
  # Make an empty data frame of the data types to later join with projects
  type_cols <- which(stringr::str_detect(colnames(s), 'data types'))
  data_types <- stringr::str_extract(colnames(s)[type_cols], '(?<=\\[).*(?=\\])')
  dt <- data.frame(matrix(0, nrow(r), length(data_types)))
  colnames(dt) <- paste0('datatype: ', data_types)

  # Identify relevant data types and widen them out
  type_col <- which(stringr::str_detect(colnames(r), 'structured'))
  r <- tidyr::separate(r, type_col,
                       into = paste0('data_type', 1:length(type_cols)),
                       sep = ',')
  r <- tidyr::pivot_longer(r, cols = tidyr::contains('data_type'),
                           names_to = 'number', values_to = 'data_type')
  r <- r[!is.na(r$data_type), ]
  r <- subset(r, select = -number)
  r$data_type <- paste0('datatype: ', trimws(r$data_type))
  ## This is the score assigned to the project's data type
  r$dummy <- 3
  r <- tidyr::pivot_wider(r, names_from = data_type, values_from = dummy)
  r <- dplyr::left_join(r, dt)
  r[is.na(r)] <- 0

  skills_cols <- which(stringr::str_detect(colnames(r), 'skills'))
  colnames(r)[skills_cols] <- paste0('skills: ',
                                     stringr::str_extract(colnames(r)[skills_cols],
                                                   '(?<=\\[).*(?=\\])'))
  r[,skills_cols] <- sapply(r[,skills_cols], factor_needs)

  # Identifying relevant courses
  course_col <- which(stringr::str_detect(colnames(r), 'relevant courses'))
  n <- max(apply(r[ ,course_col], 2, function(x) stringr::str_count(x, '\\,')))
  r <- tidyr::separate(r, course_col,
                       into = paste0('pref_course', 1:(n+1)),
                       sep = ',')
  r <- tidyr::pivot_longer(r, cols = stringr::str_which(colnames(r), 'pref_course'),
                           names_to = 'number', values_to = 'pref_course')
  r <- r[!is.na(r$pref_course),]
  r <- subset(r, select = -number)
  r$pref_course <- paste0('course: ', trimws(r$pref_course))

  ## This is the score assigned to the project's per course
  r$dummy <- 3
  r <- tidyr::pivot_wider(r, names_from = pref_course, values_from = dummy)
  r[is.na(r)] <- 0

  # Pull out subject area of faculty
  subj_col <- which(stringr::str_detect(colnames(r), 'subject area'))
  n <- max(apply(r[ ,subj_col], 2, function(x) stringr::str_count(x, '\\,')))

  r <- tidyr::separate(r, subj_col,
                       into = paste0('subj', 1:(n+1)),
                       sep = ',')
  r <- tidyr::pivot_longer(r, cols = stringr::str_which(colnames(r), 'subj'),
                           names_to = 'number', values_to = 'subj')
  r <- subset(r, select = -number)
  r <- r[!is.na(r$subj),]
  r$subj <- trimws(r$subj)

  # Bind subject area to abbr
  data("UCDdept", envir=environment())
  dept <- UCDdept

  r$id <- paste(r$Timestamp, r$`Email Address`)
  subj <- data.frame('subj' = r$subj, 'id' = r$id)
  r <- unique(subset(r, select = -subj))
  subj <- dplyr::left_join(subj, dept, by = c('subj' = 'Subject.Description'))

  # I want to fill it, so that relevant discilpines  are filled

  c_mat <- make_empty_course_matrix(r)
  for(i in 1:ncol(c_mat)){
    for(j in 1:nrow(r)){
      if(colnames(c_mat)[i] %in% subj$Subject.Code[subj$id == r$id[j]]){
        c_mat[j,i] <- 1
      } else {
        next
      }
    }
  }

  # Need to add this as a matrix with
  ids <- r$id
  r <- subset(r, select = -c(Timestamp, `Email Address`, id))
  r <- cbind(r, c_mat)
  r_mat <- as.matrix(r)
  rownames(r_mat) <- ids
  colnames(r_mat)
  return(r_mat)
}


