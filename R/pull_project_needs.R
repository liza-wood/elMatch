#' Pull research project description
#'
#' @return A rectangular matrix of researcher responses and scores
#'
#' @examples p_mat <- pull_project_needs()
#'
#' @import stringr
#' @import googlesheets4
#' @import tidyr
#'
#' @export

pull_research_needs <- function(project_url, student_url){
  # Read in researcher project needs
  p <- googlesheets4::read_sheet(project_url)
  # But also student needs because this sheet has the whole list of data types
  s <- googlesheets4::read_sheet(student_url)
  # Make an empty data frame of the data types to later join with projects
  type_cols <- which(stringr::str_detect(colnames(s), 'data types'))
  data_types <- stringr::str_extract(colnames(s)[type_cols], '(?<=\\[).*(?=\\])')
  dt <- data.frame(matrix(0, nrow(p), length(data_types)))
  colnames(dt) <- paste0('datatype: ', data_types)

  # Identify relevant data types and widen them out
  type_col <- which(stringr::str_detect(colnames(p), 'structured'))
  p <- tidyr::separate(p, type_col,
                       into = paste0('data_type', 1:length(type_cols)),
                       sep = ',')
  p <- tidyr::pivot_longer(p, cols = tidyr::contains('data_type'),
                           names_to = 'number', values_to = 'data_type')
  p <- p[!is.na(p$data_type), ]
  p <- subset(p, select = -number)
  p$data_type <- paste0('datatype: ', trimws(p$data_type))
  ## This is the score assigned to the project's data type
  p$dummy <- 3
  p <- tidyr::pivot_wider(p, names_from = data_type, values_from = dummy)
  p <- dplyr::left_join(p, dt)
  dt_cols <- stringr::str_which(colnames(p), 'datatype')
  p[,dt_cols] <- sapply(p[,dt_cols], function(x) ifelse(is.na(x), 0, x))

  skills_cols <- which(stringr::str_detect(colnames(p), 'skills'))
  colnames(p)[skills_cols] <- paste0('skills: ',
                                     stringr::str_extract(colnames(p)[skills_cols],
                                                   '(?<=\\[).*(?=\\])'))
  p[,skills_cols] <- sapply(p[,skills_cols], factor_needs)

  # Identifying relevant courses
  course_col <- which(stringr::str_detect(colnames(p), 'relevant courses'))
  ## Check that format matches, if not, delete
  p[ ,course_col] <- apply(p[course_col], 2, function(x) ifelse(stringr::str_detect(x, '[A-Za-z]{3}\\-?\\s?[0-9]{1,4}'), x, NA))
  p[[course_col]] <- ifelse(stringr::str_detect(p[[course_col]],
                                                '[A-Za-z]{3}\\-?\\s?[0-9]{1,4}'),
                            p[[course_col]], NA)
  ## Find max number of courses listed
  n <- max(stringr::str_count(p[[course_col]], '\\,'), na.rm = T)
  ## Make NAs characters because separate cannot handle. This is inefficient
  p[[course_col]] <- ifelse(is.na(p[[course_col]]), paste(rep(' , ', n)), p[[course_col]])
  p <- tidyr::separate(p, course_col,
                       into = paste0('pref_course', 1:(n+1)),
                       sep = ',')
  pref_cols <- stringr::str_which(colnames(p), 'pref_course')
  p[,pref_cols] <- sapply(p[,pref_cols], function(x) ifelse(x == " ", NA, x))
  p <- tidyr::pivot_longer(p, cols = stringr::str_which(colnames(p), 'pref_course'),
                           names_to = 'number', values_to = 'pref_course')
  p <- p[!is.na(p$pref_course) | (is.na(p$pref_course) &
           as.numeric(stringr::str_extract(p$number, '\\d{1,2}')) == 1),]
  p <- subset(p, select = -number)
  p$pref_course <- paste0('course: ', trimws(p$pref_course))

  ## This is the score assigned to the project's per course
  p$dummy <- 3
  p <- tidyr::pivot_wider(p, names_from = pref_course, values_from = dummy)
  nacol <- unique(stringr::str_detect(colnames(p), 'course: NA'))
  if(length(nacol) == 1 & nacol == T){
    p <- subset(p, select = -`course: NA`)
  }
  c_cols <- stringr::str_which(colnames(p), 'course')
  p[,c_cols] <- sapply(p[,c_cols], function(x) ifelse(is.na(x), 0, x))

  # Pull out subject area of faculty
  subj_col <- which(stringr::str_detect(colnames(p), 'subject area'))
  n <- max(apply(p[ ,subj_col], 2, function(x) stringr::str_count(x, '\\,')))

  p <- tidyr::separate(p, subj_col,
                       into = paste0('subj', 1:(n+1)),
                       sep = ',')
  p <- tidyr::pivot_longer(p, cols = stringr::str_which(colnames(p), 'subj'),
                           names_to = 'number', values_to = 'subj')
  p <- subset(p, select = -number)
  p <- p[!is.na(p$subj),]
  p$subj <- trimws(p$subj)

  # Bind subject area to abbr
  data("UCDdept", envir=environment())
  dept <- UCDdept

  p$id <- paste(p$`Please provide a brief title (3-5 words) of your research project/idea`,
                p$`Please enter your name in the following format: Last, First`,
                sep = " - ")
  subj <- data.frame('subj' = p$subj, 'id' = p$id)
  p <- unique(subset(p, select = -subj))
  subj <- dplyr::left_join(subj, dept, by = c('subj' = 'Subject.Description'))

  # I want to fill it, so that relevant discilpines  are filled

  c_mat <- make_empty_course_matrix(p)
  for(i in 1:ncol(c_mat)){
    for(j in 1:nrow(p)){
      if(colnames(c_mat)[i] %in% subj$Subject.Code[subj$id == p$id[j]]){
        c_mat[j,i] <- 1
      } else {
        next
      }
    }
  }

  # Need to add this as a matrix with
  ids <- p$id
  p <- subset(p, select = -c(Timestamp, `Email Address`, id,
                             `Please provide a brief title (3-5 words) of your research project/idea`,
                             `Please enter your name in the following format: Last, First`))
  p <- cbind(p, c_mat)
  p_mat <- as.matrix(p)
  rownames(p_mat) <- ids
  return(p_mat)
}


