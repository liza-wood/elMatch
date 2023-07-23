#' Pull student interest forms
#'
#' @return A rectangular matrix of student responses and scores
#'
#' @examples s_mat <- pull_student_interest()
#'
#' @import stringr
#' @import googlesheets4
#' @import tidyr
#'
#' @export

pull_student_interest <- function(student_url){
  # Read in data from Drive
  s <- googlesheets4::read_sheet(student_url)

  # Rename columns and convert interest into factor score
  type_cols <- which(stringr::str_detect(colnames(s), 'data types'))
  colnames(s)[type_cols] <- paste0('datatype: ',
                                   stringr::str_extract(colnames(s)[type_cols], '(?<=\\[).*(?=\\])'))
  s[,type_cols] <- sapply(s[,type_cols], factor_experience_levels)

  # Rename columns and convert skills into factor score
  skills_cols <- which(stringr::str_detect(colnames(s), 'skills'))
  colnames(s)[skills_cols] <- paste0('skills: ',
                                     stringr::str_extract(colnames(s)[skills_cols],
                                                          '(?<=\\[).*(?=\\])'))
  s[,skills_cols] <- sapply(s[,skills_cols], factor_experience_levels)

  # Download transcripts they uploaded into a local transcripts folder
  t_col <- which(stringr::str_detect(colnames(s), 'Please upload'))

  dir.create('transcripts')
  t_names <- s[, t_col]
  t_names <- t_names[[1]]
  t_ids <- stringr::str_extract(t_names, '(?<=id\\=).*')
  sapply(1:length(t_names), function(x) {googledrive::drive_download(t_names[x],
                                     path = paste0('transcripts/', t_ids[x], '.pdf'),
                                     overwrite = T)})
  fls <- list.files('transcripts', full.names = T)

  # Run transcripts through transcript_to_df function to extract data
  all_t <- data.frame()
  for(i in fls){
    df <- transcript_to_df(i)
    all_t <- rbind(all_t, df)
  }

  all_t$grade <- factor_grades(all_t$grade)
  course_codes <- dplyr::count(dplyr::group_by(all_t, id, subj_code))
  course_codes <- subset(course_codes, select = -n)
  course_codes$id <- stringr::str_remove_all(course_codes$id, '\\.pdf')

  # Get transcript data into matrix
  all_t$course <- paste(all_t$subj_code, all_t$course_num, sep = '-')
  all_t <- unique(all_t[c('id', 'course', 'grade')])
  t_mat <- tidyr::pivot_wider(all_t, id_cols = id,
                              names_from = course,
                              values_from = grade)
  t_mat <- subset(t_mat, select = -c(id))
  t_mat <- as.matrix(t_mat)
  colnames(t_mat) <- paste0('course: ', colnames(t_mat))

  # Pull out IDs from Drive upload file and use those to label and pull out survey matrix
  s$id <- t_ids
  index <- s[,c('id', 'Email Address', 'Timestamp')]
  s_mat <- subset(s, select = -c(id,`Email Address`, Timestamp, t_col))
  s_mat <- as.matrix(s_mat)

  c_mat <- make_empty_course_matrix(s)

  # I want to fill it, so that relevant courses are filled
  for(i in 1:ncol(c_mat)){
    for(j in 1:nrow(s)){
       if(colnames(c_mat)[i] %in% course_codes$subj_code[course_codes$id == s$id[j]]){
         c_mat[j,i] <- 1
       } else {
         next
       }
    }
  }

  # Bind matrices and return
  s_mat <- cbind(s_mat, t_mat)
  s_mat <- cbind(s_mat, c_mat)
  rownames(s_mat) <- index$id
  return(s_mat)
}


