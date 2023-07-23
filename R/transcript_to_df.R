#' Extract course-related data from PDF transcript
#'
#'
#' @param file The filepath to the PDF transcript
#'
#' @return A data frame of course codes, names, and grades
#'
#' @examples df <- transcript_to_df(file = "transcript.pdf")
#'
#' @import stringr
#' @import tabulizer
#' @import dplyr
#'
#' @export

transcript_to_df <- function(file){

  data("UCDdept", envir=environment())
  dept <- UCDdept
  t <- tabulizer::extract_text(file)
  if(stringr::str_detect(file, 'transcripts/')){
    id <- stringr::str_extract(file, '(?<=transcripts/).*')
  } else {
    id <- file
  }

  subj_code <- paste0(" ", paste(dept$Subject.Code, collapse = " |"), " ")
  grades <- paste0(" ", paste(c(LETTERS[c(1:4,6,9,19)],
                                paste0(LETTERS[c(1:4,6)], "\\+"),
                                paste0(LETTERS[c(1:4,6)], "\\-")),
                              collapse = " | "), " ")
  ext <- stringr::str_extract_all(t, paste0("(", subj_code, ")", ".*", "(",grades, ")"))
  df <- data.frame('raw' = ext[[1]])

  df$subj_code <- stringr::str_extract(df$raw,
                              paste0('^(',
                                     stringr::str_remove_all(subj_code, '\\s'), ')'))
  df$raw_cut <- trimws(stringr::str_remove_all(df$raw,
                                      paste0('^(',
                                             stringr::str_remove_all(subj_code, '\\s'), ')')))
  df$course_num <- stringr::str_extract(df$raw_cut, '^[0-9A-Z]{3,5}')
  df$raw_cut <- trimws(stringr::str_remove_all(df$raw_cut, '^[0-9A-Z]{3,5}'))
  df$grade <- stringr::str_extract(df$raw_cut,
                          paste0('(',
                                 stringr::str_remove_all(grades, '\\s'), ')$'))
  df$course_name <- trimws(stringr::str_remove_all(df$raw_cut,
                                          paste0('(',
                                                 stringr::str_remove_all(grades, '\\s'), ')$')))

  df <- df[,c('subj_code', 'course_num', 'course_name', 'grade')]
  df <- dplyr::left_join(df, dept, by = c('subj_code' = 'Subject.Code'))
  df$id <- id
  return(df)
}
