#' Return match scores by attribute
#'
#' @return A rectangular matrix of project x student with correlation values for different attributes
#' @param p_mat A matrix of research project needs
#' @param s_mat A matrix of student interest
#' @param attr A character vector with the attribute describing which match to evluate (datatype, skill, course, discipline)
#'
#' @examples course_match <- get_match_score(attr = 'course')
#'
#' @import stringr
#' @import googlesheets4
#' @import tidyr
#'
#' @export

get_match_score <- function(p_mat, s_mat, attr){
  if(attr == 'discipline'){
    p_dt <- p_mat[, !(stringr::str_detect(colnames(p_mat), '\\: '))]
    s_dt <- s_mat[, !(stringr::str_detect(colnames(s_mat), '\\: '))]
  } else {
    p_dt <- p_mat[, stringr::str_detect(colnames(p_mat), attr)]
    s_dt <- s_mat[, stringr::str_detect(colnames(s_mat), attr)]
  }

  mat <- matrix(NA, nrow = nrow(p_dt), ncol = nrow(s_dt))
  if(attr %in% c('datatype', 'skill')){
    for(i in 1:nrow(p_dt)){
      for(j in 1:nrow(s_dt)){
        r_needs <- p_dt[i,]
        r_needs <- r_needs[r_needs != 0]
        s_offers <- s_dt[j,names(r_needs)]
        score = r_needs - s_offers
        # you want this score to be small
        avg_score <- sum(score)/length(r_needs)
        mat[i,j] <- avg_score
      }
    }
  } else {
    for(i in 1:nrow(p_dt)){
      for(j in 1:nrow(s_dt)){
        r_needs <- p_dt[i,]
        if(length(r_needs) == 0){
          mat[i,j] <- NA
        }
        # This checks for courses to see if any align. if so:
        s_dt_omit <- na.omit(s_dt[i,])
        if(T %in% (names(r_needs) %in% colnames(s_dt_omit))){
          s_offers <- s_dt[j,names(r_needs)]
          score = r_needs - s_offers
          avg_score <- sum(score)/length(r_needs)
          mat[i,j] <- avg_score
          # If not:
        } else {
          s_offers <- rep(0, length(r_needs))
          score = r_needs - s_offers
          avg_score <- sum(score)/length(r_needs)
          mat[i,j] <- avg_score
        }

      }
    }
  }

  rownames(mat) <- rownames(p_dt)
  colnames(mat) <- rownames(s_dt)

  return(mat)
}
