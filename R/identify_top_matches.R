#' Identify top students for project
#'
#' @return List of projects, each of which contains a data frame with top matched students by four criteria and their scores, where scores closer to zero are better
#'
#' @param p_mat A matrix of research project needs
#' @param s_mat A matrix of student interest
#' @param cutoff
#'
#' @examples proj_list <- identify_top_matches(p_mat, s_mat)
#'
#' @import stringr
#' @import dplyr
#' @import tidyr
#'
#' @export

identify_top_matches <- function(p_mat, s_mat, cutoff){
  #roxygen2::load_pkgload('R/get_match_score.R')
  dt_mat <- flip_scores(get_match_score(p_mat, s_mat, attr = 'datatype'))
  sk_mat <- flip_scores(get_match_score(p_mat, s_mat, attr = 'skill'))
  co_mat <- flip_scores(get_match_score(p_mat, s_mat, attr = 'course'))
  di_mat <- flip_scores(get_match_score(p_mat, s_mat, attr = 'discipline'))


  proj_matches <- list()
  for(i in 1:nrow(dt_mat)){
    proj = rownames(dt_mat)[i]
    dt_proj <- dt_mat[rownames(dt_mat) == proj, ]
    # Take 'top' scores. Right now score is 1 (highest) to 3 (lowest).
    # Eliminating anything greater than or equal to 2
    dt_min_score <- dt_proj[dt_proj < cutoff]
    dt_best_s <- names(dt_proj[dt_proj < cutoff])

    sk_proj <- sk_mat[rownames(sk_mat) == proj, ]
    sk_min_score <- sk_proj[sk_proj < cutoff]
    sk_best_s <- names(sk_proj[sk_proj < cutoff])

    co_proj <- co_mat[rownames(co_mat) == proj, ]
    co_min_score <- co_proj[co_proj < cutoff]
    co_best_s <- names(co_proj[co_proj < cutoff])

    di_proj <- di_mat[rownames(di_mat) == proj, ]
    di_min_score <- di_proj[di_proj < cutoff]
    di_best_s <- names(di_proj[di_proj < cutoff])

    ldt <- length(dt_best_s)
    lsk <- length(sk_best_s)
    lco <- length(co_best_s)
    ldi <- length(di_best_s)
    match <- data.frame('Metrics' = c(rep('Works with relevant data types',
                                          ifelse(ldt == 0,1,ldt)),
                                      rep('Has desired skills',
                                          ifelse(lsk == 0,1,lsk)),
                                      rep('Performance in relevant courses',
                                          ifelse(lco == 0,1,lco)),
                                      rep('Disciplinary overlap',
                                          ifelse(ldi == 0,1,ldi))),
                        'Top-students' = c(if(length(dt_best_s) == 0){NA} else{dt_best_s},
                                           if(length(sk_best_s) == 0){NA} else{sk_best_s},
                                           if(length(co_best_s) == 0){NA} else{co_best_s},
                                           if(length(di_best_s) == 0){NA} else{di_best_s}),
                        'Scores' = c(if(length(dt_min_score) == 0){NA} else{
                                            dt_min_score},
                                     if(length(sk_min_score) == 0){NA} else{
                                            sk_min_score},
                                     if(length(co_min_score) == 0){NA} else{
                                            co_min_score},
                                     if(length(di_min_score) == 0){NA} else{
                                            di_min_score}))
    proj_matches[[i]] <- match
  }
  return(proj_matches)
}




