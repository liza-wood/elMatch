#' Identify top students for project
#'
#' @return List of projects, each of which contains a data frame with top matched students by four criteria and their scores, where scores closer to zero are better
#'
#' @param r_mat A matrix of research project needs
#' @param s_mat A matrix of student interest
#'
#' @examples proj_list <- identify_top_matches(r_mat, s_mat)
#'
#' @import stringr
#' @import dplyr
#' @import tidyr
#'
#' @export

identify_top_matches <- function(r_mat, s_mat){
  dt_mat <- get_match_score(r_mat, s_mat, attr = 'datatype')
  sk_mat <- get_match_score(r_mat, s_mat, attr = 'skill')
  co_mat <- get_match_score(r_mat, s_mat, attr = 'course')
  di_mat <- get_match_score(r_mat, s_mat, attr = 'discipline')

  proj_matches <- list()
  for(i in 1:nrow(dt_mat)){
    proj = rownames(dt_mat)[i]
    dt_proj <- dt_mat[rownames(dt_mat) == proj, ]
    #dt_min <- dplyr::slice_min(dt_proj, 5)
    dt_min_score <- dt_proj[dt_proj < 2]
    dt_best_s <- names(dt_proj[dt_proj < 2])

    sk_proj <- sk_mat[rownames(sk_mat) == proj, ]
    #sk_min <- round(min(sk_proj),2)
    #sk_best_s <- names(sk_proj[sk_proj == sk_min])
    sk_min_score <- sk_proj[sk_proj < 2]
    sk_best_s <- names(sk_proj[sk_proj < 2])

    co_proj <- co_mat[rownames(co_mat) == proj, ]
    #co_min <- round(min(co_proj), 2)
    #co_best_s <- names(co_proj[co_proj == co_min])
    co_min_score <- co_proj[co_proj < 2]
    co_best_s <- names(co_proj[co_proj < 2])

    di_proj <- di_mat[rownames(di_mat) == proj, ]
    #di_min <- round(min(di_proj), 2)
    #di_best_s <- names(di_proj[di_proj == di_min])
    di_min_score <- di_proj[di_proj < 2]
    di_best_s <- names(di_proj[di_proj < 2])

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
                # I HAVE AN ERROR HERE THAT I CANT FIGURE OUT ##
                        'Top-students' = c(ifelse(length(dt_best_s) == 0, NA,
                                                  dt_best_s),
                                           ifelse(length(sk_best_s) == 0, NA,
                                                  sk_best_s),
                                           ifelse(length(co_best_s) == 0, NA,
                                                  co_best_s),
                                           ifelse(length(di_best_s) == 0, NA,
                                                  di_best_s)),
                        'Scores' = c(ifelse(length(dt_min_score) == 0, NA,
                                            rep(dt_min_score, ldt)),
                                     ifelse(length(sk_min_score) == 0, NA,
                                            rep(sk_min_score, lsk)),
                                     ifelse(length(co_min_score) == 0, NA,
                                            rep(co_min_score, lco)),
                                     ifelse(length(di_min_score) == 0, NA,
                                            rep(di_min_score, ldi))))
    proj_matches[[i]] <- match
  }
  return(proj_matches)
}




