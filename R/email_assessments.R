#' Pull student index
#'
#' @return A data frame of all submitted student applications, iterating each time to account for who has been sent assessment emails
#'
#' @param project_url A URL to the Google Sheet where student interest survey responses are stored
#' @examples s_mat <- get_student_index(student_url)
#'
#' @import gmailr
#' @import dplyr
#' @import stringr
#'
#' @export
#'

email_assessments <- function(sender){

  # Make a data frame of just the skills
  s_mat <- pull_student_interest(student_url)
  sk_cols <- which(stringr::str_detect(colnames(s_mat), 'skills'))
  skills <- s_mat[,sk_cols]
  skill_words <- stringr::str_extract(colnames(skills), '(?<=skills: ).*')
  skills <- data.frame(skills)
  colnames(skills) <- skill_words
  skills$id <- rownames(s_mat)

  # Pull in existing index: note that this will update, and divide by sent and unsent
  s_index <- get_student_index(student_url)
  sent <- s_index[!is.na(s_index$Sent_assessment),]
  unsent <- s_index[is.na(s_index$Sent_assessment),]
  unsent <- dplyr::left_join(unsent, skills)
  sk_cols <- which(stringr::str_detect(colnames(unsent), paste(skill_words, collapse = "|")))


  # Use gmailr to send emails
  # Make sure to set up authentification, the steps here were great
  #https://gmailr.r-lib.org/dev/articles/oauth-client.html
  # Download json into gmailapi_oauth_json
  path_to_json <- list.files('gmailapi_oauth_json', full.names = T)
  gm_auth_configure(path = path_to_json)

  for(i in seq_len(nrow(unsent))){
    for(j in sk_cols){
      skill <- skill_words[j]
      val <- unsent[i, j]
      if(nrow(val) == 0){
        print("All students have been emailed their assessments already.")
      } else if(val > 1){
        text_msg <- gm_mime() %>%
          gm_to(unsent$Email[i]) %>%
          gm_from(sender) %>%
          gm_subject(paste("Please take the", skill, "survey")) %>%
          gm_text_body(paste("Thanks for completing the UC Davis DataLab's Experiential Learning interest form. In order to assess your skill areas, please answer the following questions on", skill, "at this link: liza-wood.github.io \n All the best,\n DataLab Team"))
        gm_send_message(text_msg)
        print("Emails have been sent!")
      } else {
        next
      }
    }
    unsent[i, 'Sent_assessment'] <- T
  }

  unsent <- unsent[ ,c('id', 'Email', 'Time', 'Sent_assessment')]
  s_index <- rbind(sent, unsent)
  # Write again to update
  usethis::use_data(s_index, overwrite = T)

}
