
stringr
googlesheets4
tidyr
pull_research_needs <- function(){
  r <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1LJ60ETr4mXtSLgXthdPG6gvTEL26LodV2icssdhC3Bc/edit?resourcekey#gid=407218839')

  r <- separate(r, 2, into = paste0('data_type', 1:8), sep = ',')

  skills_cols <- which(str_detect(colnames(r), 'skills'))
  colnames(r)[skills_cols] <- str_extract(colnames(r)[skills_cols], '(?<=\\[).*(?=\\])')

  r <- pivot_longer(r, cols = str_which(colnames(r), 'data_type'),
                names_to = 'number', values_to = 'data_type')
  r <- r[!is.na(r$data_type),]

}


