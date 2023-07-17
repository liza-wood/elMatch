UCDdept <- read.csv('data_raw/UCDdepts.csv')
UCDdept <- UCDdept[-stringr::str_which(UCDdept$Subject.Description, '^\\*'),]
usethis::use_data(UCDdept)
