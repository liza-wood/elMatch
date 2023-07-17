UCDdept <- read.csv('data_raw/UCDdepts.csv')
UCDdept <- UCDdept[-stringr::str_which(UCDdept$Subject.Description, '^\\*'),]
UCDept <- UCDdept[UCDdept$Subject.Description != 'Animal Biology Graduate',]
usethis::use_data(UCDdept)
