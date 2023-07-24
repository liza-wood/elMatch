# `elMatch`: A workflow for matching research projects and students for Experiential Learning opportunities  

This package matches proposed research projects with prospective student researchers based on four match criteria:  

1. Experience and interest with various **programming skills**   
2. Experience and interest working with different **data types**    
3. **Coursework** (experience and performance) that matches relevant project coursework  
4. **Disciplinary overlap** with the students' coursework and project sub-disciplines  

The make sequence is below. The result is a list of research projects, each with a data frame of top student matches based on the four criteria listed above. In addition to student names, the students' match score is listed for each area, where higher scores (3) suggest a great match and lower scores (0) suggest no match. A cutoff value can be selected to limit or widen the list of student matches. Details about the project and the students can be pulled with the get index functions.  

```
student_url <- 'https://docs.google.com/spreadsheets/d/1g6y2zkolHAOrtppdSThsBkRsl3RCVa7_Nve_NEOQARc/edit?usp=sharing'
project_url <- 'https://docs.google.com/spreadsheets/d/1oJd2JNKZ7CLTpwexepS7Ly1f19oLchWji6yRxz7X3X8/edit?resourcekey#gid=1824769055'

p_mat <- pull_research_needs(project_url, student_url)
s_mat <- pull_student_interest(student_url)

proj_list <- identify_top_matches(p_mat, s_mat, cutoff = 1)

p_index <- get_project_index(project_url)
s_index <- get_student_index(student_url)
```

Students also take an assessment based on 