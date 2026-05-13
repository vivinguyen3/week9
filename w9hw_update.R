library(tidyverse)
library(readr)
library(janitor)
library(dplyr)

students <- read_csv("student_info.csv")

glimpse(students)

students <- students %>%
  clean_names()

students <- students %>%
  mutate(
    research_academic_interests = str_to_lower(research_academic_interests),
    programming_languages_r_python_julia_etc = str_to_lower(programming_languages_r_python_julia_etc),
    year_of_study = str_to_lower(year_of_study)
  )

#Question 1
research_dist <- students %>%
  separate_rows(research_academic_interests, sep = ",") %>%
  mutate(research_academic_interests = str_trim(research_academic_interests)) %>%
  count(research_academic_interests, sort = TRUE)

research_dist

ggplot(research_dist, aes(x = reorder(research_academic_interests, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Research Interest", y = "Count")

#Question 2: avg level of programming experience in each group
avg_experience <- students %>%
  mutate(
    level_of_programming_experience_e_g_beginner_intermediate_advanced =
      str_to_lower(level_of_programming_experience_e_g_beginner_intermediate_advanced),
    
    prog_score = case_when(
      str_detect(level_of_programming_experience_e_g_beginner_intermediate_advanced,
                 "beginner") ~ 1,
      
      str_detect(level_of_programming_experience_e_g_beginner_intermediate_advanced,
                 "intermediate") ~ 2,
      
      str_detect(level_of_programming_experience_e_g_beginner_intermediate_advanced,
                 "advanced") ~ 3,
      
      TRUE ~ NA_real_
    )
  ) %>%
  group_by(group_number_name) %>%
  summarise(
    avg_experience = mean(prog_score, na.rm = TRUE),
    .groups = "drop"
  )
avg_experience

#N/a appears because n/a was typed in the excel sheet and not a valid number

#Question 3: how many people in each group know each program
students_lang <- students %>%
  mutate(
    knows_r = str_detect(programming_languages_r_python_julia_etc, "\\br\\b"),
    knows_python = str_detect(programming_languages_r_python_julia_etc, "python"),
    knows_sql = str_detect(programming_languages_r_python_julia_etc, "sql")
  )

lang_counts <- students_lang %>%
  group_by(group_number_name) %>%
  summarise(
    r_users = sum(knows_r, na.rm = TRUE),
    python_users = sum(knows_python, na.rm = TRUE),
    sql_users = sum(knows_sql, na.rm = TRUE)
  )

lang_counts

#n/a appears because it was typed in 'group number' in the survey

#Question 4: do undergrads or grad students have more or less experience
experience_by_level <- students %>%
  mutate(
    prog_score = case_when(
      str_detect(
        level_of_programming_experience_e_g_beginner_intermediate_advanced,
        "beginner"
      ) ~ 1,
      
      str_detect(
        level_of_programming_experience_e_g_beginner_intermediate_advanced,
        "intermediate"
      ) ~ 2,
      
      str_detect(
        level_of_programming_experience_e_g_beginner_intermediate_advanced,
        "advanced"
      ) ~ 3,
      
      TRUE ~ NA_real_
    )
  ) %>%
  group_by(year_of_study) %>%
  summarise(
    avg_experience = mean(prog_score, na.rm = TRUE),
    .groups = "drop"
  )

experience_by_level

#Na because year of study and programming experience was blank in the spreadsheet