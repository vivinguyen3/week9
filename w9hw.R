library(tidyverse)
library(readr)
library(janitor)

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

#Question 2
avg_experience <- students %>%
  group_by(group_number_name) %>%
  summarise(avg_experience = mean(programming_languages_r_python_julia_etc, na.rm = TRUE))

avg_experience

#Question 3
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

#Question 4
experience_by_level <- students %>%
  group_by(year_of_study) %>%
  summarise(avg_experience = mean(programming_languages_r_python_julia_etc, na.rm = TRUE))

experience_by_level