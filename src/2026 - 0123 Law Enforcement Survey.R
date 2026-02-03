#### CODE FOR VALUE LABELS FROM CENTIMENT

#### SAMPLE SIZES TOO SMALL FOR AGENCY

rm(list = ls())

library(haven)
library(labelled)
library(tidyverse)
library(tidylog)
library(janitor)
library(stringi)
library(data.table)
library(ggplot2)
library(scales)
library(purrr)

setwd("~/Documents/GitHub/RCH-Law-Enforcement-Survey")

options(dplyr.print_max = 1e9)

data <- read_sav("input/Law-enforcement-survey-character.sav")

data <- clean_names(data)

questions <- sapply(data, attr, "label")
questions <- as.data.frame(questions)
questions <- cbind(names(data), questions)
questions <- questions[1:2]
colnames(questions) <- c("variable", "question")
write.csv(questions, "output/questions as a list.csv")

#print(unique(data$q59))

data <- data %>%
  mutate(
         tenure = case_when(q3 %in% 0:4 ~ "0-4 years",
                            q3 %in% 5:9 ~ "5-9 years",
                            q3 %in% 10:39 ~ "10 or more years",
                            q3 == NA ~ NA),
         agency = case_when(q4 == "Sheriff’s Office or Sheriff’s Department" ~ "Sheriff",
                            q4 == "Department of Corrections" ~ "DOC, Supervision",
                            q4 == "Local law enforcement agency (e.g. municipal police department," ~ "Local",
                            q4 == "State Police or State Highway Patrol" ~ "State",
                            q4 == "Federal Law Enforcement Agency (e.g., FBI, DEA, ATF, U.S. Marsha" ~ "Federal",
                            q4 == "Department of Homeland Security Agency (e.g., CBP, ICE, USSS, TS" ~ "Federal",
                            q4 == "Other law enforcement agency (University Police, Harbor Police," ~ "Other",
                            q4 == "Transit Police Department or Airport Police Department" ~ "Other",
                            q4 == "Probation or Parole Agency" ~ "DOC, Supervision"),
         race_ethn = case_when(q58_4 != "" ~ q58_4,
                          q58_3 != "" ~ q58_3,
                          q58_7 != "" ~ q58_7,
                          TRUE ~ "Other"),
         age = case_when(q59 == "18-24" ~ "18-34",
                         q59 == "25-34" ~ "18-34",
                         q59 == "35-44" ~ "35-44",
                         q59 == "45-54" ~ "45-54",
                         q59 == "55-65" ~ "55+",
                         q59 == "65+" ~ "55+"), 
         # urban = case_when(zipdense2 == 1 ~ "Urban",
         #                   zipdense2 == 2 ~ "Suburban",
         #                   zipdense2 == 3 ~ "Rural"),
         # party = case_when(partyid == 1 ~ "Democrat",
         #                   partyid == 2 ~ "Republican",
         #                   partyid %in% 3:4 ~ "Other Party/No Party"), #combined bc n for "Other Party" was too small
         gender = case_when(q57 == "Woman" ~ "Woman",
                            q57 == "Man" ~ "Man",
                            TRUE ~ "Man"))#key in brother

unique(data$q55)

race_check <- data |>
  select(q58_1, q58_2, q58_3, q58_4, q58_5, q58_6, q58_7, race_ethn)

table(data$race_ethn)
###ADDITIONAL XTABS - role, urbanicity, live where you work, income

# Define main sample

#### function

tabs <- function(data, v1) {
  total <- nrow(data) 
  
  
  df <- data %>%
    group_by(as_factor({{v1}})) %>%
    summarise(n_cases = n()) |>
    mutate(pct = n_cases/total
    )
  
  return(df)
  
}

#le_type <- tabs(data, q4)

crosstabs <- function (data, v1, v2) {
  
  total_df <- data %>%
    group_by({{v2}}) %>%
    summarise(total = n())
  
  df <- data %>%
    group_by({{v1}}, {{v2}}) %>%
    summarise(n_cases = n()) %>%
    left_join(total_df) %>%
    mutate(pct = n_cases/total)
  
  return(df)
  
}

#test <- crosstabs(data, q46, q57)

auto <- function (data, v1) {

  age <- crosstabs(data, {{v1}}, age) %>%
    mutate(domain = "age") %>%
    arrange(domain)

  gender <- crosstabs(data, {{v1}}, gender) %>%
    mutate(domain = "gender")

  agency <- crosstabs(data, {{v1}}, agency) %>%
    mutate(domain = "agency")

  proximity <- crosstabs(data, {{v1}}, q55) %>%
    mutate(domain = "proximity")

  race <- crosstabs(data, {{v1}}, race_ethn) %>%
    mutate(domain = "race") 

  colnames(age) <- c("answer", "subcategory", "n_cases", "total", "pct", "domain")
  colnames(gender) <- c("answer", "subcategory", "n_cases", "total", "pct", "domain")
  colnames(agency) <- c("answer", "subcategory", "n_cases", "total", "pct", "domain")
  colnames(proximity) <- c("answer", "subcategory", "n_cases", "total", "pct", "domain")
  colnames(race) <- c("answer", "subcategory", "n_cases", "total", "pct", "domain")

  total <- nrow(data)

  total <- data %>%
    group_by(as_factor({{v1}})) %>%
    summarise(n_cases = n()) %>%
    mutate(total = total,
           pct = (n_cases/total),
           subcategory = "ALL",
           domain = "ALL")


  colnames(total) <- c("answer",  "n_cases", "total", "pct", "subcategory", "domain")

  df <- na.omit(rbind(total, race, gender, agency, proximity, age)) %>%
    select(domain, subcategory, answer, pct) %>%
    mutate(pct = round(pct * 100)) %>%
    arrange(domain, subcategory)



  return(df)

}

q52 <- auto(data, q52)
q51 <- auto(data, q51)
q50 <- auto(data, q50)
q50 <- auto(data, q50)
q17 <- auto(data, q17_r1)

export <- function(data, v1) {
  
  q <- auto(data, {{v1}}) 
  
  var <- as.character(substitute(v1))
  var2 <- deparse(substitute(v1))
  title <- questions[questions$variable == var, ]
  
  plot <- 
    ggplot(q, aes(x = subcategory, y = pct, fill = answer)) +
    geom_col(position = "dodge") +
    facet_wrap(~ domain, scales = "free_x") +
    labs(title = (str_wrap(title$question)),
         x = "Subcategory",
         y = "Percentage",
         fill = "Response") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  write.csv(q, file = file.path("output", paste0(var2, ".csv")), row.names = F)
  
  print(plot)
  
  print(q)
  
}

##Qs where proximity matters
q51 <- export(data, q51)
q27 <- export(data, q47)
q48 <- export(data, q48)
q47 <- export(data, q47)



plot <- 
  ggplot(q, aes(x = subcategory, y = pct, fill = answer)) +
  geom_col(position = "dodge") +
  facet_wrap(~ domain, scales = "free_x") +
  labs(title = (str_wrap(title$question)),
       x = "Subcategory",
       y = "Percentage",
       fill = "Response") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


mh_response <- data |>
  group_by()

support_vars <- c("q36_r2", "q37_r3", "q38_r4", "q39_r5", "q40_r6")
worked_vars <- c("q42_1", "q42_2", "q42_3", "q42_4", "q42_5")

programs <- data |>
  mutate(across(all_of(support_vars),
                ~case_when(
                  . %in% c("Strongly agree", "Somewhat agree") ~ "Agree",
                  . %in% c("Strongly disagree", "Somewhat disagree") ~ "Disagree",
                  TRUE ~ NA_character_
                ))) |>
  mutate(across(all_of(worked_vars),
                ~ ifelse(. == "" | is.na(.), 0, 1)))

pairs <- data.frame(
  #program = c("welfare_checks", "sobering_centers", "collisions", "deescalation", "cvi"),
  support = c("q36_r2", "q37_r3", "q38_r4", "q39_r5", "q40_r6"),
  worked  = c("q42_4",  "q42_3",  "q42_2",  "q42_1",  "q42_5")
)
  
program_crosstabs <- lapply(seq_len(nrow(pairs)), function(i) {
  tab <- table(
    Worked  = programs[[pairs$worked[i]]],
    Support = programs[[pairs$support[i]]]
  )
  list(
    program = pairs$support[i],
    table = tab,
    row_percents = prop.table(tab, 1)
  )
})

programs_summary_table <- map2_dfr(
  pairs$support,
  pairs$worked,
  ~ programs %>%
    count(Worked = .data[[.y]], Support = .data[[.x]]) %>%
    group_by(Worked) %>%
    mutate(percent = n / sum(n)) %>%
    ungroup() %>%
    mutate(program = .x)
)

programs_summary_table <- programs_summary_table |>
  mutate(case_when(
    program == "q36_r2" ~ "welfare checks",
    program == "q37_r3" ~ "sobering centers",
    program == "q38_r4" ~ "traffic collisians",
    program == "q39_r5" ~ "de-escalation",
    program == "q40_r6" ~ "cvi"
  ))

program_summary <- map2_dfr(
  pairs$support,
  pairs$worked,
  ~ programs %>%
    count(
      worked  = .data[[.y]],
      support = .data[[.x]]
    ) %>%
    group_by(worked) %>%
    mutate(
      percent = n / sum(n),
      program = .x
    ) %>%
    ungroup()
)

program_crosstabs[[1]]$table

program_summary <- data |>
  q <- auto(data, {{v1}}) |>
  mutate(stacked_answer = case_when(answer %in% c("Strongly Agree", "Somewhat Agree") ~ "Agree",
                                    answer %in% c("Strongly Disagree", "Somewhat Disagree") ~ "Disagree",
                                    answer == "Don't Know" ~ "Don't Know")) |>
  group_by(domain, subcategory, stacked_answer) |>
  summarise(pct = sum(pct))

q37_r3
q38_r4
q39_r5
q40_r6

