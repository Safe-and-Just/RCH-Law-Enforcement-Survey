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

# setwd("~/Documents/GitHub/RCH-Law-Enforcement-Survey")

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
  filter(!grepl("aide", q5_other_key_in)) |>    # remove probation aide from sample
  mutate(
         tenure = case_when(q3 %in% 0:4 ~ "0-4 years",
                            q3 %in% 5:9 ~ "5-9 years",
                            q3 %in% 10:39 ~ "10 or more years",
                            q3 == NA ~ NA),
         agency = case_when(q4 == "Sheriff’s Office or Sheriff’s Department" ~ "Sheriff",
                            q4 == "Department of Corrections" ~ "Corrections/Supervision",
                            q4 == "Local law enforcement agency (e.g. municipal police department," ~ "Local",
                            q4 == "State Police or State Highway Patrol" ~ "State",
                            q4 == "Federal Law Enforcement Agency (e.g., FBI, DEA, ATF, U.S. Marsha" ~ "Federal",
                            q4 == "Department of Homeland Security Agency (e.g., CBP, ICE, USSS, TS" ~ "Federal",
                            q4 == "Other law enforcement agency (University Police, Harbor Police," ~ "Other",
                            q4 == "Transit Police Department or Airport Police Department" ~ "Other",
                            q4 == "Probation or Parole Agency" ~ "Corrections/Supervision"),
         role = case_when(q5 == "Investigator/Detective" ~ q5,
                          q5 == "Custodial Officer/Deputy in a jail or detention center" ~ "Custodial Officer", 
                          q5 == "Supervisory role (e.g., Sergeant, Lieutenant, Captain, Major, As" ~ "Supervisory role",
                          q5_other_key_in %in% c("Correction officer", "Correctional officer", "Corrections officer", "Inside")  ~ "Custodial Officer", 
                          q5_other_key_in %in% c("Probation and Parole officer", "Probation officer", "Probation officer aide", "Probation Officer") ~ "Probation Officer",
                          q5 == "Patrol or Field Officer/Deputy" & q4 != "Probation or Parole Agency" ~ "Patrol or Field Officer/Deputy",
                          q5 == "Patrol or Field Officer/Deputy" & q4 == "Probation or Parole Agency" ~ "Probation Officer",
                          TRUE ~ "Other"),
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
         gender = case_when(q57 == "Woman" ~ "Woman",
                            q57 == "Man" ~ "Man",
                            TRUE ~ "Man"), #key in brother
         proximity = case_when(q55 == "Yes, I live in the same neighborhood." ~ "Neighborhood",
                              q55 == "Yes, I live in the same city." ~ "City or metro area",
                              q55 == "Yes, I live in the same metro area." ~ "City or metro area",
                              TRUE ~ "Outside metro area"))

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

  role <- crosstabs(data, {{v1}}, role) %>%
    mutate(domain = "role")

  agency <- crosstabs(data, {{v1}}, agency) %>%
    mutate(domain = "agency")

  proximity <- crosstabs(data, {{v1}}, proximity) %>%
    mutate(domain = "proximity")

  race <- crosstabs(data, {{v1}}, race_ethn) %>%
    mutate(domain = "race") 

  colnames(age) <- c("answer", "subcategory", "n_cases", "total", "pct", "domain")
  colnames(gender) <- c("answer", "subcategory", "n_cases", "total", "pct", "domain")
  colnames(role) <- c("answer", "subcategory", "n_cases", "total", "pct", "domain")
  colnames(race) <- c("answer", "subcategory", "n_cases", "total", "pct", "domain")
  colnames(agency) <- c("answer", "subcategory", "n_cases", "total", "pct", "domain")
  colnames(proximity) <- c("answer", "subcategory", "n_cases", "total", "pct", "domain")
  
  total <- nrow(data)

  total <- data %>%
    group_by(as_factor({{v1}})) %>%
    summarise(n_cases = n()) %>%
    mutate(total = total,
           pct = (n_cases/total),
           subcategory = "ALL",
           domain = "ALL")


  colnames(total) <- c("answer",  "n_cases", "total", "pct", "subcategory", "domain")

  df <- na.omit(rbind(total, race, gender, role, proximity, age, agency)) %>%
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
  
 # write.csv(q, file = file.path("output", paste0(var2, ".csv")), row.names = F)
  
  print(plot)
  
  print(q)
  
}

##Qs where proximity matters
q51 <- export(data, q51)
q48 <- export(data, q48)
q47 <- export(data, q47)

######## TABLES AND DATA VIZ FOR PROGRAM SUPPORT BY WORKED IN THOSE PROGRAMS

support_vars <- c("q35_r1", "q36_r2", "q37_r3", "q38_r4", "q39_r5", "q40_r6")
worked_vars <- c("q42_6", "q42_1", "q42_2", "q42_3", "q42_4", "q42_5")

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
  support = c("q35_r1", "q36_r2", "q37_r3", "q38_r4", "q39_r5", "q40_r6"),
  worked  = c("q42_6", "q42_4",  "q42_3",  "q42_2",  "q42_1",  "q42_5")
)
  
program_crosstabs <- lapply(seq_len(nrow(pairs)), function(i) {
  tab <- table(
    worked  = programs[[pairs$worked[i]]],
    support = programs[[pairs$support[i]]]
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
    count(worked = .data[[.y]], support = .data[[.x]]) %>%
    group_by(worked) %>%
    mutate(percent = n / sum(n)) %>%
    ungroup() %>%
    mutate(program = .x)
)

programs_summary_table <- programs_summary_table |>
  mutate(program = case_when(
    program == "q36_r2" ~ "welfare checks",
    program == "q37_r3" ~ "sobering centers",
    program == "q38_r4" ~ "traffic collisians",
    program == "q39_r5" ~ "de-escalation",
    program == "q40_r6" ~ "cvi",
    program == "q35_r1" ~ "online reporting"
  ))

program_ci <- programs_summary_table |>
  filter(support == "Agree") |>
  mutate(
    worked = factor(worked, labels = c("Did not work", "Worked")),
    se = sqrt(percent * (1 - percent) / n),
    ci_low = percent - 1.96 * se,
    ci_high = percent + 1.96 * se
  )

p1 <- programs_summary_table |>
  mutate(
    worked = factor(worked, labels = c("Did not work", "Worked")) ) |>
  filter(support == "Agree") |>
  ggplot(aes(x = worked, y = percent, fill = worked)) +
  geom_col() +
  facet_wrap(~ program) +
  labs(
    x = NULL,
    y = "Percent who agree"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

cvi_plot <- programs_summary_table |>
  filter(program == "cvi",
         support == "Agree") |>
  mutate(
    worked  = factor(worked, labels = c("Did not work", "Worked")),
    support = factor(support, levels = c("Agree", "Disagree"))
  ) |>
  ggplot(aes(x = support, y = percent, fill = worked)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(
    aes(label = paste0(round(percent*100), "%")),
    position = position_dodge(width = 0.8),
    vjust = -0.75,
    size = 3,
    fontface = "bold") +
  labs(
    title = "cvi",
    x = NULL,
    y = "Percent of respondents",
    fill = "Worked in program"
   )+
  theme_minimal() +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text = element_text(face = "plain", hjust = 0),
    axis.text.x = element_text(hjust = 0.5),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

cvi_plot

#####ODDS RATIOS BETWEEN WORKED GROUPS
or_df <- programs_summary_table |>
  filter(support %in% c("Agree", "Disagree")) |>
  group_by(program, worked, support) |>
  summarise(n = sum(n), .groups = "drop") |>
  pivot_wider(
    names_from = c(worked, support),
    values_from = n,
    names_sep = "_"
  )

or_df <- or_df |>
  mutate(
    or = (`1_Agree` / `1_Disagree`) / (`0_Agree` / `0_Disagree`)
  )

or_df <- or_df |>
  mutate(
    or_label = round(or, 2),
    direction = ifelse(or > 1, "Above 1", "Below 1")
  )

p4 <- ggplot(or_df, aes(x = reorder(program, or), y = or)) +
  geom_segment(aes(xend = program, y = 1, yend = or), color = "gray70") +
  geom_point(size = 4, color = "#003972") +
  geom_text(aes(label = paste0((or_label), "x"), hjust = ifelse(or > 1, -0.5, 1.5)), size = 3.5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#f47d20") +
  scale_y_log10() +
  coord_flip() +  
  labs(
    x = NULL,
    title = "Odds Ratios by Program"
  ) +
  theme_minimal()
p4

p6 <- ggplot(or_df, aes(x = reorder(program, or), y = or)) +
  geom_segment(aes(xend = program, y = 1, yend = or), color = "gray80") +
  geom_point(aes(color = direction), size = 5) +
  geom_text(aes(label = paste0((or_label), "x"), hjust = ifelse(or > 1, -0.5, 1.5)), size = 3.5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_y_log10() +
  coord_flip() +  
  scale_color_manual(values = c("Above 1" = "#003972", "Below 1" = "#f47d20")) +
  labs(
    x = NULL,
    title = "Odds Ratios by Program",
    color = "Direction"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
p6


####### BENEFITS OF MENTAL HEALTH RESPONDERS




