#### CODE FOR VALUE LABELS FROM CENTIMENT

###NOTE: WHAT TO DO ABOUT THE RESPONDANT WHO KEYED IN "BROTHER" FOR Q57: WHAT IS YOUR GENDER?


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

print(unique(data$q59))

data <- data |> 
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
                            TRUE ~ "Man"))#key in brother

race_check <- data |>
  select(q58_1, q58_2, q58_3, q58_4, q58_5, q58_6, q58_7, race_ethn)

###ADDITIONAL XTABS - urbanicity, live where you work, income

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

#le_type <- ttabs(data, q4)


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


auto <- function (data, v1) {

  age <- crosstabs(data, {{v1}}, age) %>%
    mutate(domain = "age") %>%
    arrange(domain)

  gender <- crosstabs(data, {{v1}}, gender) %>%
    mutate(domain = "gender")


  role <- crosstabs(data, {{v1}}, role) |> 
    mutate(domain = "role") 
  
  agency <- crosstabs(data, {{v1}}, agency) %>%
    mutate(domain = "agency")

  tenure <- crosstabs(data, {{v1}}, tenure) %>%
    mutate(domain = "tenure")

  race <- crosstabs(data, {{v1}}, race_ethn) %>%
    mutate(domain = "race") 

  colnames(age) <- c("answer", "subcategory", "n_cases", "total", "pct", "domain")
  colnames(gender) <- c("answer", "subcategory", "n_cases", "total", "pct", "domain")
  colnames(agency) <- c("answer", "subcategory", "n_cases", "total", "pct", "domain")
  colnames(role) <- c("answer", "subcategory", "n_cases", "total", "pct", "domain")
  colnames(tenure) <- c("answer", "subcategory", "n_cases", "total", "pct", "domain")
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

  df <- na.omit(rbind(total, race, gender, role, agency, tenure, age)) %>%
    select(domain, subcategory, answer, pct) %>%
    mutate(pct = round(pct * 100)) %>%
    arrange(domain, subcategory)

  return(df)

}

q52 <- auto(data, q52)

agree_disagree_colors <- c(
  "Strongly agree"      = "#08306B",  # dark blue
  "Somewhat agree"      = "#6BAED6",  # light blue
  "Somewhat disagree"   = "#FDBE85",  # light orange
  "Strongly disagree"   = "#D94801"   # dark orange
)

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
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none")
  
  agree_levels_present <- intersect(
    names(agree_disagree_colors),
    unique(q$answer)
  )
  
  if (length(agree_levels_present) > 0) {
    plot <- plot +
      scale_fill_manual(
        values = agree_disagree_colors,
        breaks = agree_levels_present
      )
  }
  
#  write.csv(q, file = file.path("output", paste0(var2, ".csv")), row.names = F)
  
  print(plot)
  
  print(q)
  
}

export(data, q31_r1)


