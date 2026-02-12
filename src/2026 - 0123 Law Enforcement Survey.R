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
library(rlang)
library(forcats)

# setwd("~/Documents/GitHub/RCH-Law-Enforcement-Survey")

options(dplyr.print_max = 1e9)

data <- read_sav("input/Law-enforcement-survey-character.sav")

data <- clean_names(data)

names(data) <- gsub("_r[0-9]+$", "", names(data))

questions <- sapply(data, attr, "label")
questions <- as.data.frame(questions)
questions <- cbind(names(data), questions)
questions <- questions[1:2]
colnames(questions) <- c("variable", "question")


questions <- questions %>%
  mutate(
    question = sub("^.* \\| ", "", question),
    question = case_when(variable == "q32" ~ "Individuals experiencing mental health crises often need interventions or services beyond what my department is equipped to provide.",
                         variable == "q33" ~ "Individuals experiencing drug overdoses often need interventions or treatment beyond what my department is equipped to provide.",
                         variable == "q34" ~ "Individuals experiencing homelessness often need interventions or services other than what my department is equipped to provide.",
                         TRUE ~ question)
  )

question_lookup <- setNames(
  questions$question,
  questions$variable
)

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
         role = case_when(q5 == "Investigators/Detectives" ~ q5,
                          q4 == "Probation or Parole Agency" | grepl("robation", q5_other_key_in) ~ "Probation Officers",
                          q5 == "Custodial Officer/Deputy in a jail or detention center" ~ "Custodial Officers", 
                          q5 == "Supervisory role (e.g., Sergeant, Lieutenant, Captain, Major, As" ~ "Supervisors",
                          q5_other_key_in %in% c("Correction officer", "Correctional officer", "Corrections officer", "Inside")  ~ "Custodial Officers", 
                          q5_other_key_in %in% c("Probation and Parole officer", "Probation officer", "Probation officer aide", "Probation Officer") ~ "Probation Officers",
                          q5 == "Patrol or Field Officer/Deputy" & q4 != "Probation or Parole Agency" ~ "Patrol or Field Officers",
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
         gender = case_when(q57 == "Women" ~ "Women",
                            q57 == "Men" ~ "Men",
                            TRUE ~ "Men"), #key in brother
         proximity = case_when(q55 == "Yes, I live in the same neighborhood." ~ "Neighborhood",
                              q55 == "Yes, I live in the same city." ~ "City or metro area",
                              q55 == "Yes, I live in the same metro area." ~ "City or metro area",
                              TRUE ~ "Outside metro area"))

table(data$role)

race_check <- data |>
  select(q58_1, q58_2, q58_3, q58_4, q58_5, q58_6, q58_7, race_ethn)

table(data$race_ethn)
###ADDITIONAL XTABS - role, urbanicity, live where you work, income

# Define main sample

#### function

tabs <- function(data, v1) {
  total <- nrow(data) 
  
  
  df <- data %>%
    group_by(answer = as_factor({{v1}})) %>%
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


agree_stack_levels <- c(
  "Strongly agree",
  "Somewhat agree",
  "Somewhat disagree",
  "Strongly disagree"
)

agree_disagree_colors <- c(
  "Strongly agree"      = "#08306B",  # dark blue
  "Somewhat agree"      = "#6BAED6",  # light blue
  "Somewhat disagree"   = "#FDBE85",  # light orange
  "Strongly disagree"   = "#D94801"   # dark orange
)

asj_colors <- function(...) {
  scale_color_manual(
    values =  "#08306B", "#FDBE85", "#6BAED6", "#D94801",
    drop = FALSE,
    ...
  )
}

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
  
 # write.csv(q, file = file.path("output", paste0(var2, ".csv")), row.names = F)
  
  print(plot)
  
  print(q)
  
}

##Qs where proximity matters
q51 <- export(data, q51)
q48 <- export(data, q48)
q47 <- export(data, q47)

### Functions for agree/disagree stacked charts

agree <- function(data, v1) {
  
  q <- auto(data, {{v1}}) 
  
  
  q <- q %>%
    mutate(
      answer = factor(answer, levels = agree_stack_levels),
      agree_group = case_when(
            answer %in% c("Strongly agree", "Somewhat agree") ~ "Agree",
            answer %in% c("Strongly disagree", "Somewhat disagree") ~ "Disagree",
            TRUE ~ NA_character_
          ),
          agree_group = factor(agree_group, levels = c("Agree", "Disagree")),
          answer = factor(
            answer,
            levels = c(
              "Somewhat agree",
              "Strongly agree",
              "Somewhat disagree",
              "Strongly disagree"
            )
          )
        )
    
  
  var <- as.character(substitute(v1))
  var2 <- deparse(substitute(v1))
  title <- questions[questions$variable == var, ]
  
  plot_stacked <-
    ggplot(
      q,
      aes(
        x = interaction(agree_group, subcategory),
        y = pct,
        fill = answer
      )
    ) +
    geom_col(width = 0.7) +
    facet_wrap(~ domain, scales = "free_x") +
    labs(
      title = str_wrap(title$question),
      x = NULL,
      y = "Percentage",
      fill = "Response"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
  
  
  agree_levels_present <- intersect(
    names(agree_disagree_colors),
    unique(q$answer)
  )
  
  if (length(agree_levels_present) > 0) {
    plot_stacked <- plot_stacked +
      scale_fill_manual(
        values = agree_disagree_colors,
        breaks = agree_levels_present
      )
  }
  
  plot_stacked <- plot_stacked +
    scale_x_discrete(
      labels = function(x) sub("^.*\\.", "", x)
    )
  # write.csv(q, file = file.path("output", paste0(var2, ".csv")), row.names = F)
  
  print(plot_stacked)
  
  print(q)
  
}


agree_totals <- function(v1) {
  
  q <- tabs(data, {{v1}}) 
  
  colnames(q) <- c("answer", "n_cases", "pct")
  
  q <- q %>%
    mutate(
      pct = round(pct * 100),
      answer = factor(answer, levels = agree_stack_levels),
      agree_group = case_when(
        answer %in% c("Strongly agree", "Somewhat agree") ~ "Agree",
        answer %in% c("Strongly disagree", "Somewhat disagree") ~ "Disagree",
        TRUE ~ NA_character_
      ),
      agree_group = factor(agree_group, levels = c("Disagree", "Agree")),
      answer = factor(
        answer,
        levels = c(
          "Somewhat agree",
          "Strongly agree",
          "Somewhat disagree",
          "Strongly disagree"
        )
      )
    )
  
  label_df <- q %>%
   filter(!is.na(agree_group)) %>%
   group_by(agree_group) %>%
   summarise(pct = sum(pct), .groups = "drop")
  
  var <- as.character(substitute(v1))
  var2 <- deparse(substitute(v1))
  title <- questions[questions$variable == var, ]
  plot_stacked <-
    ggplot(
      q,
      aes(
        x = interaction(agree_group),
        y = pct,
        fill = answer
      )
    ) +
    geom_col(width = 0.7) +
    
    # value labels for Agree / Disagree only
    geom_text(
      data = label_df,
      aes(
        x = agree_group,
        y = pct,
        label = paste0(pct, "%")
      ),
      hjust = -0.15,   # <-- was vjust
      size = 3.5,
      inherit.aes = FALSE
    ) +
    
    coord_flip() +   # <-- flip here
    
    labs(
      title = str_wrap(title$question, width = 60),
      x = NULL,
      y = NULL,
      fill = "Response"
    ) +
    
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +  # space for labels
    
    scale_x_discrete(
      labels = function(x) str_wrap(x, width = 20)   # <-- wrap category labels
    ) +
    
    theme_minimal() +
    theme(
      # remove value axis (now x after flip)
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      
      # keep category labels readable
      axis.text.y = element_text(hjust = 1),
      
      # remove gridlines
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      legend.position = "none"
    )
  
  
  agree_levels_present <- intersect(
    names(agree_disagree_colors),
    unique(q$answer)
  )
  
  if (length(agree_levels_present) > 0) {
    plot_stacked <- plot_stacked +
      scale_fill_manual(
        values = agree_disagree_colors,
        breaks = agree_levels_present
      )
  }
  
  plot_stacked <- plot_stacked +
    scale_x_discrete(
      labels = function(x) sub("^.*\\.", "", x)
    )
  # write.csv(q, file = file.path("output", paste0(var2, ".csv")), row.names = F)
  
  print(plot_stacked)
  print(q)
  return(q)
  
}

agree_totals(q31)
agree_totals(q32)
agree_totals(q33)
agree_totals(q34)


##################################
###### Q35 to Q40 df #############
##################################

q35 <- agree_totals(q35) |>
  mutate(q = "online reporting")
q36 <- agree_totals(q36) |>
  mutate(q = "welfare checks")
q37 <- agree_totals(q37) |>
  mutate(q = "sobering centers")
q38 <- agree_totals(q38) |>
  mutate(q = "traffic collisions")
q39 <- agree_totals(q39) |>
  mutate(q = "mh de-escalation")
q40 <- agree_totals(q40) |>
  mutate(q = "violence interruption")

q35_40 <- rbind(q35, q36, q37, q38, q39, q40)

q35_40p <- q35_40 |>
  mutate(
    agree_group = factor(agree_group, levels = c("Agree", "Disagree")),
    answer = factor(
      answer,
      levels = c(
        "Somewhat agree",
        "Strongly agree",
        "Somewhat disagree",
        "Strongly disagree"
      )
    )
  )

q35_40p_label <- q35_40p |>
  filter(!is.na(agree_group)) |>
  mutate(
    agree_group = factor(agree_group, levels = c("Agree", "Disagree"))
  ) |>
  group_by(q, agree_group) |>
  summarise(pct = sum(pct), .groups = "drop")


##################################
###### Q35 to Q40 plot #############
##################################

p_q35_40 <- ggplot(
  q35_40p,
  aes(
    x = agree_group,
    y = pct,
    fill = answer
  )
) +
  geom_col(width = 0.85) +
  
  geom_text(
    data = q35_40p_label,
    aes(
      x = factor(agree_group, levels = c("Agree", "Disagree")),
      y = pct,
      label = paste0(pct, "%")
    ),
    vjust = -0.5,
    size = 3.5,
    inherit.aes = FALSE
  ) +
  
  facet_wrap(
    ~ str_wrap(q, 40),
    scales = "free_x",
    drop = FALSE
  ) +
  
  scale_fill_manual(values = agree_disagree_colors) +
  
  theme_minimal() +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid   = element_blank()
  )



p_q35_40

##################################
###### BEYOND ####################
##################################

beyond <- rbind(tabs(data, q32) |> 
                  mutate(issue = "mental health crises"),
                tabs(data, q33) |>  
                  mutate(issue = "drug overdoses"),
                tabs(data, q34) |> 
                  mutate(issue = "homelessness"))

beyond <- beyond |> 
  filter(answer %in% c("Strongly agree", "Somewhat agree")) |> 
  group_by(issue) |> 
  summarise(pct = round(sum(pct) * 100))

ggplot(beyond, aes(x = issue, y = pct)) +
  geom_col(fill = "#08306B") +
  # Add labels: hjust = -0.2 puts them just outside the bar
  geom_text(aes(label = paste0(pct, "%"), hjust = -0.2)) + 
  theme_minimal() +
  coord_flip() +                 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
    )

write.csv(beyond, "output/beyond.csv", row.names = F)

#######################################################
###### How do officers spend their time #############
####################################################
timeq <- function(v1) {
  
  experience <- questions[questions$variable == as.character(substitute(v1)), 2]
  
  
  df <- data %>%
    mutate(frequency = case_when({{v1}} == "Never" ~ "never",
                                 {{v1}} == "Often (several times a week)" ~ "weekly",
                                 {{v1}} == "Occasionally (several times a year)" ~ "yearly",
                                 {{v1}} == "Rarely (once every few years)" ~ "rarely",
                                 TRUE ~ NA
    )
    ) |> 
    group_by(
      role, 
      frequency) %>%
    summarise(n_cases = n()) |> 
    pivot_wider(names_from = "frequency",
                values_from = "n_cases") |> 
    mutate(total = rowSums(across(c(never, weekly, yearly, rarely)), na.rm = T),
           Ever = total - never,
           `At least a few times a year` = weekly + yearly,
           `A few times a week` = weekly) |> 
    mutate(across(6:8, ~ round(.x / total * 100))) |> 
    select(role, Ever, `At least a few times a year` ,  `A few times a week`) |> 
    pivot_longer(!role)
  
  total <- data |> 
    mutate(frequency = case_when(grepl("Never", {{v1}}) ~ "never",
                                 grepl("Often", {{v1}}) ~ "weekly",
                                 grepl("Occasionally", {{v1}}) ~ "yearly",
                                 grepl("Rarely", {{v1}}) ~ "rarely",
                                 TRUE ~ NA)
    ) |> 
    group_by(
      frequency) %>%
    summarise(n_cases = n()) |> 
    pivot_wider(names_from = "frequency",
                values_from = "n_cases") |> 
    mutate(total = rowSums(across(c(never, weekly, yearly, rarely)), na.rm = T),
           Ever = total - never,
           `At least a few times a year` = weekly + yearly,
           `A few times a week` = weekly) |> 
    mutate(across(6:8, ~ round(.x / total * 100)),
           role = "All") |> 
    select(role, Ever, `At least a few times a year` ,  `A few times a week`) |> 
    pivot_longer(!role)
  
  df <- bind_rows(total, df) |> 
    filter(role %in% c("All", "Custodial Officers", "Patrol or Field Officers", "Supervisors")) |> 
    mutate(value = case_when(is.na(value) & name == "Ever" ~ 100,
                             TRUE ~ value),
           experience = gsub("people experiencing |responding to a", "", tolower(experience)))
  

  return(df)
  
}
questions[questions$variable == "q17", 2]

timeq(q19)
timeq(q17)
timeq(q18)
timeq(q20)
timeq(q22)

time_charts <- bind_rows(timeq(q19),
                         timeq(q17),
                         timeq(q18),
                         timeq(q22),
                         timeq(q20) )


time_charts <- time_charts |> 
  group_by(experience) |> 
  mutate(max = case_when(role == "All"  & name == "Ever" ~ value,
                         TRUE ~ NA)) |>
  fill(max, .direction = "downup")
  
experience_levels <- time_charts |>
  filter(role == "All", name == "Ever") |>
  arrange(max) |>
  pull(experience)

time_charts <- time_charts |> 
  arrange(max, experience) |> 
  arrange(max, role, experience, value) |> 
  mutate(name = factor(name),
         experience = factor(experience, levels = experience_levels),
         role = factor(role))



asj_colors <- function (...) {
  scale_fill_manual(
    values =  c("#193f72", "#38c3e1", "#f37659", "#009d8f", "#c5a5a5", "#fef3ee", "#ced4dc"),
  )
}


ggplot(subset(time_charts, grepl("Custodial", role)), aes(x = experience, y = value, fill = name)) +
  geom_col(position = position_dodge(width = 0.8)) +
  asj_colors() +
  geom_text(
    aes(label = paste0(value, "%")),
    position = position_dodge(width = 1),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    x = NULL,
    y = "Percent of officers",
    fill = NULL
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +  # <-- wrap labels
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 0.5),
      # remove y-axis
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      
      # remove gridlines
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
  )



# 52% of patrol officers say they respond to a violent crime in progress a few times a week
# More patrol officers deal with mh crisis and homelessness on a weekly basis than violent crime in progress.

timeq(q21)
timeq(q23)
timeq(q24)
timeq(q25)
timeq(q26)
timeq(q27)
timeq(q28)
timeq(q29)



table(data$q20, useNA = "always")

agree_totals(q40)

##############################################################################################################
######## TABLES AND DATA VIZ FOR PROGRAM SUPPORT BY WORKED IN THOSE PROGRAMS #################################
##############################################################################################################
support_vars <- c("q35", "q36", "q37", "q38", "q39", "q40")
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
  support = c("q35", "q36", "q37", "q38", "q39", "q40"),
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
    program == "q36" ~ "welfare checks",
    program == "q37" ~ "sobering centers",
    program == "q38" ~ "traffic collisions",
    program == "q39" ~ "de-escalation",
    program == "q40" ~ "violence interruption",
    program == "q35" ~ "online reporting"
  ))

# program_ci <- programs_summary_table |>
#   filter(support == "Agree") |>
#   mutate(
#     worked = factor(worked, labels = c("Did not work", "Worked")),
#     se = sqrt(percent * (1 - percent) / n),
#     ci_low = percent - 1.96 * se,
#     ci_high = percent + 1.96 * se
#   )

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
  filter(program == "violence interruption",
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
# or_df <- programs_summary_table |>
#   filter(support %in% c("Agree", "Disagree")) |>
#   group_by(program, worked, support) |>
#   summarise(n = sum(n), .groups = "drop") |>
#   pivot_wider(
#     names_from = c(worked, support),
#     values_from = n,
#     names_sep = "_"
#   )

# or_df <- or_df |>
#   mutate(
#     or = (`1_Agree` / `1_Disagree`) / (`0_Agree` / `0_Disagree`)
#   )
# 
# or_df <- or_df |>
#   mutate(
#     or_label = round(or, 2),
#     direction = ifelse(or > 1, "Above 1", "Below 1")
#   )

####### relative rate q35 to q40 by q41 worked #########

rr_df <- programs_summary_table |>
  filter(support %in% c("Agree", "Disagree")) |>
  group_by(program, worked, support) |>
  summarise(n = sum(n), .groups = "drop") |>
  pivot_wider(
    names_from = c(worked, support),
    values_from = n,
    names_sep = "_"
  ) |>
  mutate(
    r_1 = `1_Agree` / (`1_Agree` + `1_Disagree`),
    r_0 = `0_Agree` / (`0_Agree` + `0_Disagree`),
    rr = r_1 / r_0,
    percent_more_likely = (rr - 1) * 100
  )

rrp1 <- ggplot(rr_df, aes(x = percent_more_likely, y = reorder(program, percent_more_likely))) +
  geom_col(aes(fill = percent_more_likely > 0)) +
  geom_text(aes(label = paste0(round(percent_more_likely), "%")), 
            hjust = ifelse(rr_df$percent_more_likely > 0, -0.15, 1.15), 
            size = 4) +
  scale_fill_manual(values = c("TRUE" = "#003972", "FALSE" = "#f47d20"), 
                    guide = "none") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Percent more likely to agree",
    y = NULL,
    title = "Effect of working on agreement by program"
  ) +
  theme_minimal() +
  xlim(min(rr_df$percent_more_likely) * 1.2, max(rr_df$percent_more_likely) * 1.2)

rrp1

rrp2 <- ggplot(rr_df, aes(x = rr, y = reorder(program, rr))) +
  geom_point(size = 4, color = "#003972") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_text(aes(label = round(rr, 2)), hjust = -0.4, size = 4) +
  labs(
    x = "Risk Ratio (RR)",
    y = NULL,
    title = "Risk ratios for agreement by program"
  ) +
  theme_minimal()

rrp2

# p6 <- ggplot(or_df, aes(x = reorder(program, or), y = or)) +
#   geom_segment(aes(xend = program, y = 1, yend = or), color = "gray80") +
#   geom_point(aes(color = direction), size = 5) +
#   geom_text(aes(label = paste0((or_label), "x"), hjust = ifelse(or > 1, -0.5, 1.5)), size = 3.5) +
#   geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
#   scale_y_log10() +
#   coord_flip() +  
#   scale_color_manual(values = c("Above 1" = "#003972", "Below 1" = "#f47d20")) +
#   labs(
#     x = NULL,
#     title = "Odds Ratios by Program",
#     color = "Direction"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(face = "bold"),
#     plot.title = element_text(face = "bold", hjust = 0.5)
#   )
# 
# p6
#####################################################
####### BENEFITS OF MENTAL HEALTH RESPONSE ##########
#####################################################

programs_temp <- programs |>
  mutate(across(q41_1:q41_6, ~ ifelse(is.na(.) | . == "", 0, 1)),
         selected_any = pmax(q41_1, q41_2, q41_3, q41_4, q41_5))


mh_summary_table <- programs_temp |>
  pivot_longer(
    cols = q41_1:q41_6,
    names_to = "benefit",
    values_to = "response"
  ) |>
 # mutate(response_binary = response) |>
  group_by(benefit) |>
  summarise(
    n = sum(response),
    percent = mean(response) * 100,
    .groups = "drop"
  ) |>
  bind_rows(
    tibble(
      benefit = "selected_any",
      n = sum(programs_temp$selected_any),
      percent = mean(programs_temp$selected_any) * 100
    )
  )

######## ^^STILL NEED TO WRITE.CSV

benefit_labels <- c(
  selected_any = "Selected at least one benefit of having clinicians responding to mental health crisis calls",
  q41_1 = "Clinicians have skills in crisis intervention and make people feel at ease",
  q41_2 = "Clinicians are able to get on the scene quickly to help someone in crisis",
  q41_3 = "Clinicians and police can share their expertise and make decisions together about how to handle crises",
  q41_4 = "Clinicians handle mental health aspects of crisis calls which means officers can return to their other duties more quickly",
  q41_5 = "Clinicians can ensure people who need help get appropriate services or treatment and can divert them away from the justice system",
  q41_6 = "None of these seems beneficial to me"
)

mh_benefit_p <- ggplot(mh_summary_table, aes(x = reorder(benefit, percent), y = percent, fill = benefit)) + 
  geom_col() +
  scale_fill_manual(values = c(
    "selected_any" = "#003972",
    "q41_1" = "#6BAED6",
    "q41_2" = "#6BAED6",
    "q41_3" = "#6BAED6",
    "q41_4" = "#6BAED6",
    "q41_5" = "#6BAED6",
    "q41_6" = "#f47d20"
  )) +
  geom_text(aes(label = paste0(round(percent), "%")),
            vjust = -0.1, hjust = -0.15, size = 4) +
  scale_x_discrete(labels = benefit_labels) +
  labs(
    x = "Benefit",
    y = "Percent",
    title = "Percentage of Participants Agreeing to Each Benefit"
  ) +
  ylim(0, 100) +
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

mh_benefit_p 

agree_disagree_colors <- c(
  "Strongly agree"      = "#08306B",  # dark blue
  "Somewhat agree"      = "#6BAED6",  # light blue
  "Somewhat disagree"   = "#FDBE85",  # light orange
  "Strongly disagree"   = "#D94801"   # dark orange
  )

diversion <- programs_temp |>
  group_by(q42_1) |>
  summarize(n = sum(q41_5),
            percent = mean(q41_5)) 

# 65% of people who have experience working with mental health clinicians or social workers think it is beneficial that clinicians can ensure people who need help get appropriate services or treatment and can divert them away from the justice system
  

###### Q48: Shorter or longer prison sentences? ############

q48 <- tabs(data, q48) |>
  mutate(answer = factor(answer, levels = answer[order(pct)]))

q48_labels <- c(
  "Shorter prison sentences and using money saved to fund youth vio" = "Shorter prison sentences and using money saved to fund youth violence prevention, mental health interventions, and addiction programs",
  "Longer prison sentences and maintaining the prison budget to kee" = "Longer prison sentences and maintaining the prison budget to keep people incarcerated for the full length of their sentences",
  "Don’t know" = "Don’t know"
)

q48_plot <- 
  ggplot(q48, aes(x = answer, y = pct, fill = answer)) +
  geom_col(width = 0.7) +
  labs(title = "Which do you prefer?",
       x = "Answer",
       y = "Percentage",
       fill = "Response") +
  geom_text(aes(label = paste0(round(pct*100), "%")),
            vjust = 0.25, hjust = -0.15, size = 4) +
  scale_x_discrete(labels = benefit_labels) +
  scale_fill_manual(
    values = c(
      "Shorter prison sentences and using money saved to fund youth vio" = "#6BAED6",
      "Longer prison sentences and maintaining the prison budget to kee" = "#f47d20",
      "Don’t know" = "black"
    ),
    labels = q48_labels
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(face = "plain", hjust = 0),
        panel.background = element_blank(),
        plot.background  = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom",
        legend.direction = "vertical")

q48_plot


###### Q49: For people who have completed their sentences and remained crime-free, which do you prefer: ############

q49 <- tabs(data, q49) |>
  mutate(answer = factor(answer, levels = answer[order(pct)]))

q49_labels <- c(
  "Policies that allow them to clear their public records to expand" = "Policies that allow them to clear their public records to expand job opportunities and housing stability. Law enforcement would still maintain access",
  "Policies that allow records to appear on background checks for e" = "Policies that allow records to appear on background checks for employment or housing"
)

q49_all_plot <- 
  ggplot(q49, aes(x = 1, y = pct, fill = answer)) +
  geom_col(width = 0.25) +
  labs(title = "For people who have completed their sentences and remained crime-free, which do you prefer:",
       x = NULL,
       y = "Percentage",
       fill = "Response") +
  geom_text(aes(label = paste0(round(pct*100), "%")),
            position = position_stack(vjust = 0.5),
            size = 4) +
  scale_x_discrete(labels = benefit_labels) +
  scale_fill_manual(
    values = c(
      "Policies that allow them to clear their public records to expand" = "#6BAED6",
      "Policies that allow records to appear on background checks for e" = "#f47d20"
    ),
    labels = q49_labels
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background  = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom",
        legend.direction = "vertical")

q49_all_plot

#########CALL OUT ROLE ON Q49################
q49x <- crosstabs(data, q49, role) |>
  mutate(answer = q49) |>
  group_by(role) |>
  mutate(answer = factor(answer, levels = answer[order(-pct)])) |>
  ungroup()

q49x_plot <- 
  ggplot(q49x, aes(x = answer, y = reorder(answer, pct), fill = answer)) +
  geom_col(width = .95) +
  labs(title = "For people who have completed their sentences and remained crime-free, which do you prefer:",
       x = "",
       y = "Percentage",
       fill = "Response") +
  geom_text(aes(label = paste0(round(pct*100), "%")),
            vjust = -0.5, hjust = 0.5, size = 4) +
  scale_x_discrete(labels = benefit_labels) +
  scale_fill_manual(
    values = c(
      "Policies that allow them to clear their public records to expand" = "#6BAED6",
      "Policies that allow records to appear on background checks for e" = "#f47d20"
    ),
    labels = q49_labels
  ) +
  facet_wrap(~role, strip.position = "bottom",
             labeller = labeller(role = c(
               "Custodial Officer" = "Custodial Officer (n = 56)",
               "Investigator/Detective" = "Investigator/Detective (n = 27)",
               "Patrol or Field Officer/Deputy" = "Patrol or Field Officer/Deputy (n = 99)",
               "Probation Officer" = "Probation Officer (n = 10)",
               "Supervisory role" = "Supervisory role (n = 57)",
               "Other" = "Other (n = 28)"
               ))) +
  guides(fill = guide_legend(reverse = TRUE)) +
  #coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
       # strip.text = element_text(face = "plain", hjust = 0),
        panel.background = element_blank(),
        plot.background  = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(hjust = 0.5),
        panel.spacing.y = unit(0.5, "lines"),
        legend.position = "bottom",
        legend.direction = "vertical")

q49x_plot


