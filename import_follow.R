#  ------------------------------------------------------------------------
#
# Title : Import Follow Data
#    By : PhM
#  Date : 2026-11-12
#
#  ------------------------------------------------------------------------
rm(list = ls())

library(baseph)
library(janitor)
library(readODS)
library(lubridate)
library(tidyverse)
library(labelled)


nn <- c("", " ", "NA", "K", "D", "Unknown", "unknown", "not performed")

fam <- function(x) {
  zz <- as.character(str_to_lower(x, "en"))
  fam2 <- as.factor(case_when(
    str_detect(zz, "venetoclax") ~ "Venetoclax based",
    str_detect(zz, "inib") ~ "BTKi",
    is.na(zz) ~ NA,
    TRUE ~ "Other"
  ))

  fam2 <- fct_relevel(
    fam2,
    "Other", "Venetoclax based", "BTKi"
  )
  return(fam2)
}

# Naissance

tt <- read_ods("datas/FOLLOW.ods", sheet = 1, na = nn) |>
  clean_names() |>
  mutate(across(is.character, as.factor)) |>
  mutate(across(starts_with("date"), mdy)) |>
  mutate(date_naiss = dmy(paste0("15/", month_birth, "/", year_birth))) |>
  mutate(age = round(as.numeric(date_inclusion - date_naiss) / 365.25, 0)) |>
  relocate(age, .after = "sex") |>
  dplyr::filter(age > 79) |>
  mutate(agec = cut(age, c(00, 84, 89, 100),
    labels = c("80-84", "85-89", "> 89")
  )) |>
  mutate(across(where(is.factor), fct_drop)) |>
  dplyr::select(-c(month_birth, year_birth)) |>
  mutate(ecog = as.factor(ecog)) |>
  mutate(igvh_status = fct_relevel(
    igvh_status,
    "unmutated", "mutated"
  )) |>
  mutate(tp53_status = fct_relevel(
    tp53_status,
    "unmutated", "mutated"
  )) |>
  mutate(ecog = fct_recode(ecog,
    "0, 1" = "0",
    "0, 1" = "1",
    "2, 3, 4" = "2",
    "2, 3, 4" = "3"
  )) |>
  remove_empty()

cardio <- tt |>
  dplyr::select(ischemic_heart_disease:valvulopathy) |>
  mutate(across(everything(), ~ ifelse(.x == "yes", 1, 0))) |>
  mutate(cardio = rowSums(across(everything()))) |>
  mutate(cardio = as.factor(ifelse(cardio > 0, "yes", "no")))

tt <- tt |>
  mutate(cardio = cardio$cardio) |>
  relocate(cardio, .after = "valvulopathy")
#


tt <- tt |>
  mutate(type_treatment_1 =tolower(type_treatment_1)) |>
  mutate(tt1 = case_when(
    str_detect(type_treatment_1, "venetocl") ~ "venetoclax",
    str_detect(type_treatment_1, "inib") ~ "BTKi",
    type_treatment_1 == "Other" ~ "Other",
    TRUE ~ "Autre traitement"
  )) |>
  mutate(tt1 = as.factor(tt1)) |>
  mutate(tt1 = fct_relevel(tt1, "BTKi", "venetoclax", "Autre traitement" ))
var_label(tt$tt1) <- "Traitement initial"





# var_label(tt$tt_ant) <- "Previous Treatment"
# var_label(tt$nb_lines_ant) <- "Number of previous Treatment lines"
# var_label(tt$status) <- "Previous treatment"
# var_label(tt$rechute) <- "Relapse"
# var_label(tt$agec) <- "Age"
# var_label(tt$performance_status_ecog) <- "Performance Status ECOG"
# var_label(tt$igvh_status) <- "IGHV Status"
# var_label(tt$tp53_status) <- "TP53 Status"
# var_label(tt$cardio) <- "Cardiovascular Comorbidities"
# var_label(tt$score_cirs) <- "Score CIRS"





#
save(tt, file = "datas/follow.RData")
rm(list = ls())
load("datas/follow.RData")
