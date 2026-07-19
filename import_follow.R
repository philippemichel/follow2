#  ------------------------------------------------------------------------
#
# Title : Import Follow Data
#    By : PhM
#  Date : 2026-07-02
#
#  ------------------------------------------------------------------------
rm(list = ls())

library(baseph)
library(janitor)
library(readODS)
library(lubridate)
library(tidyverse)
library(labelled)


nn <- c("", " ", "NA", "K", "D", "not performed")

fam <- function(x) {
  zz <- as.character(str_to_lower(x, "en"))
  fam2 <- as.factor(case_when(
    str_detect(zz, "venetoclax") ~ "Venetoclax based",
    str_detect(zz, "inib") ~ "BTKi",
    is.na(zz) ~ NA,
    TRUE ~ "Other"
  ))

  fam2 <-  fct_relevel(fam2,
      "Other", "Venetoclax based", "BTKi"
    )
  return(fam2)
}

# Naissance

tt <- read_ods("datas/Extraction1.ods", sheet = 1, na = nn) |>
  clean_names() |>
  #  mutate(across(ends_with("dte"), dmy)) |>
  mutate(across(is.character, as.factor)) |>
  mutate(across(starts_with("date"), ymd)) |>
  mutate(date_naiss = dmy(paste0("15/", month_of_birth, "/", year_of_birth))) |>
  mutate(age = round(as.numeric(date_inclusion_visite - date_naiss) / 365.25, 0)) |>
  relocate(age, .after = "sex") |>
  dplyr::filter(age > 79) |>
  mutate(agec = cut(age, c(00, 84, 89, 100),
    labels = c("80-84", "85-89", "> 89")
  )) |>
  dplyr::select(-c(month_of_birth, year_of_birth)) |>
  mutate(performance_status_ecog = as.factor(performance_status_ecog)) |>
  mutate(igvh_status = fct_relevel(
    igvh_status,
    "unmutated", "mutated"
  )) |>
  mutate(tp53_status = fct_relevel(
    tp53_status,
    "unmutated", "mutated"
  )) |>
  mutate(performance_status_ecog = fct_recode(performance_status_ecog,
    "0, 1, 2" = "0",
    "0, 1, 2" = "1",
    "0, 1, 2" = "2"
  ))


cardio <- tt |>
  dplyr::select(ischemic_heart_disease:valvulopathy) |>
  mutate(across(everything(), ~ ifelse(.x == "yes", 1, 0))) |>
  mutate(cardio = rowSums(across(everything()))) |>
  mutate(cardio = as.factor(ifelse(cardio > 0, "yes", "no")))

tt <- tt |>
  mutate(cardio = cardio$cardio) |>
  relocate(cardio, .after = "valvulopathy")
#

avant <- read_ods("datas/avant.ods", sheet = 1, na = nn) |>
  clean_names() |>
  mutate(across(is.character, as.factor)) |>
  mutate(across(starts_with("date"), ymd)) |>
  mutate(line = paste(line_1_tt, line_2_tt, line_3_tt, line_4_tt,
    line_5_tt, line_6_tt, line_7_tt,
    sep = " "
  ))

fam2 <- fam(avant$line)
avant <- mutate(avant, status = fam2)
zz <- which(avant$tt_ant == "no" | avant$line == "NA NA NA NA NA NA NA")
avant$status[zz] <- NA

## Réordonnancement de avant$status
avant$status <- avant$status |>
  fct_relevel(
    "Other", "Venetoclax based", "BTKi"
  )

var_label(avant$status) <- "Family of treatment before inclusion"


av2 <- avant |>
  dplyr::select(patient, tt_ant, nb_lines_ant, status) |>
  mutate(status = as.factor(status)) |>
  mutate(nb_lines_ant = as.factor(nb_lines_ant)) |>
  mutate(rechute = ifelse(tt_ant == "yes", "Relapse", "First-line Treatment"))


tt <- left_join(tt, av2, by = "patient")

var_label(tt$tt_ant) <- "Previous Treatment"
var_label(tt$nb_lines_ant) <- "Number of previous Treatment lines"
var_label(tt$status) <- "Previous treatment"
var_label(tt$rechute) <- "Relapse"
var_label(tt$treatment_1) <- "Initial Treatment Type"
var_label(tt$agec) <- "Age"
var_label(tt$performance_status_ecog) <- "Performance Status ECOG"
var_label(tt$igvh_status) <- "IGHV Status"
var_label(tt$tp53_status) <- "TP53 Status"
var_label(tt$cardio) <- "Cardiovascular Comorbidities"
var_label(tt$score_cirs) <- "Score CIRS"


fam2 <- fam(tt$treatment_1)
tt <- mutate(tt, famille = fam2)
var_label(tt$famille) <- "Family of initial treatment"


#
save(tt, file = "datas/follow.RData")
rm(list = ls())
load("datas/follow.RData")
