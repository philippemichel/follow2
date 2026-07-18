#  ------------------------------------------------------------------------
#
# Title : Import Follow Data
#    By : PhM
#  Date : 2026-07-02
#
#  ------------------------------------------------------------------------

library(baseph)
library(janitor)
library(readODS)
library(lubridate)
library(tidyverse)
library(labelled)


nn <- c("", " ", "NA", "K", "D", "not performed")

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
  mutate(tt_initial = as.factor(ifelse(str_sub(type_of_cll_treatment_1, 1, 10) ==
    "venetoclax", "Durée fixe", "Durée variable"))) |>
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
  )) |>
  mutate(venetoclax = ifelse(str_detect(line, "venetoclax"), "yes", "no")) |>
  mutate(inhibitor = ifelse(str_detect(line, "tinib"), "yes", "no"))

avant <-
  avant |>
  mutate(
    status = case_when(
      venetoclax == "yes" & inhibitor == "yes" ~ "venetoclax + BTK inhibitor",
      venetoclax == "yes" ~ "venetoclax",
      inhibitor == "yes" ~ "BTK inhibitor",
      tt_ant == "yes" ~ "other treatment",
      TRUE ~ "no"
    )
  ) |>
  mutate(status = fct_relevel(
    status,
    "no", "venetoclax", "BTK inhibitor", "venetoclax + BTK inhibitor",
    "other treatment"
  ))

tt <- tt |>
  mutate(venetoclax = ifelse(str_detect(
    type_of_cll_treatment_1,
    "venetoclax"
  ), "yes", "no")) |>
  mutate(inhibitor = ifelse(str_detect(type_of_cll_treatment_1, "tinib"),
    "yes", "no"
  )) |>
  mutate(
    type_tt_init = case_when(
      venetoclax == "yes" & inhibitor == "yes" ~ "venetoclax + BTK inhibitor",
      inhibitor == "yes" ~ "BTK inhibitor",
      venetoclax == "yes" ~ "venetoclax",
      TRUE ~ "Other treatment"
    )) |>
  mutate(type_tt_init = fct_relevel(
    type_tt_init,
    "venetoclax", "BTK inhibitor", "venetoclax + BTK inhibitor",
    "Other treatment"
  ))

tt$type_tt_init2 <- tt$type_tt_init |>
  fct_recode(
    "venetoclax" = "venetoclax + BTK inhibitor"
  )




tt <- tt |>
  mutate(type_tt_init = zz$type_tt_init)

av2 <- avant |>
  dplyr::select(patient, tt_ant, nb_lines_ant, status) |>
  mutate(status = as.factor(status)) |>
  mutate(nb_lines_ant = as.factor(nb_lines_ant)) |>
  mutate(rechute = ifelse(tt_ant == "yes", "Relapse", "First-line Treatment"))

var_label(av2$tt_ant) <- "Previous Treatment"
var_label(av2$nb_lines_ant) <- "Number of previous Treatment lines"
var_label(av2$status) <- "Previous treatment"
var_label(av2$rechute) <- "Relapse"
var_label(tt$type_of_cll_treatment_1) <- "Initial Treatment Type"
var_label(tt$tt_initial) <- "Initial Treatment"
var_label(tt$type_tt_init) <- "Initial  Type of Treatment"
var_label(tt$type_tt_init2) <- "Initial  Type of Treatment"

tt <- left_join(tt, av2, by = "patient")
rm(avant, av2, cardio, zz)


#
save(tt, file = "datas/follow.RData")
rm(list = ls())
load("datas/follow.RData")
