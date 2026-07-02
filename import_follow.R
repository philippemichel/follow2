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
  ))

# bn <- read_ods("datas/NAISSANCE.ods", sheet = 2, na = nn)
# var_label(naiss) <- bn$nom


#
save(tt, file = "datas/follow.RData")
load("datas/follow.RData")
