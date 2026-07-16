tt |>
  dplyr::select(patient_status_m12) |>
  summarise(n_distinct(patient_status_m12))


zz <- tibble(tt$patient_status_m12, mm18)


m12 <- round(prop.table(table(tt$patient_status_m12)) * 100, 1)[[1]]

dd <- which(tt$patient_status_m12 == "deceased")

mm18 <- tt$patient_status_m18
mm18[dd] <- "deceased"
dd2 <- which(mm18 == "deceased")
mm24 <- tt$patient_status_m24
mm24[dd2] <- "deceased"

m24 <- round(prop.table(table(mm24)) * 100, 1)[[1]]
