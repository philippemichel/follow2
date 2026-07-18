dd <- which(tt$patient_status_m12 == "deceased")
mm18 <- tt$patient_status_m18
mm18[dd] <- "deceased"
dd2 <- which(mm18 == "deceased")
mm24 <- tt$patient_status_m24
mm24[dd2] <- "deceased"

m12 <- round(prop.table(table(tt$patient_status_m12)) * 100, 1)[[1]]
m18 <- round(prop.table(table(tt$patient_status_m18)) * 100, 1)[[1]]
m24 <- round(prop.table(table(tt$patient_status_m24)) * 100, 1)[[1]]

nt12 <- length(na.omit(tt$patient_status_m12))
nt18 <- length(na.omit(mm18))
nt24 <- length(na.omit(mm24))

m0 <- 100
nt0 <- 211

survx <- c(m0, m12, m18, m24)
nb <- c(nt0, nt12, nt18, nt24)
month <- c("Inclusion", "M12", "M18", "M24")

tibble(month, survx, nb) |>
  kbl()
