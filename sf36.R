

#  ------------------------------------------------------------------------
#
# Title : SF36
#    By : PhM
#  Date : 2026-08-21
#
#  ------------------------------------------------------------------------


#
#       Import des données & transformation numérique
#
sfimport <- function(dt, sheet = 1) {
  library(tidyverse)
  library(readODS)
  library(janitor)
#
sff <- read_ods(dt, sheet = sheet, col_names = TRUE) |>
  janitor::clean_names()
#
sffn <- sff |>
  mutate(across(everything(),~ as.numeric(str_sub(.x, 1, 1))))
#



#
# Calcul des scores SF36
#



# PF

sffnx <- sffn |>
  mutate(pf = rowSums(across(c(q3a:q3h), ~.x))) |>
  mutate(pf = 100*pf/24) |>
  mutate(rp = rowSums(across(c(q4a:q4d), ~.x))) |>
  mutate(rp = 100*rp/8) |>
  mutate(bp = rowSums(across(c(q7,q8), ~.x))) |>
  mutate(bp = 100*bp/10) |>
  mutate(gh = rowSums(across(c(q1,q11a:q11d), ~.x))) |>
  mutate(gh = 100*gh/25) |>
  mutate(vt = rowSums(across(c(q9a,q9e,q9g,q9i), ~.x))) |>
  mutate(vt = 100*vt/24) |>
  mutate(sf = rowSums(across(c(q6,q10), ~.x))) |>
  mutate(sf = 100*sf/9) |>
  mutate(re = rowSums(across(c(q5a:q5c), ~.x))) |>
  mutate(re = 100*re/6) |>
  mutate(mh = rowSums(across(c(q9b:q9d,q9f,q9h), ~.x))) |>
  mutate(mh = 100*mh/30) |>
  mutate(pcs = (pf +rp +bp + gh)/4) |>
  mutate(mcs = (vt + sf + re + mh)/4) |>
  dplyr::select(pf:mcs)

save(sff,sffn,sffnx, file = "datas/sf36.RData")
}

