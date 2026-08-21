#  ------------------------------------------------------------------------
#
# Title : Import Face Q
#    By : PhM
#  Date : 2026-08-13
#
#  ---------------------------fam(---------------------------------------

fam <- function(x) {
  library(baseph)
  library(janitor)
  library(readODS)
  library(lubridate)
  library(tidyverse)
  library(labelled)

  nn <- c("", " ", "NA", "K", "#ND#", "#NA#")


  #
  # Demog
  #
  demog <- read_ods("datas/nasal.ods", sheet = 1, na = nn) |>
    clean_names() |>
    mutate(across(is.character, as.factor)) |>
    mutate(across(starts_with("date"), ~ mdy(.x)))
  bb <- read_ods("datas/nasal.ods", sheet = 2, na = nn)
  var_label(demog) <- bb$nom


  #
  # Exclusions des témoins k+
  #
  ww <- which(demog$groupe == "Témoin" & !is.na(demog$histo))
  demog <- demog |>
    filter(!id %in% demog$id[ww])
  #
  tt <- demog |>
    dplyr::select(id, age, sexe)
  #
  # Patients
  #
  pat <- read_ods("datas/patients.ods", sheet = 1, na = nn) |>
    clean_names() |>
    mutate(across(is.character, as.factor)) |>
    left_join(tt, by = "id") |>
    relocate(age, sexe, .before = moment)
  bb <- read_ods("datas/patients.ods", sheet = 2, na = nn)
  var_label(pat) <- bb$nom
  #
  # témoins
  #
  tem <- read_ods("datas/temoins.ods", sheet = 1, na = nn) |>
    clean_names() |>
    mutate(across(is.character, as.factor)) |>
    left_join(tt, by = "id") |>
    relocate(age, sexe, .before = moment)
  bb <- read_ods("datas/temoins.ods", sheet = 2, na = nn)
  var_label(tem) <- bb$nom
  tem <- tem |>
    filter(!id %in% demog$id[ww])
  #
  ###########################################################
  #
  #
  ###########################################################
  #
  #                             Témoins
  #
 demogt <- read_ods("datas/nasal.ods", sheet = 1, na = nn) |>
    clean_names() |>
    mutate(across(is.character, as.factor)) |>
    mutate(across(starts_with("date"), ~ mdy(.x)))
  bb <- read_ods("datas/nasal.ods", sheet = 2, na = nn)
  var_label(demogt) <- bb$nom


#
  ww <- which(demogt$groupe != "Témoin" & !is.na(demog$histo))
  demogt <- demogt |>
    filter(!id %in% demog$id[ww])
  #
  ttem <- demogt |>
    dplyr::select(id, age, sexe)
  tem <- read_ods("datas/temoins.ods", sheet = 2, na = nn) |>
    clean_names() |>
    mutate(across(is.character, as.factor)) |>
    left_join(ttem, by = "id") |>
    relocate(age, sexe, .before = moment)
  bb <- read_ods("datas/temoins.ods", sheet = 3, na = nn)
  var_label(tem) <- bb$nom
  #
  ###########################################################
  #

  se(demog, pat, tem, file = "datas/faceq.RData")
}


# rm(list = ls())
load("datas/faceq.RData")
