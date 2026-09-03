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
    mutate(across(starts_with("date"), ~ mdy(.x))) |>
    mutate(frontal = ifelse(nature_ntervention %in%
      c("Lambeau de schmid-Meyer", "Lambeau frontal"),
    "yes", "no"
    ))
  bb <- read_ods("datas/nasal.ods", sheet = 2, na = nn)
  var_label(demog) <- c(bb$nom, "lambeau frontal")

  # Exclusions des témoins k+
  #
  ww <- which(demog$groupe == "Témoin" & !is.na(demog$histo))
  demog <- demog |>
    filter(!id %in% demog$id[ww])
  #
  # Patients
  #
  demogt <- demog |>
    dplyr::filter(groupe == "Patient") |>
    dplyr::select(id, age, sexe)

  pat <- read_ods("datas/patients.ods", sheet = 1, na = nn) |>
    clean_names() |>
    mutate(across(is.character, as.factor)) |>
    left_join(demogt, by = "id") |>
    relocate(age, sexe, .before = moment)
  bb <- read_ods("datas/patients.ods", sheet = 2, na = nn)
  var_label(pat) <- bb$nom
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
    dplyr::filter(groupe == "Témoin") |>
    dplyr::select(id, age, sexe)

  tem <- read_ods("datas/temoins.ods", sheet = 1, na = nn) |>
    clean_names() |>
    mutate(across(is.character, as.factor)) |>
    left_join(demogt, by = "id") |>
    relocate(age, sexe, .before = moment)
  bb <- read_ods("datas/temoins.ods", sheet = 2, na = nn)
  var_label(tem) <- bb$nom
  #
  ###########################################################
  #

  save(demog, pat, tem, file = "datas/faceq.RData")
}


# rm(list = ls())
load("datas/faceq.RData")
