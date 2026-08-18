zz <- pat |>
  dplyr::select(a_symetrie_visage:d_parfaite_sante) |>
  mutate(across(everything(), ~ as.numeric(str_sub(.x, 1, 1))))

dl <- c(0, 10, 22, 32, 49, 55, 63, 73, 77, 96, 106, 110, 116, 127, 130)
patn <- tibble(id = pat$id, jj = pat$moment)
for (i in seq_along(dl[-length(dl)])) {
  dp <- zz[, (dl[i] + 1):dl[i + 1]]
  nom <- paste0("pat", i)
  ptn <- tibble(rowSums(dp, na.rm = TRUE))
  names(ptn)[1] <- nom
  patn <- cbind(patn, ptn)
}


aa <- patn |>
  dplyr::select(id, jj, pat1) |>
  mutate(nj = paste0(id, "_", jj)) |>
  dplyr::filter_out(id == "09024AE") |>
  pivot_wider(names_from = nj, values_from = pat1)


patnx <-
  patn |>
  group_by(id) |>
  dplyr::filter(n() == 2) |>
  ungroup() |>
  droplevels() |>
  arrange(id, jj)

ad0 <- NULL
ad7 <- NULL
for (i in seq(1, 130, 2)) {
  ad0 <- c(ad0, patnx$pat1[i])
  ad7 <- c(ad7, patnx$pat1[i + 1])
}
aat <- t.test(ad0, ad7, paired = TRUE)
mm <- round(aat$estimate, 2)
bb <- round(aat$conf.int[[1]], 2)
hh <- round(aat$conf.int[[2]], 2)
moy <- paste0(mm, " [", bb, " ; ", mm, "]")
vp <- beaup(aat$p.value)
