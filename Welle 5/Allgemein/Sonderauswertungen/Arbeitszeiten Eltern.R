# Aline

dat_ges %>%
  filter(S11_3__1 %in% 2:10) %>%
  group_by(as_factor(S2__1)) %>%
  summarise(
    Vor=weighted.mean(A1co__2, Faktor__2, na.rm=T),
    März=weighted.mean(A1d_1__2, Faktor__2, na.rm=T),
    April=weighted.mean(A1d_2__2, Faktor__2, na.rm=T),
    Mai=weighted.mean(A1d_3__2, Faktor__2, na.rm=T),
    Juni=weighted.mean(A1d_4__2, Faktor__2, na.rm=T),
    Juli=weighted.mean(A1d_1__3, Faktor__3, na.rm=T),
    August=weighted.mean(A1d_2__3, Faktor__3, na.rm=T),
    September=weighted.mean(A1d_3__3, Faktor__3, na.rm=T),
    Okztober=weighted.mean(A1d_4__3, Faktor__3, na.rm=T)
    )
