attributes(dat_ges$F20_3__1)
attributes(dat_ges$A2b_3__3)

regeln_branche <- dat_ges %>%
  dplyr_relfreq2(S5__1, F20_3__1, gew=Faktor__1) %>%
  filter(y=="Ja")

regeln_branche$nov <- dat_ges %>%
  dplyr_relfreq2(S5__1, A2b_3__3, gew=Faktor__3) %>%
  filter(y=="Ja") %>%
  pull(anteil)

regeln_gk <- dat_ges %>%
  dplyr_relfreq2(F32__1, F20_3__1, gew=Faktor__1) %>%
  filter(y=="Ja")

regeln_gk$nov <- dat_ges %>%
  dplyr_relfreq2(F32__1, A2b_3__3, gew=Faktor__3) %>%
  filter(y=="Ja") %>%
  pull(anteil)

write.csv2(regeln_branche, "./Output/Regeln_Branche.csv")
write.csv2(regeln_gk, "./Output/Regeln_GK.csv")
