# Branchen und Betriebsgrößen (heute-journal)

ho_branche <- dat_ges %>%
  dplyr_relfreq2(x=S5__4, y=W4_1a__4, gew=Faktor__4) %>%
  filter(y == "Ja")

ho_gk <- dat_ges %>%
  dplyr_relfreq2(x=F32__1, y=W4_1a__4, gew=Faktor__4) %>%
  filter(y == "Ja")

dat %>%
  dplyr_relfreq(W4_1a, gew=Faktor) 

write.csv2(ho_branche, "./Output/ho_branche.csv")
write.csv2(ho_gk, "./Output/ho_gk.csv")

# Wer hat die Leute ins Homeoffice geholt, AG oder Beschäftigte?
dat %>% dplyr_relfreq2(x=W4_1a, y=W4_3a, gew=Faktor)

# Wann wurde befragt?
table(dat_ges$Interviewtag__4)

