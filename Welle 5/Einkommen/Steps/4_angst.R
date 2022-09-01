unbalanced <- dat_long %>% 
  mutate(angst = case_when(
    F5_w235 %in% 1:2 ~ "Ja / eher ja",
    F5_w235 %in% 3:4 ~ "Eher nein / Nein, auf keinen Fall"
  )) %>% 
  #filter(S6_w123 %in% 1:2 | S6_w45 %in% 1:2) %>% 
  dplyr_relfreq2(Welle, angst, Faktor_w12345) 

balanced <- dat_long %>% group_by(lfdn_w1__1) %>% mutate(teilnahme = sum(
  Welle == 2 & !is.na(S1_w12345),
  Welle == 3 & !is.na(S1_w12345),
  Welle == 5 & !is.na(S1_w12345)),
  et = sum(
    Welle == 2 & SC1_w12345 == 1,
    Welle == 3 & SC1_w12345 == 1,
    Welle == 5 & SC1_w12345 == 1
  ),
  gew_w5 = Faktor_w12345[5]) %>%
  filter(teilnahme == 3 & et == 3) %>% 
  mutate(angst = case_when(
    F5_w235 %in% 1:2 ~ "Ja / eher ja",
    F5_w235 %in% 3:4 ~ "Eher nein / Nein, auf keinen Fall"
    )) %>%
  dplyr_relfreq2(Welle, angst, gew_w5)



x <- tibble(
  Welle = unbalanced[,1],
  Sorge = unbalanced[,2],
  Alle = unbalanced$anteil, `Nur Erwerbstätig in allen Wellen` = balanced$anteil
) %>% pivot_longer(cols=-c(1,2), names_to="Betrachtung", values_to="Anteil") %>%
  filter(Sorge$y == "Ja / eher ja")

ggplot(x, aes(x=Welle$x, y=Anteil, fill=Betrachtung )) +
  geom_text(aes(x=Welle$x, y=Anteil+0.5, group=Betrachtung, label=Anteil ), position = position_dodge(width=0.9)) +
  geom_col(position="dodge") +
  labs(title="Einkommen vor der Krise und anteilige Verluste", x="", y="%")
