# Welle 4; kleiner Exkurs
write.csv2(
  dat %>%
  filter(Welle == 4) %>%
  mutate(schuld = case_when(
    W4_3a_w4 == 1 ~ "im Homeoffice, ok",
    W4_4a_w4 == 1 ~ "im Betrieb, ok",
    (W4_3a_w4 == 2 & W4_4a_w4 == 1) | (W4_3b_w4 == 3 & W4_4b_w4 == 1) ~ "Wunsch mehr Homeoffice, Arbeitgeber verhindert das",
    (W4_3a_w4 == 2 & W4_4a_w4 == 2) | (W4_3b_w4 == 3 & W4_4b_w4 == 2) ~ "Wunsch mehr Homeoffice, andere Gründe",
    (W4_3a_w4 == 3 & W4_4a_w4 == 1) | (W4_3b_w4 == 2 & W4_4b_w4 == 1) ~ "Wunsch weniger Homeoffice, Arbeitgeber verhindert das",
    (W4_3a_w4 == 3 & W4_4a_w4 == 2) | (W4_3b_w4 == 2 & W4_4b_w4 == 2) ~ "Wunsch weniger Homeoffice, andere Gründe",
    W4_3a_w4 == 4 | W4_2_w4 == 4 ~ "Trifft nicht zu / keine Eignung für Homeoffice"
  )) %>%
  group_by(schuld) %>%
  filter(!is.na(schuld)) %>%
  summarise(n=sum(gewicht_w4)) %>%
    mutate(p = round(n/sum(n)*100)),
  "Output/schuld.csv"
)
