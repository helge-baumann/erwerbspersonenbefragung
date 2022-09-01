# Verteilung der Homeoffice-Variablen----
ho <- dat %>%
  dplyr_relfreq(x = W4_1, gew = Faktor) %>%
  mutate(dec = anteil)
ho$jan <- dat %>%
  dplyr_relfreq(x = A2, gew = Faktor) %>%
  pull(anteil)

# Verteilung von April 2020 bis November
ho$apr <- dat_13 %>%
  dplyr_relfreq(x = F2__1, gew = Faktor__1) %>%
  pull(anteil)
ho$jun <- dat_13 %>%
  dplyr_relfreq(x = A2__2, gew = Faktor__2) %>%
  pull(anteil)
ho$nov <- dat_13 %>%
  dplyr_relfreq(x = A2__3, gew = Faktor__3) %>%
  pull(anteil)

# Longformat für Plot Zeitreihe
ho_long <-
  ho %>%
  select(-anteil, -n) %>%
  pivot_longer(cols = -x, names_to = "Monat", values_to = "Wert")

ho_long$Monat[ho_long$Monat == "apr"] <- "2020/04/15"
ho_long$Monat[ho_long$Monat == "jun"] <- "2020/06/15"
ho_long$Monat[ho_long$Monat == "nov"] <- "2020/11/15"
ho_long$Monat[ho_long$Monat == "dec"] <- "2020/12/15"
ho_long$Monat[ho_long$Monat == "jan"] <- "2021/01/15"
ho_long$Monat <- as.Date(ho_long$Monat)

plot_ho <-
  ggplot(ho_long, 
         aes(x = Monat, y = Wert, color = x, label = paste(Wert, "%"))) +
  geom_line() +
  geom_text(nudge_y = 2) +
  theme(legend.position = "bottom") +
  scale_color_manual(
    name = "",
    position = "bottom",
    values = c(
      "Ich habe ausschließlich / überwiegend in meinem Betrieb gearbeitet." = "#00384f",
      "Ich habe an wechselnden Arbeitsorten gearbeitet (Betrieb, zu Hause, mobil von unterwegs)" = "#00668a",
      "Ich habe ausschließlich / überwiegend von zu Hause aus gearbeitet." = "#4197bc",
      "Traf auf mich nicht zu" = "#d3e3ee"
    )
  ) +
  guides(
    color = guide_legend(ncol = 1, title = NULL, byrow = T)
  ) +
  labs(x = "", y = "")

# Sehr gut. 18 --> 25%

# Beschluss hat geholfen?----
beschluss <- dat %>% dplyr_relfreq(x = W4_1a, gew = Faktor)
# 13% der Befragten sagten, sie arbeiten jetzt verstärkt im Homeoffice
# aufgrund der neuen Verordnung. Passt auch gut zu den Homeoffice-Antworten.

beschluss_ho <- dat %>% dplyr_relfreq2(x = A2, y = W4_1a, gew = Faktor)
# 33% der im Homeoffice Beschäftigten sagen, sie täten das WEGEN der Verordnung.

beschluss_kausal <- dat %>%
  filter(dat$W4_1 != 2 & dat$A2 == 2) %>%
  dplyr_relfreq(x = W4_1a, gew = Faktor)

# Potenzial----
potenzial <- dat %>% dplyr_relfreq(x = W4_2, gew = Faktor)
# 20% voll, 20% teilweise, 13% weniger.
# Im Aggregat muss man sagen: Das Potenzial ist ganz gut ausgeschöpft.

potenzial_ho <- dat %>% dplyr_relfreq2(x = W4_2, y = A2, gew = Faktor)

# Plot Potenzial
plot_potenzial <-
  potenzial_ho %>%
  ggplot(aes(x = x, y = anteil, fill = y, label = paste(anteil, "%"))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    name = "", position = "bottom",
    values = c(
      "Ich arbeite ausschließlich / überwiegend in meinem Betrieb." = "#00384f",
      "Ich arbeite an wechselnden Arbeitsorten (Betrieb, zu Hause, mobil von unterwegs)." = "#00668a",
      "Ich arbeite ausschließlich / überwiegend von zu Hause aus." = "#4197bc",
      "Trifft nicht auf mich zu" = "#d3e3ee"
    )
  ) +
  theme(legend.position = "bottom") +
  geom_text(size = 3, position = position_stack(vjust = 0.5), color = "white") +
  guides(
    fill = guide_legend(ncol = 2, title = NULL, byrow = T)
  ) +
  labs(y = "", x = "", title = "Verteilung Homeoffice im Januar nach Eignung der Tätigkeit") +
  scale_x_discrete(labels = c(
    "Tätigkeit kann \n uneingeschränkt von zu Hause \n ausgeführt werden.",
    "Tätigkeit kann \n zum Großteil von zu Hause \n ausgeführt werden.",
    "Tätigkeit kann \n zum Großteil nur im Betrieb \n ausgeführt werden.",
    "Tätigkeit ist \n für das Homeoffice \n ungeeignet."
  ))

# 17% derjenigen, die vollständig im Homeoffice arbeiten können: im Betrieb
# 32% derjenigen, die zum Großteil von zu Hause aus arbeiten können: im Betrieb

# Die könnte man sich nochmal genauer anschauen:
potenzial_betrieb <- dat %>%
  filter(W4_2 %in% c(1, 2)) %>%
  dplyr_relfreq2(x = S5, y = A2, gew = Faktor) %>%
  filter(n > 100) %>%
  ggplot(aes(x = x, y = anteil, fill = y, label = paste(anteil, "%"))) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  theme(legend.position = "bottom") +
  geom_text(size = 3, position = position_stack(vjust = 0.5), color = "white") +
  scale_fill_manual(
    name = "", position = "bottom",
    values = c(
      "Ich arbeite ausschließlich / überwiegend in meinem Betrieb." = "#00384f",
      "Ich arbeite an wechselnden Arbeitsorten (Betrieb, zu Hause, mobil von unterwegs)." = "#00668a",
      "Ich arbeite ausschließlich / überwiegend von zu Hause aus." = "#4197bc",
      "Trifft nicht auf mich zu" = "#d3e3ee"
    )
  ) +
  theme(legend.position = "bottom") +
  geom_text(size = 3, position = position_stack(vjust = 0.5), color = "white") +
  guides(
    fill = guide_legend(ncol = 2, title = NULL, byrow = T)
  ) +
  labs(y = "", x = "", title = "Verteilung Homeoffice im Januar nach Branchen \n (nur für Homeoffice geeignete Tätigkeiten")

# lässt sich nicht viel draus ziehen.

# Verantwortlichkeit---
ho_wunsch <- dat %>%
  filter(A2 == 2) %>%
  dplyr_relfreq(x = W4_3a, gew = Faktor)
betrieb_wunsch <- dat %>% dplyr_relfreq(x = W4_3b, gew = Faktor)

# weniger Leute, die im Homeoffice sind, würden gerne weniger im Homeoffice arbeiten als das Pendant im Betrieb.

# wer hindert?
ho_hindert <- dat %>% dplyr_relfreq2(x = W4_3a, y = W4_4a, gew = Faktor)
betrieb_hindert <- dat %>% dplyr_relfreq2(x = W4_3b, y = W4_4b, gew = Faktor)

# Zusätzlich Fragen
# wer wird gehindert?
dat %>% dplyr_relfreq(x=SC1, gew=Faktor)
pot <- dat %>% dplyr_relfreq(x=W4_2, gew=Faktor, round=F)
betr <- dat %>% filter(W4_2 %in% c(1,2)) %>% dplyr_relfreq(x=A2, gew=Faktor, round=F)
ho_wunsch2 <- dat %>% filter(W4_2 %in% c(1,2) & A2 == 1) %>% dplyr_relfreq(W4_3b, gew=Faktor, round=F)
ho_wunsch2_zwang <- dat %>% filter(W4_2 %in% c(1,2) & A2 == 1 & W4_3b == 3) %>% 
  dplyr_relfreq(W4_4b, gew=Faktor, round=F)

# nur Arbeiter und Angestellte
dat %>% filter(S6 %in% 1:3) %>% dplyr_relfreq(x=W4_2, gew=Faktor)
dat %>% filter(S6 %in% 1:3) %>% filter(W4_2 %in% c(1,2)) %>% dplyr_relfreq(x=A2, gew=Faktor)
dat %>% filter(W4_2 %in% c(1,2) & A2 == 1) %>% dplyr_relfreq(W4_3b, gew=Faktor)
dat %>% filter(S6 %in% 1:3) %>% filter(W4_2 %in% c(1,2) & A2 == 1 & W4_3b == 3) %>% 
  dplyr_relfreq(W4_4b, gew=Faktor)

# zum Homeoffice gezwungen
dat %>% filter(W4_2 %in% c(3,4) & A2 %in% c(2,3) & W4_3a == 3) %>% 
  dplyr_relfreq(W4_4a, gew=Faktor)
dat %>% filter(W4_2 %in% c(3,4) & A2 %in% c(2,3) & W4_3a == 3) %>% 
  dplyr_relfreq(W4_4a, gew=Faktor)

# Betriebsrat
br <- dat_13 %>% select(lfdn__1, D9__3)
dat <- dat %>% left_join(br, by=c("lfdn_W1" = "lfdn__1"))

dat %>% dplyr_relfreq2(D9__3, A2, gew=Faktor)

# Wechsel
dat <- 
  dat %>%
  mutate(
    change = 
      case_when(
        W4_1 == 1 & A2 == 1 ~ "unverändert im Betrieb",
        W4_1 == 2 & A2 == 2 ~ "unverändert Wechsel",
        W4_1 == 3 & A2 == 3 ~ "unverändert Homeoffice",
        A2 > W4_1 & W4_1 != 4 & A2 != 4 ~ "mehr Homeoffice",
        A2 < W4_1 & W4_1 != 4 & A2 != 4 ~ "weniger Homeoffice"
      )
  )

merge <- 
  dat_13 %>% select(lfdn__1, S3__3, A2b_1__3, A2b_2__3, A2b_3__3,
                    F5__3, A5__3, D2__3, D4__3, D7__3, D8__3) 
dat <- dat %>% left_join(merge, by=c("lfdn_W1" = "lfdn__1"))
  
  
change <- dat %>% 
  filter(!is.na(change)) %>%
  select(change, S5, S2, starts_with("S11"), , A3, S3__3, starts_with("A2b"), 
         F5__3, A5__3, D2__3, D4__3, D7__3, D8__3, D9__3, Faktor) %>%
  mutate_at(vars(-Faktor), as_factor) %>%
  pivot_longer(cols=-c(change, Faktor), names_to="Variable", values_to="Wert") %>%
  group_by(Variable, Wert, change) %>%
  summarise(anteil = sum(Faktor), n=n()) %>%
  mutate(anteil=round((anteil/sum(anteil)*100)), n=sum(n)) %>%
  filter(!(Wert %in% c("NA", "Weiß nicht", "Keine Angabe")))
  
write.csv2(change, "./Output/Change.csv")

zukunft <- dat %>% filter(A2 %in% 2:3) %>% dplyr_relfreq(A2d, gew=Faktor)
