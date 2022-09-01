dat %>%
  mutate(
    partei_6 = as_factor(
      case_when(
        W6_2_w6 %in% 1:2 ~ "CDU/CSU",
        W6_2_w6 == 3 ~ "SPD",
        W6_2_w6 == 7 ~ "AfD",
        W6_2_w6 == 5 ~ "FDP",
        W6_2_w6 == 4 ~ "Bündnis 90 / Die Grünen",
        W6_2_w6 == 6 ~ "Die Linke",
        W6_2_w6 %in% 8:14 ~ "andere Partei",
        W6_2_w6 %in% 96 | W6_SC3_w6 == 3 ~ "nicht oder ungültig"
      )),
    partei_5 = 
      factor(case_when(
        partei_5 == levels(partei_5)[1] ~ "CDU/CSU",
        partei_5 == levels(partei_5)[2] ~ "SPD",
        partei_5 == levels(partei_5)[3] ~ "AfD",
        partei_5 == levels(partei_5)[4] ~ "FDP",
        partei_5 == levels(partei_5)[6] ~ "Bündnis 90 / Die Grünen",
        partei_5 == levels(partei_5)[5] ~ "Die Linke",
        partei_5 %in% levels(partei_5)[7:14] ~ "andere Partei",
        partei_5 %in% levels(partei_5)[15:16] ~ "nicht oder ungültig"
        ), levels=c("CDU/CSU", "Bündnis 90 / Die Grünen", "SPD", "FDP", "AfD", "Die Linke", "andere Partei", "nicht oder ungültig"))
    )%>%
  group_by(partei_5, partei_6) %>%
  filter(!is.na(partei_5) & !is.na(partei_6)) %>%
  summarise(n = sum(Faktor_w123456), N=n()) %>%
  mutate(p = round(n/sum(n)*100), N=sum(N)) %>%
  pivot_wider(names_from=partei_6, values_from=p, id_cols=c(partei_5, N)) %>%
  
  select(partei_5, N, `CDU/CSU`, `Bündnis 90 / Die Grünen`, SPD, FDP, AfD, `Die Linke`, `andere Partei`, `nicht oder ungültig`) %>%
  write.csv2("Output/wählerwanderung2.csv")
  

# Politikpräferenzen
# Themen
dat %>%
  mutate(spd = 
           case_when(
             partei_5 == "SPD" & W6_2_w6 == 3 ~ "SPD-Stammwähler",
             partei_5 %in% levels(partei_5)[c(1,3:16,18,19)] & W6_2_w6 == 3 ~ "zur SPD gewechselt",
             W6_2_w6 %in% c(1,2, 4:14) ~ "Nicht-SPD-Wähler"
           )) %>%
  pivot_longer(cols=c(W6_4a_1_w6:W6_4g_3_w6), names_to="Variable", values_to="Wert") %>%
  filter(Wert %in% 1:7) %>%
  mutate(
    Wert = case_when(
      Wert %in% 1:2 ~ "wichtig", 
      Wert %in% 3:5 ~ "neutral", 
      Wert %in% 6:7 ~ "unwichtig")) %>%
  group_by(spd, Variable, Wert) %>%
  summarise(n = sum(Faktor_w123456), N=n()) %>%
  mutate(p = round(n/sum(n)*100), N=sum(N)) %>%
  filter(Wert == "wichtig" & !is.na(spd)) %>%
  select(spd, Variable, p) %>%
  pivot_wider(names_from=spd, values_from=p, id_cols=c(Variable)) %>%
  mutate(
    Variable = sapply(Variable, function(x) attributes(dat[[x]])$label),
    Variable = str_remove_all(Variable, ".*: ")) %>%
  write.csv2("Output/spd_praeferenzen.csv", row.names=F)
