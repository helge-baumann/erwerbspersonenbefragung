homewechsel <- dat %>%
  filter(A2__3 == 1) %>%
  mutate(rueck = if_else(F2__1 %in% c(2,3) & A2__3 == 1, 1, 0),
         branche = as_factor(S5__3)) %>%
  group_by(branche, rueck) %>% 
  summarise(anteil = sum(Faktor__3), n=n()) %>%
  mutate(anteil=round((anteil/sum(anteil)*100)), n=sum(n))

  

Ergebnis <- list(
  Varnames = list(),
  Label = list(),
  Labels = list(),
  Hinweis = list(),
  Tabelle = list(),
  Plot = list()
)

strata <- c("Alle", "Alter", "Geschlecht", "Kinder", "Branche", "BG", "BR")

dat$F2__1[dat$F2__1 == 2] <- 5
dat$F2__1[dat$F2__1 == 3] <- 2
dat$F2__1[dat$F2__1 == 5] <- 3
dat$A2__3[dat$A2__3 == 2] <- 5
dat$A2__3[dat$A2__3 == 3] <- 2
dat$A2__3[dat$A2__3 == 5] <- 3

Ergebnis$Tabelle[["rueck"]] <- 
  dat %>% 
  filter(!is.na(A2__3)) %>%
  filter(!(F2__1 == 4 | A2__3 == 4)) %>%
  mutate(rueck = 
           case_when(
             A2__3 > F2__1 ~ "Mehr Homeoffice",   
             A2__3 == F2__1 ~ "Konstanz",
             A2__3 < F2__1 ~ "Weniger Homeoffice",
           )) %>%
  mutate(gewicht = Faktor__3) %>%
  select(rueck, strata, gewicht) %>%
  gather(key = Variable, value = Ausprägung, -rueck, -gewicht) %>%
  group_by(Variable, Ausprägung, rueck) %>%
  summarise(weighted_n = sum(gewicht),
            n=sum(!is.na(rueck))) %>%
  mutate(N = sum(n),
         weighted_group_size = sum(weighted_n),
         weighted_estimate = weighted_n/weighted_group_size) %>%
  select(Variable, Ausprägung, rueck, weighted_estimate, N) %>%
  spread(rueck, weighted_estimate) %>%
  filter(!(is.na(Ausprägung)))

write.csv2(Ergebnis$Tabelle[["rueck"]], "./Output/Rückkehr.csv")

Ergebnis$Tabelle[["bleib"]] <- 
  dat %>% 
  filter(!is.na(A2__3)) %>%
  filter(F2__1 == 3) %>%
  filter(!(F2__1 == 4 | A2__3 == 4)) %>%
  mutate(bleib = 
           if_else(F2__1 == 3 & A2__3 %in% c(1,2),  "zurück in den Betrieb", "im Homeoffice geblieben")
           ) %>%
  mutate(gewicht = Faktor__3) %>%
  select(bleib, strata, gewicht) %>%
  gather(key = Variable, value = Ausprägung, -bleib, -gewicht) %>%
  group_by(Variable, Ausprägung, bleib) %>%
  summarise(weighted_n = sum(gewicht),
            n=sum(!is.na(bleib))) %>%
  mutate(N = sum(n),
         weighted_group_size = sum(weighted_n),
         weighted_estimate = weighted_n/weighted_group_size) %>%
  select(Variable, Ausprägung, bleib, weighted_estimate, N) %>%
  spread(bleib, weighted_estimate) %>%
  filter(!(is.na(Ausprägung)) & Variable == "BR")

write.csv2(Ergebnis$Tabelle[["bleib"]], "./Output/Geblieben.csv")

  dat %>% 
  filter(!is.na(A2__3) & F32__1 > 3) %>%
  filter(F2__1 == 3) %>%
  filter(!(F2__1 == 4 | A2__3 == 4)) %>%
  mutate(bleib = 
           if_else(F2__1 == 3 & A2__3 %in% c(1,2),  "zurück in den Betrieb", "im Homeoffice geblieben")
  ) %>%
  mutate(gewicht = Faktor__3) %>%
  select(bleib, strata, gewicht) %>%
  gather(key = Variable, value = Ausprägung, -bleib, -gewicht) %>%
  group_by(Variable, Ausprägung, bleib) %>%
  summarise(weighted_n = sum(gewicht),
            n=sum(!is.na(bleib))) %>%
  mutate(N = sum(n),
         weighted_group_size = sum(weighted_n),
         weighted_estimate = weighted_n/weighted_group_size) %>%
  select(Variable, Ausprägung, bleib, weighted_estimate, N) %>%
  spread(bleib, weighted_estimate) %>%
  filter(!(is.na(Ausprägung)))
