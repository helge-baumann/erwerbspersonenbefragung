dat <- 
  dat %>%
  mutate(
    bildung_beruf = case_when(
      S9_1b__5 == 1 ~ "kein Abschluss",
      S9_1__5 == 1 ~ "Berufsausbildung, Berufsfachschule",
      S9_4__5 == 1 | S9_2__5 == 1 | S9_3__5 == 1 ~ "Bachelor, Meister oder Fachschule",
      S9_5__5 == 1 ~ "Master", 
      S9_6__5 == 1 ~ "Promotion",
      S9_7__5 == 1 ~ "Sonstiges"
      ),
    ostwest = case_when(
        S4__6 %in% 1:11 ~ "West", S4__6 %in% 12:16 ~ "Ost"
      ),
    einkommen_g = cut(hh_income_aeq__5,
                      breaks=c(-1, 1500, 2000, 2500, 1000000), 
                      labels=c("0 bis 1.499 Euro", "1500 bis 1.999 Euro", "2.000 bis 2.499 Euro", "2.500 Euro und mehr"), right=F),
    afd=as_factor(case_when(W6_2__6==7 ~ "AfD", W6_2__6 %in% 1:6 ~ "andere Parteien im Bundestag"))
    ) %>%
  pivot_wider(cols=c(bildung_beruf, einkommen_g), names=to="Variable", values_to="Wert") %>%
  group_by(Variable, Wert, afd, ostwest) %>%
  summarise(n=sum(Faktor_w123456), N=n())
  
