# (Verluste)

dat %>%
  filter(Welle == 5 & is.na(Aufstockung_w5)) %>%
  mutate(
    alle="gesamt", 
    einkommen_vor = 
           cut(
             hh_income_vor_aeq_w5,
             breaks=c(0, 1250, 2000, 2750, 100000), 
             labels=c("< 1250", "1250 bis 2000", "2000 bis 2750", "2750 und mehr")),
         verlust= hh_income_aeq_w12345-hh_income_vor_aeq_w5, 
         verlust_proz = (hh_income_aeq_w12345-hh_income_vor_aeq_w5)/hh_income_vor_aeq_w5*100
         ) %>%
  pivot_longer(c(alle, einkommen_vor), names_to="Variable", values_to="Wert") %>%
  group_by(Variable, Wert) %>%
  summarise(
    N=n(), 
    `Durchschnittlich vor der Krise` = wtd.mean(hh_income_vor_aeq_w5, Faktor_w123456),
      `Durchschnittlich Juli 2021` =  wtd.mean(hh_income_aeq_w12345, Faktor_w123456),
      `Durchschnittlicher Gewinn` =  wtd.mean(verlust, Faktor_w123456),
      `Durchschnittlicher Gewinn in %` =  wtd.mean(verlust_proz, Faktor_w123456)
  ) %>%
  filter(!is.na(Wert)) %>%
  ungroup() %>%
  select(-Variable) %>%
  write.csv2("Output/Einkommensgewinne.csv")

dat %>%
  filter(Welle == 5 & is.na(Aufstockung_w5)) %>%
  mutate(
    alle="gesamt", 
    einkommen_vor = 
      cut(
        hh_income_vor_aeq_w5,
        breaks=c(0, 1250, 2000, 2750, 100000), 
        labels=c("< 1250", "1250 bis 2000", "2000 bis 2750", "2750 und mehr")),
    verlust= hh_income_aeq_w12345-hh_income_vor_aeq_w5, 
    verlust_proz = (hh_income_aeq_w12345-hh_income_vor_aeq_w5)/hh_income_vor_aeq_w5*100
  ) %>%
  pivot_longer(c(alle, einkommen_vor), names_to="Variable", values_to="Wert") %>%
  group_by(Variable, Wert) %>%
  summarise(
    N=n(), 
    `Durchschnittlich vor der Krise` = wtd.mean(hh_income_vor_aeq_w5, Faktor_w123456),
    `Durchschnittlich Juli 2021` =  wtd.mean(hh_income_aeq_w12345, Faktor_w123456),
    `Durchschnittlicher Gewinn` =  wtd.mean(verlust, Faktor_w123456),
    `Durchschnittlicher Gewinn in %` =  wtd.mean(verlust_proz, Faktor_w123456)
  ) %>%
  filter(!is.na(Wert)) %>%
  ungroup() %>%
  select(-Variable)
