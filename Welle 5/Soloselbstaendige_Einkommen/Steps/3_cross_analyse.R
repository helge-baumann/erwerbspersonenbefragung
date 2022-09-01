# (Verluste)

dat %>%
  filter(Welle == 5) %>%
  mutate(alle="gesamt", geschlecht = as_factor(S2_w123456), 
         selbst_solo = 
           case_when(
             S6_w45 %in% c(1:3) ~ "Arbeiter/in, Angestellte/r, Beamte/r",
             S6_w45 %in% c(4:6) & W5_S6h_w5 == 1 ~ "Selbständig ohne Mitarbeiter:innen",
             S6_w45 %in% c(4:6) & W5_S6h_w5 == 2 ~ "Selbständig mit Mitarbeiter:innen",
             S6_w45 %in% c(7, 99) | is.na(S6_w45) ~ NA_character_),
         F9_w1235 =if_else(F9_w1235 == 4 & F8_w1235 %in% 1:3, as.integer(F8_w1235), as.integer(F9_w1235)),
         verlust_i=case_when(F8_w1235 %in% 1 ~ 1, F8_w1235 %in% 2:3 ~ 0),
         verlust_hh=case_when(F9_w1235 %in% 1 ~ 1, F9_w1235 %in% 2:3 ~ 0)
         
  ) %>%
  pivot_longer(c(alle, selbst_solo), names_to="Variable1", values_to="Wert1") %>%
  mutate(alle="gesamt") %>%
  pivot_longer(c(alle, geschlecht), names_to="Variable2", values_to="Wert2") %>%
  group_by(Variable1, Wert1, Variable2, Wert2) %>%
  summarise(
    N=n(), 
    `Durchschnittliches Haushaltsäquivalenzeinkommen vor der Krise` =  wtd.mean(hh_income_vor_aeq_w5, Faktor) , 
    `Durchschnittliches Haushaltsäquivalenzeinkommen Juli 2021` =  wtd.mean(hh_income_aeq_w12345, Faktor) , 
    `Durchschnittliches Haushaltseinkommen vor der Krise` =  wtd.mean(hh_income_vor_w5, Faktor) , 
    `Durchschnittliches Haushaltseinkommen Juli 2021` =  wtd.mean(hh_income_w12345, Faktor),
      `Verlust des Individualeinkommens` = wtd.mean(verlust_i, Faktor),
      `Verlust des Haushaltseinkommens` =  wtd.mean(verlust_hh, Faktor)
  ) %>%
  filter(!is.na(Wert1) & Wert2 != "Divers" & !is.na(Wert2)) %>%
  ungroup() %>%
  select(-c(Variable1, Variable2)) %>%
  pivot_wider(names_from=Wert2, 
              values_from=c(N, `Durchschnittliches Haushaltsäquivalenzeinkommen vor der Krise`:
                              `Verlust des Haushaltseinkommens`), 
              id_cols=c(Wert1)) %>%
  write.csv2("Output/Einkommensverluste_test.csv")

