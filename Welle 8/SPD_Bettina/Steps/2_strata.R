# Auswertungsschichten generieren
dat <-
  dat %>%
  group_by(ID) %>%
  arrange(ID, Welle) %>%
  # Übertrag aus anderen Wellen (Längsschnitt)
  mutate(
    migration = as_factor(S16_w1)[1],
    partei_5 = as_factor(W5_S12_w57[5]),
    bundesland = as_factor(S4_w16[1]),
  ) %>%
  ungroup() %>%
  # Gruppen bilden
  mutate(
    alter = cut(
      S1_w12345678,
      breaks = c(0, 30, 45, 60, 100),
      labels = c("unter 30", "30 bis unter 45", "45 bis unter 60", "60+")
    ),
    erwerbsstatus = as_factor(SC1_w12345678),
    geschlecht = as_factor(S2_w12345678),
    bildung = as_factor(S3_w12345678),
    nettoeinkommen = cut(
      hh_income_aeq_w1234578,
      breaks = c(0, 1500, 2000, 2500, 10000),
      labels = c(
        "bis unter 1.500 Euro", "1.500 bis unter 2.000 Euro",
        "2.000 bis 2.500 Euro", "2.500 Euro und mehr"
      )
    )
  ) %>%
  filter(!is.na(S1_w12345678) & Welle == 8)
