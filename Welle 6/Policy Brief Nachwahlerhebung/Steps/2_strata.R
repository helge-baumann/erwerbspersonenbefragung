# Auswertungsschichten generieren
dat <- 
  dat %>%
  group_by(ID) %>%
  arrange(ID, Welle) %>%
  # keine nachgezogenen Selbstständigen
  filter(ziehung_w5[5] == 0) %>%
  # Übertrag aus anderen Wellen (Längsschnitt)
  mutate(
    einkommensverlust_hh = as_factor(
      labelled(add_hh_w1235[5], c("nein" = 0, "ja" = 1))
      ),
    gewerkschaft = as_factor(S15_w16),
    `(1) gesamt` = as_factor(""),
    geschlecht = as_factor(S2_w123456),
    bildung = as_factor(S3_w123456), 
    wahllokal = as_factor(
      case_when(W6_SC3_w6 == 1 ~ "Briefwahl", W6_SC3_w6 == 2 ~ "Wahllokal")),
    bundesland = as_factor(S4_w16), 
    befristet = as_factor(
      labelled(
        case_when(!is.na(D6_1_w5[5]) & D6_1_w5[5] < 99 ~ D6_1_w5[5]), 
        c("nein" = 2, "ja" = 1))),
    sv_beschaeftigt = as_factor(
      labelled(
        case_when(!is.na(D6_2_w5[5]) & D6_2_w5[5] < 99 ~ D6_2_w5[5]), 
        c("nein" = 2, "ja" = 1))),
    teilzeit = as_factor(
      labelled(
        case_when(!is.na(D6_3_w5[5]) & D6_3_w5[5] < 99 ~ D6_3_w5[5]), 
        c("nein" = 2, "ja" = 1))),
    leiharbeit = as_factor(
      labelled(
        case_when(!is.na(D6_4_w5[5]) & D6_4_w5[5] < 99 ~ D6_4_w5[5]), 
        c("nein" = 2, "ja" = 1))),
    werkvertrag = as_factor(
      labelled(
        case_when(!is.na(D6_5_w5[5]) & D6_5_w5[5] < 99 ~ D6_5_w5[5]), 
        c("nein" = 2, "ja" = 1))),
    minijob = as_factor(
      labelled(
        case_when(!is.na(D6_6_w5[5]) & D6_6_w5[5] < 99 ~ D6_6_w5[5]), 
        c("nein" = 2, "ja" = 1))),
    selbststaendig = as_factor(
      labelled(
        case_when(
          !is.na(S6_w45[5]) & S6_w45[5] %in% 4:6 ~ 1, 
          !is.na(S6_w45[4]) & is.na(S6_w45[5]) & S6_w45[4] %in% 4:6 ~ 1, 
          !is.na(S6_w45[5]) & S6_w45[5] %in% 1:3 ~ 2, 
          !is.na(S6_w45[4]) & is.na(S6_w45[5]) & S6_w45[4] %in% 1:3 ~ 2), 
        c("nein" = 2, "ja" = 1))),
    
    migration = as_factor(S16_w1)[1],
    erwerbsstatus = as_factor(SC1_w123456),
    partei_5 = as_factor(W5_S12_w5[5]),
    zufriedenheit_regierung = case_when(!is.na(F12_w1235) & F12_w1235 < 99 ~ F12_w1235),
    zufriedenheit_regierung = mean(zufriedenheit_regierung, na.rm=T),
    einschraenkung_grundrechte = case_when(!is.na(F18_w1235) & F18_w1235 < 99 ~ F18_w1235),
    einschraenkung_grundrechte = mean(einschraenkung_grundrechte, na.rm=T),
    sorgen_wirtschaft = case_when(!is.na(F29_2_w1235) & F29_2_w1235 < 99 ~ F29_2_w1235),
    sorgen_wirtschaft = mean(sorgen_wirtschaft, na.rm=T),
    sorgen_gesundheit = case_when(!is.na(F29_3_w1235) & F29_3_w1235 < 99 ~ F29_3_w1235),
    sorgen_gesundheit = mean(sorgen_gesundheit, na.rm=T),
    sorgen_familie = case_when(!is.na(F29_4_w1235) & F29_4_w1235 < 99 ~ F29_4_w1235),
    sorgen_familie = mean(sorgen_familie, na.rm=T),
    sorgen_zusammenhalt = case_when(!is.na(F29_5_w1235) & F29_5_w1235 < 99 ~ F29_5_w1235),
    sorgen_zusammenhalt = mean(sorgen_zusammenhalt, na.rm=T),
    sorgen_ungleichheit = case_when(!is.na(C12_6_w235) & C12_6_w235 < 99 ~ C12_6_w235),
    sorgen_ungleichheit = mean(sorgen_ungleichheit, na.rm=T),
    nettoeinkommen = cut(
      hh_income_aeq_w12345[5], 
      breaks=c(0, 1500, 2000, 2500, 10000),
      labels=c("bis unter 1.500 Euro", "1.500 bis unter 2.000 Euro", 
               "2.000 bis 2.500 Euro", "2.500 Euro und mehr")
    ), 
    nettoeinkommen_ka = case_when(!is.na(nettoeinkommen) ~ as.character(nettoeinkommen), D4_w35[5] == 99 ~ "keine Angabe"),
    nettoeinkommen_vor = cut(
      hh_income_vor_aeq_w5[5], 
      breaks=c(0, 1500, 2000, 2500, 10000),
      labels=c("bis unter 1.500 Euro", "1.500 bis unter 2.000 Euro", 
               "2.000 bis 2.500 Euro", "2.500 Euro und mehr")
    ),
    nettoeinkommen_vor_ka = case_when(!is.na(nettoeinkommen_vor) ~ as.character(nettoeinkommen_vor), W5_D4a_w5[5] == 99 ~ "keine Angabe"),
    betroffenheit_corona = 
      as_factor(
        labelled(
          case_when(
            Welle == 6 & !is.na(C13_w35[5]) & C13_w35[5] != 99 ~ C13_w35[5],
            Welle == 6 & !is.na(C13_w35[3]) & C13_w35[3] != 99 & is.na(C13_w35[5]) ~ C13_w35[3],
            ), 
          c("nein" = 2, "ja" = 1)
      )),
    impfung = 
      as_factor(
        case_when(
          W5_C14_w5[5] %in% 1:3 | (W5_C14_w5[5] == 4 & W5_C15_w5[5] == 1) ~ "Geimpft / klare Impfabsicht",
          W5_C14_w5[5] == 4 & W5_C15_w5[5] %in% 2:3 ~ "Ungeimpft, unentschlossen",
          W5_C14_w5[5] == 4 & W5_C15_w5[5] == 4 ~ "Entschlossene Impfverweigerung"
        )
      ),
    taetigkeit = as_factor(case_when(!is.na(S6_w45[5]) ~ S6_w45[5], is.na(S6_w45[5]) ~ S6_w45[4]))
  ) %>%
  ungroup() %>%
  # Gruppen bilden
  mutate(
         alter = cut(
           S1_w123456, 
           breaks=c(0, 30, 45, 60, 100), 
           labels=c("unter 30", "30 bis unter 45", "45 bis unter 60", "60+")
         ),
         zufriedenheit_regierung = cut(
           zufriedenheit_regierung,
           breaks=c(0, 1.8, 2.4, 3, 5), 
           labels= c("Im Schnitt sehr zufrieden", "im Schnitt zufrieden", 
                     "im Schnitt weniger zufrieden", "im Schnitt gar nicht zufrieden")
         ),
         einschraenkung_grundrechte = cut(
           einschraenkung_grundrechte,
           breaks=c(0, 4, 7, 9, 12), 
           labels= c("unberechtigt", "eher unberechtigt", 
                     "berechtigt", "absolut berechtigt")
         ),
         sorgen_wirtschaft = cut(
           sorgen_wirtschaft,
           breaks=c(1, 1.75, 2.25, 4), 
           labels= c("Große Sorgen", "Sorgen", "eher keine Sorgen")
         ),
         sorgen_gesundheit = cut(
           sorgen_gesundheit,
           breaks=c(1, 1.75, 2.25, 4), 
           labels= c("Große Sorgen", "Sorgen", "eher keine Sorgen")
         ),
         sorgen_familie = cut(
           sorgen_familie,
           breaks=c(1, 1.75, 2.25, 4), 
           labels= c("Große Sorgen", "Sorgen", "eher keine Sorgen")
         ),
         sorgen_zusammenhalt = cut(
           sorgen_zusammenhalt,
           breaks=c(1, 1.75, 2.25, 4), 
           labels= c("Große Sorgen", "Sorgen", "eher keine Sorgen")
         ),
         sorgen_ungleichheit = cut(
           sorgen_ungleichheit,
           breaks=c(1, 1.75, 2.25, 4), 
           labels= c("Große Sorgen", "Sorgen", "eher keine Sorgen")
         )) %>%
  filter(!is.na(S1_w123456) & Welle == 6)
