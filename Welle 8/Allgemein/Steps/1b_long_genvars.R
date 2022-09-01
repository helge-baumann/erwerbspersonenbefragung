# Personen im Haushalt----
dat_long_full <- 
  dat_long_full %>% 
  group_by(ID) %>%
  arrange(ID, Welle) %>%
  mutate(
    # Gewicht zum Wellenvergleich: Faktor
    hh_ue18_w1234578 = case_when(
      # Welle 1
      Welle == 1 & S11_1_w1 < 99 ~ S11_1_w1-1,
      # Welle == 1 & S11_1_w1 == 99 ~ -7,
      # Welle 2: immer aus Welle 1 übertragen.
      !is.na(S11_1_w1[1]) & S11_1_w1[1] < 99 & Welle == 2 & !is.na(S1_w12345678) ~ 
        as.double(S11_1_w1[1]-1),
      # Welle 3
      !is.na(S11_1_w1[1]) & S11_1_w1[1] < 99 & Welle == 3 & !is.na(S1_w12345678) &
        S11_1_w3 == 0 ~ as.double(S11_1_w1[1]-1),
      !is.na(S11_1o_w3478) & Welle == 3 ~ as.double(S11_1o_w3478),
      # Welle 4
      !is.na(S11_1o_w3478[3]) & is.na(S11_1o_w3478[4]) & Welle == 4 & !is.na(S1_w12345678)  ~ 
        as.double(S11_1o_w3478[3]),
      !is.na(S11_1_w1[1]) & S11_1_w1[1] < 99 & Welle == 4 & !is.na(S1_w12345678) &
        is.na(S11_1o_w3478) & is.na(S11_1o_w3478[3]) ~ as.double(S11_1_w1[1]-1),
      !is.na(S11_1o_w3478)  & Welle == 4 ~ as.double(S11_1o_w3478),
      # Welle 5
      !is.na(S11_1o_w3478[4]) & is.na(S11_1_w5) & Welle == 5 & !is.na(S1_w12345678)  ~ 
        as.double(S11_1o_w3478[4]),
      !is.na(S11_1o_w3478[3]) &  is.na(S11_1o_w3478[4]) & is.na(S11_1_w5) & Welle == 5 & 
        !is.na(S1_w12345678)  ~ as.double(S11_1o_w3478[3]),
      is.na(S11_1o_w3478[3]) &  is.na(S11_1o_w3478[4]) & !is.na(S11_1_w1[1]) & 
        S11_1_w1[1] < 99 & Welle == 5 & !is.na(S1_w12345678) &
        is.na(S11_1_w5)  ~ as.double(S11_1_w1[1]-1),
      !is.na(S11_1_w5)  & Welle == 5 ~ as.double(S11_1_w5),#
      # Welle 7
        # Übertrag aus Welle 5
      !is.na(S11_1_w5[5]) & Welle == 7 & !is.na(S1_w12345678) & is.na(S11_1o_w3478)  ~ 
        as.double(S11_1_w5[5]),
        # Übertrag aus Welle 4
      !is.na(S11_1o_w3478[4]) & is.na(S11_1_w5[5]) & Welle == 7 & !is.na(S1_w12345678) & is.na(S11_1o_w3478)  ~ 
        as.double(S11_1o_w3478[4]),
        # Übertrag aus Welle 3
      !is.na(S11_1o_w3478[3]) &  is.na(S11_1o_w3478[4]) & is.na(S11_1_w5[5]) & Welle == 7 & 
        !is.na(S1_w12345678) &  is.na(S11_1o_w3478) ~ as.double(S11_1o_w3478[3]),
        # Übertrag aus Welle 1
      is.na(S11_1o_w3478[3]) &  is.na(S11_1o_w3478[4]) & !is.na(S11_1_w1[1]) & 
        S11_1_w1[1] < 99 & Welle == 7  & !is.na(S1_w12345678) &
        is.na(S11_1_w5[5]) & is.na(S11_1o_w3478)  ~ as.double(S11_1_w1[1]-1),
        # kein Übertrag
      !is.na(S11_1o_w3478)  & Welle == 7 ~ as.double(S11_1o_w3478),
      # Welle 8
      # Übertrag aus Welle 7
      !is.na(S11_1_w5[7]) & Welle == 8 & !is.na(S1_w12345678) & is.na(S11_1o_w3478)  ~ 
        as.double(S11_1_w5[7]),
      # Übertrag aus Welle 5
      !is.na(S11_1_w5[5]) & is.na(S11_1_w5[7]) & Welle == 8 & !is.na(S1_w12345678) & is.na(S11_1o_w3478)  ~ 
        as.double(S11_1_w5[5]),
      # Übertrag aus Welle 4
      !is.na(S11_1o_w3478[4]) & is.na(S11_1_w5[5]) & is.na(S11_1_w5[7]) & Welle == 8 & !is.na(S1_w12345678) & is.na(S11_1o_w3478)  ~ 
        as.double(S11_1o_w3478[4]),
      # Übertrag aus Welle 3
      !is.na(S11_1o_w3478[3]) &  is.na(S11_1o_w3478[4]) & is.na(S11_1_w5[5]) & is.na(S11_1_w5[7]) & Welle == 8 &
        !is.na(S1_w12345678) &  is.na(S11_1o_w3478) ~ as.double(S11_1o_w3478[3]),
      # Übertrag aus Welle 1
      is.na(S11_1o_w3478[3]) &  is.na(S11_1o_w3478[4]) & !is.na(S11_1_w1[1]) & 
        S11_1_w1[1] < 99 & is.na(S11_1_w5[7]) & Welle == 8 & !is.na(S1_w12345678) &
        is.na(S11_1_w5[5]) & is.na(S11_1o_w3478)  ~ as.double(S11_1_w1[1]-1),
      # kein Übertrag
      !is.na(S11_1o_w3478)  & Welle == 8 ~ as.double(S11_1o_w3478)
    ),
    # Haushaltsmitglieder zwischen 14 und 18
    hh_ue14u18_w1234578 = case_when(
      # Welle 1
      Welle == 1 & S11_2_w1 < 99 ~ S11_2_w1-1,
      # Welle == 1 & S11_1_w1 == 99 ~ -7,
      # Welle 2
      !is.na(S11_2_w1[1]) & S11_2_w1[1] < 99 & Welle == 2 & !is.na(S1_w12345678) ~ 
        as.double(S11_2_w1[1]-1),
      # Welle 3
      !is.na(S11_2o_w34578) & Welle == 3 ~ as.double(S11_2o_w34578),
      !is.na(S11_2_w1[1]) & S11_2_w1[1] < 99 & Welle == 3 & !is.na(S1_w12345678) &
        S11_2_w3 == 0 & (S11_2_w1[1]-1) > 0 ~ as.double(S11_2_w1[1]-1),
      S11_2_w3 == 0 & (S11_2_w1[1]-1) == 0 ~ 0,
      # Welle 4
      !is.na(S11_2o_w34578)  & Welle == 4 ~ as.double(S11_2o_w34578),
      !is.na(S11_2o_w34578[3]) & is.na(S11_2o_w34578[4]) & Welle == 4 & !is.na(S1_w12345678)  ~ 
        as.double(S11_2o_w34578[3]),
      is.na(S11_2o_w34578[4]) & Welle == 4 & !is.na(S1_w12345678) & S11_2_w3[3] == 0 & (S11_2_w1[1]-1) == 0 ~ 0,
      !is.na(S11_2_w1[1]) & S11_2_w1[1] < 99 & Welle == 4 & !is.na(S1_w12345678) &
        is.na(S11_2o_w34578) & is.na(S11_2o_w34578[3]) ~ as.double(S11_2_w1[1]-1),
      # Welle 5
      !is.na(S11_2o_w34578)  & Welle == 5 ~ as.double(S11_2o_w34578),
      !is.na(S11_2o_w34578[4]) & is.na(S11_2o_w34578) & Welle == 5 & !is.na(S1_w12345678)  ~ 
        as.double(S11_2o_w34578[4]),
      !is.na(S11_2o_w34578[3]) &  is.na(S11_2o_w34578[4]) & is.na(S11_2o_w34578) & Welle == 5 & 
        !is.na(S1_w12345678)  ~ as.double(S11_2o_w34578[3]),
      is.na(S11_2o_w34578[5]) & is.na(S11_2o_w34578[4]) & Welle == 5 & !is.na(S1_w12345678) & 
        S11_2_w3[3] == 0 & (S11_2_w1[1]-1) == 0 ~ 0,
      is.na(S11_2o_w34578[3]) &  is.na(S11_2o_w34578[4]) & !is.na(S11_2_w1[1]) & 
        S11_2_w1[1] < 99 & Welle == 5 & !is.na(S1_w12345678) &
        is.na(S11_2o_w34578)  ~ as.double(S11_2_w1[1]-1),
      # Welle 7
      !is.na(S11_2o_w34578)  & Welle == 7 ~ as.double(S11_2o_w34578),
      !is.na(S11_2o_w34578[5]) & is.na(S11_2o_w34578[7]) & Welle == 7 & !is.na(S1_w12345678)  ~ 
        as.double(S11_2o_w34578[5]),
      !is.na(S11_2o_w34578[4]) & is.na(S11_2o_w34578[5]) & Welle == 7 & !is.na(S1_w12345678) & is.na(S11_2o_w34578[7]) ~ 
        as.double(S11_2o_w34578[4]),
      !is.na(S11_2o_w34578[3]) &  is.na(S11_2o_w34578[4]) & is.na(S11_2o_w34578[5]) & is.na(S11_2o_w34578[7]) & Welle == 7 & 
        !is.na(S1_w12345678)  ~ as.double(S11_2o_w34578[3]),
      is.na(S11_2o_w34578[7]) & is.na(S11_2o_w34578[5]) & is.na(S11_2o_w34578[4]) & Welle == 7 & !is.na(S1_w12345678) & 
        S11_2_w3[3] == 0 & (S11_2_w1[1]-1) == 0 ~ 0,
      is.na(S11_2o_w34578[3]) & is.na(S11_2o_w34578[7]) & is.na(S11_2o_w34578[4]) & is.na(S11_2o_w34578[5]) & !is.na(S11_2_w1[1]) & 
        S11_2_w1[1] < 99 & Welle == 7 & !is.na(S1_w12345678) ~ as.double(S11_2_w1[1]-1),
      # Welle 8
      !is.na(S11_2o_w34578)  & Welle == 8 ~ as.double(S11_2o_w34578),
      !is.na(S11_2o_w34578[7])  & is.na(S11_2o_w34578) & Welle == 8 ~ as.double(S11_2o_w34578[7]),
      !is.na(S11_2o_w34578[5]) & is.na(S11_2o_w34578[7]) & is.na(S11_2o_w34578) & Welle == 8 & !is.na(S1_w12345678)  ~ 
        as.double(S11_2o_w34578[5]),
      !is.na(S11_2o_w34578[4]) & is.na(S11_2o_w34578[5])& is.na(S11_2o_w34578) & Welle == 8 & !is.na(S1_w12345678) & is.na(S11_2o_w34578[7]) ~ 
        as.double(S11_2o_w34578[4]),
      !is.na(S11_2o_w34578[3]) &  is.na(S11_2o_w34578[4]) & is.na(S11_2o_w34578[5]) & is.na(S11_2o_w34578[7]) & is.na(S11_2o_w34578) & Welle == 8 & 
        !is.na(S1_w12345678)  ~ as.double(S11_2o_w34578[3]),
      is.na(S11_2o_w34578[8]) & is.na(S11_2o_w34578[7]) & is.na(S11_2o_w34578[5]) & is.na(S11_2o_w34578[4]) & Welle == 8 & !is.na(S1_w12345678) & 
        S11_2_w3[3] == 0 & (S11_2_w1[1]-1) == 0 ~ 0,
      is.na(S11_2o_w34578[3]) & is.na(S11_2o_w34578[7]) & is.na(S11_2o_w34578[4]) & is.na(S11_2o_w34578[5]) & !is.na(S11_2_w1[1]) & 
        S11_2_w1[1] < 99 & is.na(S11_2o_w34578) & Welle == 8 & !is.na(S1_w12345678) ~ as.double(S11_2_w1[1]-1),
    ),
    # Haushaltsmitglieder unter 14
    hh_u14_w1234578 = case_when(
      # Welle 1
      Welle == 1 & S11_3_w1 < 99 ~ S11_3_w1-1,
      # Welle == 1 & S11_1_w1 == 99 ~ -7,
      # Welle 2
      !is.na(S11_3_w1[1]) & S11_3_w1[1] < 99 & Welle == 2 & !is.na(S1_w12345678) ~ 
        as.double(S11_3_w1[1]-1),
      # Welle 3
      !is.na(S11_3o_w34578) & Welle == 3 ~ as.double(S11_3o_w34578),
      !is.na(S11_3_w1[1]) & S11_3_w1[1] < 99 & Welle == 3 & !is.na(S1_w12345678) &
        S11_3_w3 == 0 & (S11_3_w1[1]-1) > 0 ~ as.double(S11_3_w1[1]-1),
      S11_3_w3 == 0 & (S11_3_w1[1]-1) == 0 ~ 0,
      # Welle 4
      !is.na(S11_3o_w34578)  & Welle == 4 ~ as.double(S11_3o_w34578),
      !is.na(S11_3o_w34578[3]) & is.na(S11_3o_w34578[4]) & Welle == 4 & !is.na(S1_w12345678)  ~ 
        as.double(S11_3o_w34578[3]),
      is.na(S11_3o_w34578[4]) & Welle == 4 & !is.na(S1_w12345678) & S11_3_w3[3] == 0 & (S11_3_w1[1]-1) == 0 ~ 0,
      !is.na(S11_3_w1[1]) & S11_3_w1[1] < 99 & Welle == 4 & !is.na(S1_w12345678) &
        is.na(S11_3o_w34578) & is.na(S11_3o_w34578[3]) ~ as.double(S11_3_w1[1]-1),
      # Welle 5
      !is.na(S11_3o_w34578)  & Welle == 5 ~ as.double(S11_3o_w34578),
      !is.na(S11_3o_w34578[4]) & is.na(S11_3o_w34578) & Welle == 5 & !is.na(S1_w12345678)  ~ 
        as.double(S11_3o_w34578[4]),
      !is.na(S11_3o_w34578[3]) &  is.na(S11_3o_w34578[4]) & is.na(S11_3o_w34578) & Welle == 5 & 
        !is.na(S1_w12345678)  ~ as.double(S11_3o_w34578[3]),
      is.na(S11_3o_w34578[5]) & is.na(S11_3o_w34578[4]) & Welle == 5 & !is.na(S1_w12345678) & S11_3_w3[3] == 0 & 
        (S11_3_w1[1]-1) == 0 ~ 0,
      is.na(S11_3o_w34578[3]) &  is.na(S11_3o_w34578[4]) & !is.na(S11_3_w1[1]) & 
        S11_3_w1[1] < 99 & Welle == 5 & !is.na(S1_w12345678) &
        is.na(S11_3o_w34578)  ~ as.double(S11_3_w1[1]-1),
      # Welle 7
      !is.na(S11_3o_w34578) & Welle == 7 ~ as.double(S11_3o_w34578),
      !is.na(S11_3o_w34578[5])  & Welle == 7 & !is.na(S1_w12345678) &  is.na(S11_3o_w34578) ~ as.double(S11_3o_w34578[5]),
      !is.na(S11_3o_w34578[4]) & is.na(S11_3o_w34578[5]) & is.na(S11_3o_w34578) & Welle == 7 & !is.na(S1_w12345678)  ~ 
        as.double(S11_3o_w34578[4]),
      !is.na(S11_3o_w34578[3]) &  is.na(S11_3o_w34578[4]) & is.na(S11_3o_w34578[5]) & is.na(S11_3o_w34578) & Welle == 7 & !is.na(S1_w12345678) 
      ~ as.double(S11_3o_w34578[3]),
      is.na(S11_3o_w34578[7]) & is.na(S11_3o_w34578[5]) & is.na(S11_3o_w34578[4]) & Welle == 7 & !is.na(S1_w12345678) & S11_3_w3[3] == 0 & 
        (S11_3_w1[1]-1) == 0 ~ 0,
      is.na(S11_3o_w34578[3]) &  is.na(S11_3o_w34578[4]) & is.na(S11_3o_w34578[5]) & is.na(S11_3o_w34578) & !is.na(S11_3_w1[1]) & 
        S11_3_w1[1] < 99 & Welle == 7 & !is.na(S1_w12345678)   ~ as.double(S11_3_w1[1]-1),
      # Welle 8
      !is.na(S11_3o_w34578) & Welle == 8 ~ as.double(S11_3o_w34578),
      !is.na(S11_3o_w34578[7]) & is.na(S11_3o_w34578) & Welle == 8  ~ as.double(S11_3o_w34578[7]),
      !is.na(S11_3o_w34578[5]) & is.na(S11_3o_w34578) & Welle == 8 & !is.na(S1_w12345678) &  is.na(S11_3o_w34578) ~ as.double(S11_3o_w34578[5]),
      !is.na(S11_3o_w34578[4]) & is.na(S11_3o_w34578[5]) & is.na(S11_3o_w34578) & is.na(S11_3o_w34578) & Welle == 8 & !is.na(S1_w12345678)  ~ 
        as.double(S11_3o_w34578[4]),
      !is.na(S11_3o_w34578[3]) &  is.na(S11_3o_w34578[4]) & is.na(S11_3o_w34578[5]) & is.na(S11_3o_w34578) & is.na(S11_3o_w34578) & Welle == 8 & !is.na(S1_w12345678) 
      ~ as.double(S11_3o_w34578[3]),
      is.na(S11_3o_w34578[8]) & is.na(S11_3o_w34578[7]) & is.na(S11_3o_w34578[5]) & is.na(S11_3o_w34578[4]) & Welle == 8 & !is.na(S1_w12345678) & S11_3_w3[3] == 0 & 
        (S11_3_w1[1]-1) == 0 ~ 0,
      is.na(S11_3o_w34578[3]) &  is.na(S11_3o_w34578[4]) & is.na(S11_3o_w34578[5]) & is.na(S11_3o_w34578) & !is.na(S11_3_w1[1]) & 
        S11_3_w1[1] < 99 & is.na(S11_3o_w34578) & Welle == 8 & !is.na(S1_w12345678)   ~ as.double(S11_3_w1[1]-1)
    ),
    # Warnungen
    hh_ue18_flag_w1234578 = case_when(
      # Welle 1
      Welle == 1 & S11_1_w1 < 99 ~ "Originalangabe",
      # Welle 2: immer aus Welle 1 übertragen.
      !is.na(S11_1_w1[1]) & S11_1_w1[1] < 99 & Welle == 2 & !is.na(S1_w12345678) ~ 
        "Übertrag aus Welle 1",
      # Welle 3
      !is.na(S11_1_w1[1]) & S11_1_w1[1] < 99 & Welle == 3 & !is.na(S1_w12345678) &
        S11_1_w3 == 0 ~ "Übertrag aus Welle 1",
      !is.na(S11_1o_w3478) & Welle == 3 ~ "Originalangabe",
      # Welle 4
      !is.na(S11_1o_w3478[3]) & is.na(S11_1o_w3478[4]) & Welle == 4 & !is.na(S1_w12345678)  ~ 
        "Übertrag aus Welle 3",
      !is.na(S11_1_w1[1]) & S11_1_w1[1] < 99 & Welle == 4 & !is.na(S1_w12345678) &
        is.na(S11_1o_w3478) & is.na(S11_1o_w3478[3]) ~ "Übertrag aus Welle 1",
      !is.na(S11_1o_w3478)  & Welle == 4 ~ "Originalangabe",
      # Welle 5
      !is.na(S11_1o_w3478[4]) & is.na(S11_1_w5) & Welle == 5 & !is.na(S1_w12345678)  ~ 
        "Übertrag aus Welle 4",
      !is.na(S11_1o_w3478[3]) &  is.na(S11_1o_w3478[4]) & is.na(S11_1_w5) & Welle == 5 & 
        !is.na(S1_w12345678)  ~ "Übertrag aus Welle 3",
      is.na(S11_1o_w3478[3]) &  is.na(S11_1o_w3478[4]) & !is.na(S11_1_w1[1]) & 
        S11_1_w1[1] < 99 & Welle == 5 & !is.na(S1_w12345678) &
        is.na(S11_1_w5)  ~ "Übertrag aus Welle 1",
      !is.na(S11_1_w5)  & Welle == 5 ~ "Originalangabe",
      # Welle 7
      # Übertrag aus Welle 5
      !is.na(S11_1_w5[5]) & Welle == 7 & !is.na(S1_w12345678) & is.na(S11_1o_w3478)  ~ 
       "Übertrag aus Welle 5",
      # Übertrag aus Welle 4
      !is.na(S11_1o_w3478[4]) & is.na(S11_1_w5[5]) & Welle == 7 & !is.na(S1_w12345678) & is.na(S11_1o_w3478)  ~ 
        "Übertrag aus Welle 4",
      # Übertrag aus Welle 3
      !is.na(S11_1o_w3478[3]) &  is.na(S11_1o_w3478[4]) & is.na(S11_1_w5[5]) & Welle == 7 & 
        !is.na(S1_w12345678) &  is.na(S11_1o_w3478) ~ "Übertrag aus Welle 3",
      # Übertrag aus Welle 1
      is.na(S11_1o_w3478[3]) &  is.na(S11_1o_w3478[4]) & !is.na(S11_1_w1[1]) & 
        S11_1_w1[1] < 99 & Welle == 7  & !is.na(S1_w12345678) &
        is.na(S11_1_w5[5]) & is.na(S11_1o_w3478)  ~ "Übertrag aus Welle 1",
      # kein Übertrag
      !is.na(S11_1o_w3478)  & Welle == 7 ~ "Originalangabe",
      # Welle 8
      # Übertrag aus Welle 7
      !is.na(S11_1_w5[7]) & Welle == 8 & !is.na(S1_w12345678) & is.na(S11_1o_w3478)  ~ 
        "Übertrag aus Welle 7",
      # Übertrag aus Welle 5
      !is.na(S11_1_w5[5]) & is.na(S11_1_w5[7]) & Welle == 8 & !is.na(S1_w12345678) & is.na(S11_1o_w3478)  ~ 
        "Übertrag aus Welle 5",
      # Übertrag aus Welle 4
      !is.na(S11_1o_w3478[4]) & is.na(S11_1_w5[5]) & is.na(S11_1_w5[7]) & Welle == 8 & !is.na(S1_w12345678) & is.na(S11_1o_w3478)  ~ 
        "Übertrag aus Welle 4",
      # Übertrag aus Welle 3
      !is.na(S11_1o_w3478[3]) &  is.na(S11_1o_w3478[4]) & is.na(S11_1_w5[5]) & is.na(S11_1_w5[7]) & Welle == 8 &
        !is.na(S1_w12345678) &  is.na(S11_1o_w3478) ~ "Übertrag aus Welle 3",
      # Übertrag aus Welle 1
      is.na(S11_1o_w3478[3]) &  is.na(S11_1o_w3478[4]) & !is.na(S11_1_w1[1]) & 
        S11_1_w1[1] < 99 & is.na(S11_1_w5[7]) & Welle == 8 & !is.na(S1_w12345678) &
        is.na(S11_1_w5[5]) & is.na(S11_1o_w3478)  ~ "Übertrag aus Welle 1",
      # kein Übertrag
      !is.na(S11_1o_w3478)  & Welle == 8 ~ "Originalangabe"
    ),
    # Haushaltsmitglieder zwischen 14 und 18
    hh_ue14u18_flag_w1234578 = case_when(
      # Welle 1
      Welle == 1 & S11_2_w1 < 99 ~ "Originalangabe",
      # Welle == 1 & S11_1_w1 == 99 ~ -7,
      # Welle 2
      !is.na(S11_2_w1[1]) & S11_2_w1[1] < 99 & Welle == 2 & !is.na(S1_w12345678) ~ 
        "Übertrag aus Welle 1",
      # Welle 3
      !is.na(S11_2o_w34578) & Welle == 3 ~ "Originalangabe",
      !is.na(S11_2_w1[1]) & S11_2_w1[1] < 99 & Welle == 3 & !is.na(S1_w12345678) &
        S11_2_w3 == 0 & (S11_2_w1[1]-1) > 0 ~ "Übertrag aus Welle 1",
      S11_2_w3 == 0 & (S11_2_w1[1]-1) == 0 ~ "Originalangabe",
      # Welle 4
      !is.na(S11_2o_w34578)  & Welle == 4 ~ "Originalangabe",
      !is.na(S11_2o_w34578[3]) & is.na(S11_2o_w34578[4]) & Welle == 4 & !is.na(S1_w12345678)  ~ 
        "Übertrag aus Welle 3",
      is.na(S11_2o_w34578[4]) & Welle == 4 & !is.na(S1_w12345678) & S11_2_w3[3] == 0 & (S11_2_w1[1]-1) == 0 ~ "Übertrag aus Welle 3",
      !is.na(S11_2_w1[1]) & S11_2_w1[1] < 99 & Welle == 4 & !is.na(S1_w12345678) &
        is.na(S11_2o_w34578) & is.na(S11_2o_w34578[3]) ~ "Übertrag aus Welle 1",
      # Welle 5
      !is.na(S11_2o_w34578)  & Welle == 5 ~ "Originalangabe",
      !is.na(S11_2o_w34578[4]) & is.na(S11_2o_w34578) & Welle == 5 & !is.na(S1_w12345678)  ~ 
        "Übertrag aus Welle 4",
      !is.na(S11_2o_w34578[3]) &  is.na(S11_2o_w34578[4]) & is.na(S11_2o_w34578) & Welle == 5 & 
        !is.na(S1_w12345678)  ~ "Übertrag aus Welle 3",
      is.na(S11_2o_w34578[5]) & is.na(S11_2o_w34578[4]) & Welle == 5 & !is.na(S1_w12345678) & 
        S11_2_w3[3] == 0 & (S11_2_w1[1]-1) == 0 ~ "Übertrag aus Welle 3",
      is.na(S11_2o_w34578[3]) &  is.na(S11_2o_w34578[4]) & !is.na(S11_2_w1[1]) & 
        S11_2_w1[1] < 99 & Welle == 5 & !is.na(S1_w12345678) &
        is.na(S11_2o_w34578)  ~ "Übertrag aus Welle 1",
      # Welle 7
      !is.na(S11_2o_w34578)  & Welle == 7 ~ "Originalangabe",
      !is.na(S11_2o_w34578[5]) & is.na(S11_2o_w34578[7]) & Welle == 7 & !is.na(S1_w12345678)  ~ 
        "Übertrag aus Welle 5",
      !is.na(S11_2o_w34578[4]) & is.na(S11_2o_w34578[5]) & Welle == 7 & !is.na(S1_w12345678) & is.na(S11_2o_w34578[7]) ~ 
        "Übertrag aus Welle 4",
      !is.na(S11_2o_w34578[3]) &  is.na(S11_2o_w34578[4]) & is.na(S11_2o_w34578[5]) & is.na(S11_2o_w34578[7]) & Welle == 7 & 
        !is.na(S1_w12345678)  ~ "Übertrag aus Welle 3",
      is.na(S11_2o_w34578[7]) & is.na(S11_2o_w34578[5]) & is.na(S11_2o_w34578[4]) & Welle == 7 & !is.na(S1_w12345678) & 
        S11_2_w3[3] == 0 & (S11_2_w1[1]-1) == 0 ~ "Übertrag aus Welle 3",
      is.na(S11_2o_w34578[3]) & is.na(S11_2o_w34578[7]) & is.na(S11_2o_w34578[4]) & is.na(S11_2o_w34578[5]) & !is.na(S11_2_w1[1]) & 
        S11_2_w1[1] < 99 & Welle == 7 & !is.na(S1_w12345678) ~ "Übertrag aus Welle 1",
      # Welle 8
      !is.na(S11_2o_w34578)  & Welle == 8 ~ "Originalangabe",
      !is.na(S11_2o_w34578[7])  & is.na(S11_2o_w34578) & Welle == 8 ~ "Übertrag aus Welle 7",
      !is.na(S11_2o_w34578[5]) & is.na(S11_2o_w34578[7]) & is.na(S11_2o_w34578) & Welle == 8 & !is.na(S1_w12345678)  ~ 
        "Übertrag aus Welle 5",
      !is.na(S11_2o_w34578[4]) & is.na(S11_2o_w34578[5])& is.na(S11_2o_w34578) & Welle == 8 & !is.na(S1_w12345678) & is.na(S11_2o_w34578[7]) ~ 
       "Übertrag aus Welle 4",
      !is.na(S11_2o_w34578[3]) &  is.na(S11_2o_w34578[4]) & is.na(S11_2o_w34578[5]) & is.na(S11_2o_w34578[7]) & is.na(S11_2o_w34578) & Welle == 8 & 
        !is.na(S1_w12345678)  ~ "Übertrag aus Welle 3",
      is.na(S11_2o_w34578[8]) & is.na(S11_2o_w34578[7]) & is.na(S11_2o_w34578[5]) & is.na(S11_2o_w34578[4]) & Welle == 8 & !is.na(S1_w12345678) & 
        S11_2_w3[3] == 0 & (S11_2_w1[1]-1) == 0 ~ "Übertrag aus Welle 3",
      is.na(S11_2o_w34578[3]) & is.na(S11_2o_w34578[7]) & is.na(S11_2o_w34578[4]) & is.na(S11_2o_w34578[5]) & !is.na(S11_2_w1[1]) & 
        S11_2_w1[1] < 99 & is.na(S11_2o_w34578) & Welle == 8 & !is.na(S1_w12345678) ~ "Übertrag aus Welle 1",
    ),
    # Haushaltsmitglieder unter 14
    hh_u14_flag_w1234578 = case_when(
      # Welle 1
      Welle == 1 & S11_3_w1 < 99 ~ "Originalangabe",
      # Welle == 1 & S11_1_w1 == 99 ~ -7,
      # Welle 2
      !is.na(S11_3_w1[1]) & S11_3_w1[1] < 99 & Welle == 2 & !is.na(S1_w12345678) ~ 
        "Übertrag aus Welle 1",
      # Welle 3
      !is.na(S11_3o_w34578) & Welle == 3 ~ "Originalangabe",
      !is.na(S11_3_w1[1]) & S11_3_w1[1] < 99 & Welle == 3 & !is.na(S1_w12345678) &
        S11_3_w3 == 0 & (S11_3_w1[1]-1) > 0 ~ "Übertrag aus Welle 1",
      S11_3_w3 == 0 & (S11_3_w1[1]-1) == 0 ~ "Originalangabe",
      # Welle 4
      !is.na(S11_3o_w34578)  & Welle == 4 ~ "Originalangabe",
      !is.na(S11_3o_w34578[3]) & is.na(S11_3o_w34578[4]) & Welle == 4 & !is.na(S1_w12345678)  ~ 
        "Übertrag aus Welle 3",
      is.na(S11_3o_w34578[4]) & Welle == 4 & !is.na(S1_w12345678) & S11_3_w3[3] == 0 & (S11_3_w1[1]-1) == 0 ~ "Übertrag aus Welle 3",
      !is.na(S11_3_w1[1]) & S11_3_w1[1] < 99 & Welle == 4 & !is.na(S1_w12345678) &
        is.na(S11_3o_w34578) & is.na(S11_3o_w34578[3]) ~ "Übertrag aus Welle 1",
      # Welle 5
      !is.na(S11_3o_w34578)  & Welle == 5 ~ "Originalangabe",
      !is.na(S11_3o_w34578[4]) & is.na(S11_3o_w34578) & Welle == 5 & !is.na(S1_w12345678)  ~ 
        "Übertrag aus Welle 4",
      !is.na(S11_3o_w34578[3]) &  is.na(S11_3o_w34578[4]) & is.na(S11_3o_w34578) & Welle == 5 & 
        !is.na(S1_w12345678)  ~ "Übertrag aus Welle 3",
      is.na(S11_3o_w34578[5]) & is.na(S11_3o_w34578[4]) & Welle == 5 & !is.na(S1_w12345678) & S11_3_w3[3] == 0 & 
        (S11_3_w1[1]-1) == 0 ~ "Übertrag aus Welle 3",
      is.na(S11_3o_w34578[3]) &  is.na(S11_3o_w34578[4]) & !is.na(S11_3_w1[1]) & 
        S11_3_w1[1] < 99 & Welle == 5 & !is.na(S1_w12345678) &
        is.na(S11_3o_w34578)  ~ "Übertrag aus Welle 1",
      # Welle 7
      !is.na(S11_3o_w34578) & Welle == 7 ~ "Originalangabe",
      !is.na(S11_3o_w34578[5])  & Welle == 7 & !is.na(S1_w12345678) &  is.na(S11_3o_w34578) ~ "Übertrag aus Welle 5",
      !is.na(S11_3o_w34578[4]) & is.na(S11_3o_w34578[5]) & is.na(S11_3o_w34578) & Welle == 7 & !is.na(S1_w12345678)  ~ 
        "Übertrag aus Welle 4",
      !is.na(S11_3o_w34578[3]) &  is.na(S11_3o_w34578[4]) & is.na(S11_3o_w34578[5]) & is.na(S11_3o_w34578) & Welle == 7 & !is.na(S1_w12345678) 
      ~ "Übertrag aus Welle 3",
      is.na(S11_3o_w34578[7]) & is.na(S11_3o_w34578[5]) & is.na(S11_3o_w34578[4]) & Welle == 7 & !is.na(S1_w12345678) & S11_3_w3[3] == 0 & 
        (S11_3_w1[1]-1) == 0 ~ "Übertrag aus Welle 3",
      is.na(S11_3o_w34578[3]) &  is.na(S11_3o_w34578[4]) & is.na(S11_3o_w34578[5]) & is.na(S11_3o_w34578) & !is.na(S11_3_w1[1]) & 
        S11_3_w1[1] < 99 & Welle == 7 & !is.na(S1_w12345678)   ~ "Übertrag aus Welle 1",
      # Welle 8
      !is.na(S11_3o_w34578) & Welle == 8 ~ "Originalangabe",
      !is.na(S11_3o_w34578[7]) & is.na(S11_3o_w34578) & Welle == 8  ~ "Übertrag aus Welle 7",
      !is.na(S11_3o_w34578[5]) & is.na(S11_3o_w34578) & Welle == 8 & !is.na(S1_w12345678) &  is.na(S11_3o_w34578) ~ "Übertrag aus Welle 5",
      !is.na(S11_3o_w34578[4]) & is.na(S11_3o_w34578[5]) & is.na(S11_3o_w34578) & is.na(S11_3o_w34578) & Welle == 8 & !is.na(S1_w12345678)  ~ 
        "Übertrag aus Welle 4",
      !is.na(S11_3o_w34578[3]) &  is.na(S11_3o_w34578[4]) & is.na(S11_3o_w34578[5]) & is.na(S11_3o_w34578) & is.na(S11_3o_w34578) & Welle == 8 & !is.na(S1_w12345678) 
      ~ "Übertrag aus Welle 3",
      is.na(S11_3o_w34578[8]) & is.na(S11_3o_w34578[7]) & is.na(S11_3o_w34578[5]) & is.na(S11_3o_w34578[4]) & Welle == 8 & !is.na(S1_w12345678) & S11_3_w3[3] == 0 & 
        (S11_3_w1[1]-1) == 0 ~ "Übertrag aus Welle 3",
      is.na(S11_3o_w34578[3]) &  is.na(S11_3o_w34578[4]) & is.na(S11_3o_w34578[5]) & is.na(S11_3o_w34578) & !is.na(S11_3_w1[1]) & 
        S11_3_w1[1] < 99 & is.na(S11_3o_w34578) & Welle == 8 & !is.na(S1_w12345678)   ~ "Übertrag aus Welle 1"
    )
  ) # Neuerung: Die Nuller bei den Mitglidern unter 18 in Welle 3 aufgrund fehlender Angaben auch auf Folgewellen übertragen!

# Haushaltseinkommen
dat_long_full <- 
  dat_long_full %>%
  ungroup() %>%
  mutate(
    oecd_weight_w1234578 = 1+(hh_ue18_w1234578+hh_ue14u18_w1234578-1)*0.5+hh_u14_w1234578*0.3,
    # Klassenmittelwerte (Achtung: Welle 1 kein Individualeinkommen)
    hh_income_w1234578 = case_when(
      S10_w1 == 1 | (D4_w2 == 1 | (D4_w2 == 99 & D2_w2 == 1)) | 
        (D4_w3578 == 1 | (D4_w3578 == 99 & D2_w3578 == 1)) ~ 400,
      S10_w1 == 2 | (D4_w2 == 2 | (D4_w2 == 99 & D2_w2 == 2)) | 
        (D4_w3578 == 2 | (D4_w3578 == 99 & D2_w3578 == 2)) ~ 700,
      S10_w1 == 3 | (D4_w2 == 3 | (D4_w2 == 99 & D2_w2 == 3)) | 
        (D4_w3578 == 3 | (D4_w3578 == 99 & D2_w3578 == 3)) ~ 1100,
      S10_w1 == 4 | (D4_w2 == 4 | (D4_w2 == 99 & D2_w2 == 4)) | 
        (D4_w3578 == 4 | (D4_w3578 == 99 & D2_w3578 == 4)) ~ 1400,
      S10_w1 == 5 | (D4_w2 == 5 | (D4_w2 == 99 & D2_w2 == 5)) | 
        (D4_w3578 == 5 | (D4_w3578 == 99 & D2_w3578 == 5)) ~ 1600,
      S10_w1 == 6 | (D4_w2 == 6 | (D4_w2 == 99 & D2_w2 == 6)) | 
        (D4_w3578 == 6 | (D4_w3578 == 99 & D2_w3578 == 6)) ~ 1850,
      S10_w1 == 7 | (D4_w2 == 7 | (D4_w2 == 99 & D2_w2 == 7)) | 
        (D4_w3578 == 7 | (D4_w3578 == 99 & D2_w3578 == 7)) ~ 2300,
      S10_w1 == 8 | (D4_w2 == 8 | (D4_w2 == 99 & D2_w2 == 8)) | 
        (D4_w3578 == 8 | (D4_w3578 == 99 & D2_w3578 == 8)) ~ 2900,
      S10_w1 == 9 | (D4_w2 == 9 | (D4_w2 == 99 & D2_w2 == 9))  ~ 3850, 
      (D4_w3578 == 9 | (D4_w3578 == 99 & D2_w3578 == 9)) ~ 3500,
      S10_w1 == 10 ~ 5750,
      (D4_w2 == 10 | (D4_w2 == 99 & D2_w2 == 10)) |
        (D4_w3578 == 11 | (D4_w3578 == 99 & D2_w3578 == 11)) ~ 5250,
      (D4_w3578 == 10 | (D4_w3578 == 99 & D2_w3578 == 10)) ~ 4150,
      (D4_w2 == 11 | (D4_w2 == 99 & D2_w2 == 11)) |
        (D4_w3578 == 12 | (D4_w3578 == 99 & D2_w3578 == 12)) ~ 6600
     ), # KLassengrenzen
    hh_income_vor_w5 = 
      case_when(
          (W5_D4a_w5 == 1 | (W5_D4a_w5 == 99 & W5_D2a_w5 == 1)) ~ 400,
          (W5_D4a_w5 == 2 | (W5_D4a_w5 == 99 & W5_D2a_w5 == 2)) ~ 700,
          (W5_D4a_w5 == 3 | (W5_D4a_w5 == 99 & W5_D2a_w5 == 3)) ~ 1100,
          (W5_D4a_w5 == 4 | (W5_D4a_w5 == 99 & W5_D2a_w5 == 4)) ~ 1400,
          (W5_D4a_w5 == 5 | (W5_D4a_w5 == 99 & W5_D2a_w5 == 5)) ~ 1600,
          (W5_D4a_w5 == 6 | (W5_D4a_w5 == 99 & W5_D2a_w5 == 6)) ~ 1850,
          (W5_D4a_w5 == 7 | (W5_D4a_w5 == 99 & W5_D2a_w5 == 7)) ~ 2300,
          (W5_D4a_w5 == 8 | (W5_D4a_w5 == 99 & W5_D2a_w5 == 8)) ~ 2900,
        (W5_D4a_w5 == 9 | (W5_D4a_w5 == 99 & W5_D2a_w5 == 9)) ~ 3500,
        (W5_D4a_w5 == 11 | (W5_D4a_w5 == 99 & W5_D2a_w5 == 11)) ~ 4150,
          (W5_D4a_w5 == 11 | (W5_D4a_w5 == 99 & W5_D2a_w5 == 11)) ~ 5250,
          (W5_D4a_w5 == 12 | (W5_D4a_w5 == 99 & W5_D2a_w5 == 12)) ~ 6600
      ),
    hh_income_aeq_w1234578 = hh_income_w1234578/oecd_weight_w1234578,
    hh_income_vor_aeq_w5 = hh_income_vor_w5/oecd_weight_w1234578
  )

# Additiver Index

cum.na <- function(x) {
  x[which(is.na(x))] <- 0
  return(cumsum(x))
}

dat_long_full <- 
  dat_long_full %>%
  group_by(ID) %>%
  arrange(ID, Welle) %>%
  mutate(
    v_i = case_when(
      F8_w12357 == 1 ~ 1,
      F8_w12357 %in% 2:3 ~ 0,
      F8_w12357 == 99 ~ NA_real_,
      is.na(F8_w12357) ~ NA_real_
    ),
    v_hh = case_when(
      F9_w12357 == 1 ~ 1,
      F9_w12357 %in% 2:3 ~ 0,
      F9_w12357 == 99 ~ NA_real_,
      is.na(F9_w12357) ~ NA_real_
    ),
    v_hh = if_else(F9_w12357 == 4, v_i, v_hh),
    add_i = cumsum(v_i),
    add_i = if_else(add_i > 1, 1, as.double(add_i)),
    add_i_w12357 = case_when(
      !is.na(add_i) ~ as.double(add_i),
      Welle == 2 & !is.na(S1_w12345678) & (v_i[1] == 1| v_i[2] == 1) ~ 1,
      Welle == 3 & !is.na(S1_w12345678) & (v_i[1] == 1 | v_i[2] == 1 | v_i[3] == 1) ~ 1,
      Welle == 5 & ziehung_w5 == 0 & !is.na(S1_w12345678) & (v_i[1] == 1 | v_i[2] == 1 | v_i[3] == 1 | v_i[5] == 1) ~ 1,
      Welle == 5 & ziehung_w5 == 0 & !is.na(S1_w12345678) & v_i[5] == 0 & v_i[1] == 0 & v_i[2] == 0 & v_i[3] == 0 ~ 0,
      Welle == 7 & ziehung_w5[5] == 0 & !is.na(S1_w12345678) & (v_i[1] == 1 | v_i[2] == 1 | v_i[3] == 1 | v_i[5] == 1 | v_i[7] == 1) ~ 1,
      Welle == 7 & ziehung_w5[5] == 0 & !is.na(S1_w12345678) & v_i[7] == 0 & v_i[5] == 0 & v_i[1] == 0 & v_i[2] == 0 & v_i[3] == 0 ~ 0,
      Welle == 5 & ziehung_w5 == 1 ~ as.double(v_i),
      Welle == 7 & ziehung_w5[5] == 1 & (v_i[5] == 1 | v_i[7] == 1) ~ 1,
      Welle == 7 & ziehung_w5[5] == 1 & v_i[5] == 0 & v_i[7] == 0 ~ 0
      
    ),
    add_hh = cumsum(v_hh),
    add_hh = if_else(add_hh > 1, 1, as.double(add_hh)),
    add_hh_w12357 = case_when(
      !is.na(add_hh) ~ as.double(add_hh),
      Welle == 2 & !is.na(S1_w12345678) & (v_hh[1] == 1| v_hh[2] == 1) ~ 1,
      Welle == 3 & !is.na(S1_w12345678) & (v_hh[1] == 1 | v_hh[2] == 1 | v_hh[3] == 1) ~ 1,
      Welle == 5 & ziehung_w5 == 0 & !is.na(S1_w12345678) & (v_hh[1] == 1 | v_hh[2] == 1 | v_hh[3] == 1 | v_hh[5] == 1) ~ 1,
      Welle == 5 & ziehung_w5 == 0 & !is.na(S1_w12345678) & v_hh[5] == 0 & v_hh[1] == 0 & v_hh[2] == 0 & v_hh[3] == 0 ~ 0,
      Welle == 7 & ziehung_w5[5] == 0 & !is.na(S1_w12345678) & (v_hh[1] == 1 | v_hh[2] == 1 | v_hh[3] == 1 | v_hh[5] == 1 | v_hh[7] == 1) ~ 1,
      Welle == 7 & ziehung_w5[5] == 0 & !is.na(S1_w12345678) & v_hh[7] == 0 & v_hh[5] == 0 & v_hh[1] == 0 & v_hh[2] == 0 & v_hh[3] == 0 ~ 0,
      Welle == 5 & ziehung_w5 == 1 ~ as.double(v_hh),
      Welle == 7 & ziehung_w5[5] == 1 & (v_hh[5] == 1 | v_hh[7] == 1) ~ 1,
      Welle == 7 & ziehung_w5[5] == 1 & v_hh[5] == 0 & v_hh[7] == 0 ~ 0
      
    )
  ) %>%
  select(-c(v_i, v_hh, add_i, add_hh))

# Labels
attributes(dat_long_full$hh_ue18_w1234578)$label <- 
  "Personen über 18 Jahre (ab Welle 5: 'ab' 18 Jahren)"
attributes(dat_long_full$hh_ue14u18_w1234578)$label <- 
  "Personen über 14 Jahre (ab Welle 3: 'ab' 14 Jahren) und unter 18 Jahren"
attributes(dat_long_full$hh_u14_w1234578)$label <- 
  "Personen bis 14 Jahre (ab Welle 3: 'unter' 14 Jahren)"
attributes(dat_long_full$hh_ue18_flag_w1234578)$label <- 
  "Quelle der Variable hh_ue18"
attributes(dat_long_full$hh_ue14u18_flag_w1234578)$label <- 
  "Quelle der Variable hh_ue14u18"
attributes(dat_long_full$hh_u14_flag_w1234578)$label <- "Quelle der Variable hh_u14"
attributes(dat_long_full$oecd_weight_w1234578)$label <- "OECD-Bedarfsgewicht"
attributes(dat_long_full$add_i_w12357)$label <- "Kumulativer Index: Bisher Verlust durch Krise (Individualeinkommen)"
attributes(dat_long_full$add_hh_w12357)$label <- "Kumulativer Index: Bisher Verlust durch Krise (Haushaltseinkommen)"
attributes(dat_long_full$hh_income_w1234578)$label <- "Haushaltseinkommen (Klassenmittelwerte)"
attributes(dat_long_full$hh_income_vor_w5)$label <- "Haushaltseinkommen vor der Krise (Klassenmittelwerte)"
attributes(dat_long_full$hh_income_aeq_w1234578)$label <- "Haushaltsnettoäquivalenzeinkommen"
attributes(dat_long_full$hh_income_vor_aeq_w5)$label <- "Haushaltsnettoäquivalenzeinkommen"

attributes(dat_long_full$add_i_w12357)$labels <- 
  setNames(c(0,1), c("bisher kein Verlust durch Krise", "bereits Verlust durch Krise")) 

attributes(dat_long_full$add_hh_w12357)$labels <- 
  setNames(c(0,1), c("bisher kein Verlust durch Krise", "bereits Verlust durch Krise"))

# Quelle der Stichprobe
dat_long_full <- 
  dat_long_full %>% 
  group_by(ID) %>% 
  mutate(Stichprobe_w5 = 
           case_when(
             Aufstockung_w5[5] == 1 ~ "Aufstockungsstichprobe", 
                     is.na(Aufstockung_w5)[5] ~ "Basisstichprobe")
  )

attributes(dat_long_full$Stichprobe_w5)$label <- "Quelle der Stichprobenziehung"
