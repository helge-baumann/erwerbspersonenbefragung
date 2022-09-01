# Personen im Haushalt----
dat_long_full <- 
  dat_long_full %>% 
  group_by(ID) %>%
  arrange(ID, Welle) %>%
  mutate(
    # Gewicht zum Wellenvergleich: Faktor
    hh_ue18_w12345 = case_when(
      # Welle 1
      Welle == 1 & S11_1_w1 < 99 ~ S11_1_w1-1,
      # Welle == 1 & S11_1_w1 == 99 ~ -7,
      # Welle 2
      !is.na(S11_1_w1[1]) & S11_1_w1[1] < 99 & Welle == 2 & !is.na(S1_w123456) ~ 
        as.double(S11_1_w1[1]-1),
      # Welle 3
      !is.na(S11_1_w1[1]) & S11_1_w1[1] < 99 & Welle == 3 & !is.na(S1_w123456) &
        S11_1_w3 == 0 ~ as.double(S11_1_w1[1]-1),
      !is.na(S11_1o_w34) & Welle == 3 ~ as.double(S11_1o_w34),
      # Welle 4
      !is.na(S11_1o_w34[3]) & is.na(S11_1o_w34[4]) & Welle == 4 & !is.na(S1_w123456)  ~ 
        as.double(S11_1o_w34[3]),
      !is.na(S11_1_w1[1]) & S11_1_w1[1] < 99 & Welle == 4 & !is.na(S1_w123456) &
        is.na(S11_1o_w34) & is.na(S11_1o_w34[3]) ~ as.double(S11_1_w1[1]-1),
      !is.na(S11_1o_w34)  & Welle == 4 ~ as.double(S11_1o_w34),
      # Welle 5
      !is.na(S11_1o_w34[4]) & is.na(S11_1_w5) & Welle == 5 & !is.na(S1_w123456)  ~ 
        as.double(S11_1o_w34[4]),
      !is.na(S11_1o_w34[3]) &  is.na(S11_1o_w34[4]) & is.na(S11_1_w5) & Welle == 5 & 
        !is.na(S1_w123456)  ~ as.double(S11_1o_w34[3]),
      is.na(S11_1o_w34[3]) &  is.na(S11_1o_w34[4]) & !is.na(S11_1_w1[1]) & 
        S11_1_w1[1] < 99 & Welle == 5 & !is.na(S1_w123456) &
        is.na(S11_1_w5)  ~ as.double(S11_1_w1[1]-1),
      !is.na(S11_1_w5)  & Welle == 5 ~ as.double(S11_1_w5)
    ),
    # Haushaltsmitglieder zwischen 14 und 18
    hh_ue14u18_w12345 = case_when(
      # Welle 1
      Welle == 1 & S11_2_w1 < 99 ~ S11_2_w1-1,
      # Welle == 1 & S11_1_w1 == 99 ~ -7,
      # Welle 2
      !is.na(S11_2_w1[1]) & S11_2_w1[1] < 99 & Welle == 2 & !is.na(S1_w123456) ~ 
        as.double(S11_2_w1[1]-1),
      # Welle 3
      !is.na(S11_2o_w345) & Welle == 3 ~ as.double(S11_2o_w345),
      !is.na(S11_2_w1[1]) & S11_2_w1[1] < 99 & Welle == 3 & !is.na(S1_w123456) &
        S11_2_w3 == 0 & (S11_2_w1[1]-1) > 0 ~ as.double(S11_2_w1[1]-1),
      S11_2_w3 == 0 & (S11_2_w1[1]-1) == 0 ~ 0,
      # Welle 4
      !is.na(S11_2o_w345)  & Welle == 4 ~ as.double(S11_2o_w345),
      !is.na(S11_2o_w345[3]) & is.na(S11_2o_w345[4]) & Welle == 4 & !is.na(S1_w123456)  ~ 
        as.double(S11_2o_w345[3]),
      !is.na(S11_2_w1[1]) & S11_2_w1[1] < 99 & Welle == 4 & !is.na(S1_w123456) &
        is.na(S11_2o_w345) & is.na(S11_2o_w345[3]) ~ as.double(S11_2_w1[1]-1),
      # Welle 5
      !is.na(S11_2o_w345)  & Welle == 5 ~ as.double(S11_2o_w345),
      !is.na(S11_2o_w345[4]) & is.na(S11_2o_w345) & Welle == 5 & !is.na(S1_w123456)  ~ 
        as.double(S11_2o_w345[4]),
      !is.na(S11_2o_w345[3]) &  is.na(S11_2o_w345[4]) & is.na(S11_2o_w345) & Welle == 5 & 
        !is.na(S1_w123456)  ~ as.double(S11_2o_w345[3]),
      is.na(S11_2o_w345[3]) &  is.na(S11_2o_w345[4]) & !is.na(S11_2_w1[1]) & 
        S11_2_w1[1] < 99 & Welle == 5 & !is.na(S1_w123456) &
        is.na(S11_2o_w345)  ~ as.double(S11_2_w1[1]-1)
    ),
    # Haushaltsmitglieder zwischen 14 und 18
    hh_u14_w12345 = case_when(
      # Welle 1
      Welle == 1 & S11_3_w1 < 99 ~ S11_3_w1-1,
      # Welle == 1 & S11_1_w1 == 99 ~ -7,
      # Welle 2
      !is.na(S11_3_w1[1]) & S11_3_w1[1] < 99 & Welle == 2 & !is.na(S1_w123456) ~ 
        as.double(S11_3_w1[1]-1),
      # Welle 3
      !is.na(S11_3o_w345) & Welle == 3 ~ as.double(S11_3o_w345),
      !is.na(S11_3_w1[1]) & S11_3_w1[1] < 99 & Welle == 3 & !is.na(S1_w123456) &
        S11_3_w3 == 0 & (S11_3_w1[1]-1) > 0 ~ as.double(S11_3_w1[1]-1),
      S11_3_w3 == 0 & (S11_3_w1[1]-1) == 0 ~ 0,
      # Welle 4
      !is.na(S11_3o_w345)  & Welle == 4 ~ as.double(S11_3o_w345),
      !is.na(S11_3o_w345[3]) & is.na(S11_3o_w345[4]) & Welle == 4 & !is.na(S1_w123456)  ~ 
        as.double(S11_3o_w345[3]),
      !is.na(S11_3_w1[1]) & S11_3_w1[1] < 99 & Welle == 4 & !is.na(S1_w123456) &
        is.na(S11_3o_w345) & is.na(S11_3o_w345[3]) ~ as.double(S11_3_w1[1]-1),
      # Welle 5
      !is.na(S11_3o_w345)  & Welle == 5 ~ as.double(S11_3o_w345),
      !is.na(S11_3o_w345[4]) & is.na(S11_3o_w345) & Welle == 5 & !is.na(S1_w123456)  ~ 
        as.double(S11_3o_w345[4]),
      !is.na(S11_3o_w345[3]) &  is.na(S11_3o_w345[4]) & is.na(S11_3o_w345) & Welle == 5 & 
        !is.na(S1_w123456)  ~ as.double(S11_3o_w345[3]),
      is.na(S11_3o_w345[3]) &  is.na(S11_3o_w345[4]) & !is.na(S11_3_w1[1]) & 
        S11_3_w1[1] < 99 & Welle == 5 & !is.na(S1_w123456) &
        is.na(S11_3o_w345)  ~ as.double(S11_3_w1[1]-1)
    ),
    hh_ue18_flag_w12345 = case_when(
      # Welle 1
      Welle == 1 & S11_1_w1 < 99 ~ "Originalangabe",
      # Welle == 1 & S11_1_w1 == 99 ~ -7,
      # Welle 2
      !is.na(S11_1_w1[1]) & S11_1_w1[1] < 99 & Welle == 2 & !is.na(S1_w123456) ~ 
        "Übertrag aus Welle 1",
      # Welle 3
      !is.na(S11_1_w1[1]) & S11_1_w1[1] < 99 & Welle == 3 & !is.na(S1_w123456) &
        S11_1_w3 == 0 ~ "Übertrag aus Welle 1",
      !is.na(S11_1o_w34) & Welle == 3 ~ "Originalangabe",
      # Welle 4
      !is.na(S11_1o_w34[3]) & is.na(S11_1o_w34[4]) & Welle == 4 & !is.na(S1_w123456)  ~ 
        "Übertrag aus Welle 3",
      !is.na(S11_1_w1[1]) & S11_1_w1[1] < 99 & Welle == 4 & !is.na(S1_w123456) &
        is.na(S11_1o_w34) & is.na(S11_1o_w34[3]) ~ "Übertrag aus Welle 1",
      !is.na(S11_1o_w34)  & Welle == 4 ~ "Originalangabe",
      # Welle 5
      !is.na(S11_1o_w34[4]) & is.na(S11_1_w5) & Welle == 5 & !is.na(S1_w123456)  ~ 
        "Übertrag aus Welle 4",
      !is.na(S11_1o_w34[3]) &  is.na(S11_1o_w34[4]) & is.na(S11_1_w5) & Welle == 5 & 
        !is.na(S1_w123456)  ~ "Übertrag aus Welle 3",
      is.na(S11_1o_w34[3]) &  is.na(S11_1o_w34[4]) & !is.na(S11_1_w1[1]) & 
        S11_1_w1[1] < 99 & Welle == 5 & !is.na(S1_w123456) &
        is.na(S11_1_w5)  ~ "Übertrag aus Welle 1",
      !is.na(S11_1_w5)  & Welle == 5 ~ "Originalangabe"
    ),
    # Haushaltsmitglieder zwischen 14 und 18
    hh_ue14u18_flag_w12345 = case_when(
      # Welle 1
      Welle == 1 & S11_2_w1 < 99 ~ "Originalangabe",
      # Welle == 1 & S11_1_w1 == 99 ~ -7,
      # Welle 2
      !is.na(S11_2_w1[1]) & S11_2_w1[1] < 99 & Welle == 2 & !is.na(S1_w123456) ~ 
        "Übertrag aus Welle 1",
      # Welle 3
      !is.na(S11_2o_w345) & Welle == 3 ~ "Originalangabe",
      !is.na(S11_2_w1[1]) & S11_2_w1[1] < 99 & Welle == 3 & !is.na(S1_w123456) &
        S11_2_w3 == 0 & (S11_2_w1[1]-1) > 0 ~ "Übertrag aus Welle 1",
      S11_2_w3 == 0 & (S11_2_w1[1]-1) == 0 ~ "Originalangabe",
      # Welle 4
      !is.na(S11_2o_w345)  & Welle == 4 ~ "Originalangabe",
      !is.na(S11_2o_w345[3]) & is.na(S11_2o_w345[4]) & Welle == 4 & !is.na(S1_w123456)  ~ 
        "Übertrag aus Welle 3",
      !is.na(S11_2_w1[1]) & S11_2_w1[1] < 99 & Welle == 4 & !is.na(S1_w123456) &
        is.na(S11_2o_w345) & is.na(S11_2o_w345[3]) ~ "Übertrag aus Welle 1",
      # Welle 5
      !is.na(S11_2o_w345)  & Welle == 5 ~ "Originalangabe",
      !is.na(S11_2o_w345[4]) & is.na(S11_2o_w345) & Welle == 5 & !is.na(S1_w123456)  ~ 
        "Übertrag aus Welle 4",
      !is.na(S11_2o_w345[3]) &  is.na(S11_2o_w345[4]) & is.na(S11_2o_w345) & Welle == 5 & 
        !is.na(S1_w123456)  ~ "Übertrag aus Welle 3",
      is.na(S11_2o_w345[3]) &  is.na(S11_2o_w345[4]) & !is.na(S11_2_w1[1]) & 
        S11_2_w1[1] < 99 & Welle == 5 & !is.na(S1_w123456) &
        is.na(S11_2o_w345)  ~ "Übertrag aus Welle 1"
    ),
    # Haushaltsmitglieder zwischen 14 und 18
    hh_u14_flag_w12345 = case_when(
      # Welle 1
      Welle == 1 & S11_3_w1 < 99 ~ "Originalangabe",
      # Welle == 1 & S11_1_w1 == 99 ~ -7,
      # Welle 2
      !is.na(S11_3_w1[1]) & S11_3_w1[1] < 99 & Welle == 2 & !is.na(S1_w123456) ~ 
        "Übertrag aus Welle 1",
      # Welle 3
      !is.na(S11_3o_w345) & Welle == 3 ~ "Originalangabe",
      !is.na(S11_3_w1[1]) & S11_3_w1[1] < 99 & Welle == 3 & !is.na(S1_w123456) &
        S11_3_w3 == 0 & (S11_3_w1[1]-1) > 0 ~ "Übertrag aus Welle 1",
      S11_3_w3 == 0 & (S11_3_w1[1]-1) == 0 ~ "Originalangabe",
      # Welle 4
      !is.na(S11_3o_w345)  & Welle == 4 ~ "Originalangabe",
      !is.na(S11_3o_w345[3]) & is.na(S11_3o_w345[4]) & Welle == 4 & !is.na(S1_w123456)  ~ 
        "Übertrag aus Welle 3",
      !is.na(S11_3_w1[1]) & S11_3_w1[1] < 99 & Welle == 4 & !is.na(S1_w123456) &
        is.na(S11_3o_w345) & is.na(S11_3o_w345[3]) ~ "Übertrag aus Welle 1",
      # Welle 5
      !is.na(S11_3o_w345)  & Welle == 5 ~ "Originalangabe",
      !is.na(S11_3o_w345[4]) & is.na(S11_3o_w345) & Welle == 5 & !is.na(S1_w123456)  ~ 
        "Übertrag aus Welle 4",
      !is.na(S11_3o_w345[3]) &  is.na(S11_3o_w345[4]) & is.na(S11_3o_w345) & Welle == 5 & 
        !is.na(S1_w123456)  ~ "Übertrag aus Welle 3",
      is.na(S11_3o_w345[3]) &  is.na(S11_3o_w345[4]) & !is.na(S11_3_w1[1]) & 
        S11_3_w1[1] < 99 & Welle == 5 & !is.na(S1_w123456) &
        is.na(S11_3o_w345)  ~ "Übertrag aus Welle 1"
    )
  )

# Haushaltseinkommen
dat_long_full <- 
  dat_long_full %>%
  ungroup() %>%
  mutate(
    oecd_weight_w12345 = 1+(hh_ue18_w12345+hh_ue14u18_w12345-1)*0.5+hh_u14_w12345*0.3,
    # Klassenmittelwerte (Achtung: Welle 1 kein Individualeinkommen)
    hh_income_w12345 = case_when(
      S10_w1 == 1 | (D4_w2 == 1 | (D4_w2 == 99 & D2_w2 == 1)) | 
        (D4_w35 == 1 | (D4_w35 == 99 & D2_w35 == 1)) ~ 400,
      S10_w1 == 2 | (D4_w2 == 2 | (D4_w2 == 99 & D2_w2 == 2)) | 
        (D4_w35 == 2 | (D4_w35 == 99 & D2_w35 == 2)) ~ 700,
      S10_w1 == 3 | (D4_w2 == 3 | (D4_w2 == 99 & D2_w2 == 3)) | 
        (D4_w35 == 3 | (D4_w35 == 99 & D2_w35 == 3)) ~ 1100,
      S10_w1 == 4 | (D4_w2 == 4 | (D4_w2 == 99 & D2_w2 == 4)) | 
        (D4_w35 == 4 | (D4_w35 == 99 & D2_w35 == 4)) ~ 1400,
      S10_w1 == 5 | (D4_w2 == 5 | (D4_w2 == 99 & D2_w2 == 5)) | 
        (D4_w35 == 5 | (D4_w35 == 99 & D2_w35 == 5)) ~ 1600,
      S10_w1 == 6 | (D4_w2 == 6 | (D4_w2 == 99 & D2_w2 == 6)) | 
        (D4_w35 == 6 | (D4_w35 == 99 & D2_w35 == 6)) ~ 1850,
      S10_w1 == 7 | (D4_w2 == 7 | (D4_w2 == 99 & D2_w2 == 7)) | 
        (D4_w35 == 7 | (D4_w35 == 99 & D2_w35 == 7)) ~ 2300,
      S10_w1 == 8 | (D4_w2 == 9 | (D4_w2 == 99 & D2_w2 == 9)) | 
        (D4_w35 == 8 | (D4_w35 == 99 & D2_w35 == 8)) ~ 2900,
      S10_w1 == 9 | (D4_w2 == 3 | (D4_w2 == 99 & D2_w2 == 9))  ~ 3850, 
      (D4_w35 == 9 | (D4_w35 == 99 & D2_w35 == 9)) ~ 3500,
      S10_w1 == 10 ~ 5750,
      (D4_w2 == 10 | (D4_w2 == 99 & D2_w2 == 10)) |
        (D4_w35 == 11 | (D4_w35 == 99 & D2_w35 == 11)) ~ 5250,
      (D4_w35 == 10 | (D4_w35 == 99 & D2_w35 == 10)) ~ 4150,
      (D4_w2 == 11 | (D4_w2 == 99 & D2_w2 == 11)) |
        (D4_w35 == 12 | (D4_w35 == 99 & D2_w35 == 12)) ~ 6600
     ),
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
    hh_income_aeq_w12345 = hh_income_w12345/oecd_weight_w12345,
    hh_income_vor_aeq_w5 = hh_income_vor_w5/oecd_weight_w12345
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
      F8_w1235 == 1 ~ 1,
      F8_w1235 %in% 2:3 ~ 0,
      F8_w1235 == 99 ~ NA_real_,
      is.na(F8_w1235) ~ NA_real_
    ),
    v_hh = case_when(
      F9_w1235 == 1 ~ 1,
      F9_w1235 %in% 2:3 ~ 0,
      F9_w1235 == 99 ~ NA_real_,
      is.na(F9_w1235) ~ NA_real_
    ),
    v_hh = if_else(F9_w1235 == 4, v_i, v_hh),
    add_i = cumsum(v_i),
    add_i = if_else(add_i > 1, 1, as.double(add_i)),
    add_i_w1235 = case_when(
      !is.na(add_i) ~ as.double(add_i),
      Welle == 2 & !is.na(S1_w123456) & (v_i[1] == 1| v_i[2] == 1) ~ 1,
      Welle == 3 & !is.na(S1_w123456) & (v_i[1] == 1 | v_i[2] == 1 | v_i[3] == 1) ~ 1,
      Welle == 5 & ziehung_w5 == 0 & !is.na(S1_w123456) & (v_i[1] == 1 | v_i[2] == 1 | v_i[3] == 1 | v_i[5] == 1) ~ 1,
      Welle == 5 & ziehung_w5 == 0 & !is.na(S1_w123456) & v_i[5] == 0 & v_i[1] == 0 & v_i[2] == 0 & v_i[3] == 0 ~ 0,
      ziehung_w5 == 1 ~ as.double(v_i)
    ),
    add_hh = cumsum(v_hh),
    add_hh = if_else(add_hh > 1, 1, as.double(add_hh)),
    add_hh_w1235 = case_when(
      !is.na(add_hh) ~ as.double(add_hh),
      Welle == 2 & !is.na(S1_w123456) & (v_hh[1] == 1| v_hh[2] == 1) ~ 1,
      Welle == 3 & !is.na(S1_w123456) & (v_hh[1] == 1 | v_hh[2] == 1 | v_hh[3] == 1) ~ 1,
      Welle == 5 & ziehung_w5 == 0 & !is.na(S1_w123456) & (v_hh[1] == 1 | v_hh[2] == 1 | v_hh[3] == 1 | v_hh[5] == 1) ~ 1,
      Welle == 5 & ziehung_w5 == 0 & !is.na(S1_w123456) & v_hh[5] == 0 & v_hh[1] == 0 & v_hh[2] == 0 & v_hh[3] == 0 ~ 0,
      ziehung_w5 == 1 ~ as.double(v_hh)
    )
  ) %>%
  select(-c(v_i, v_hh, add_i, add_hh))

# Labels
attributes(dat_long_full$hh_ue18_w12345)$label <- 
  "Personen über 18 Jahre (ab Welle 5: 'ab' 18 Jahren)"
attributes(dat_long_full$hh_ue14u18_w12345)$label <- 
  "Personen über 14 Jahre (ab Welle 3: 'ab' 14 Jahren) und unter 18 Jahren"
attributes(dat_long_full$hh_u14_w12345)$label <- 
  "Personen bis 14 Jahre (ab Welle 3: 'unter' 14 Jahren)"
attributes(dat_long_full$hh_ue18_flag_w12345)$label <- 
  "Quelle der Variable hh_ue18"
attributes(dat_long_full$hh_ue14u18_flag_w12345)$label <- 
  "Quelle der Variable hh_ue14u18"
attributes(dat_long_full$hh_u14_flag_w12345)$label <- "Quelle der Variable hh_u14"
attributes(dat_long_full$oecd_weight_w12345)$label <- "OECD-Bedarfsgewicht"
attributes(dat_long_full$add_i_w1235)$label <- "Kumulativer Index: Bisher Verlust durch Krise (Individualeinkommen)"
attributes(dat_long_full$add_hh_w1235)$label <- "Kumulativer Index: Bisher Verlust durch Krise (Haushaltseinkommen)"
attributes(dat_long_full$hh_income_w12345)$label <- "Haushaltseinkommen (Klassenmittelwerte)"
attributes(dat_long_full$hh_income_vor_w5)$label <- "Haushaltseinkommen vor der Krise (Klassenmittelwerte)"
attributes(dat_long_full$hh_income_aeq_w12345)$label <- "Haushaltsnettoäquivalenzeinkommen"
attributes(dat_long_full$hh_income_vor_aeq_w5)$label <- "Haushaltsnettoäquivalenzeinkommen"

attributes(dat_long_full$add_i_w1235)$labels <- 
  setNames(c(0,1), c("bisher kein Verlust durch Krise", "bereits Verlust durch Krise")) 

attributes(dat_long_full$add_hh_w1235)$labels <- 
  setNames(c(0,1), c("bisher kein Verlust durch Krise", "bereits Verlust durch Krise"))

