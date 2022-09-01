# neue Variablen generieren mit mutate(); siehe ?mutate

dat <- 
  dat_roh %>%
  group_by(ID) %>%
  mutate(
    # gewichte
    gewicht_w1 = Faktor_w123456[1], 
    gewicht_w2 = Faktor_w123456[2],
    gewicht_w3 = Faktor_w123456[3],
    gewicht_w4 = Faktor_w123456[4],
    gewicht_w5 = Faktor_w123456[5],
    # Homeoffice (vereinfacht)
    `2020-01-15` = case_when(F3_w1 == 2 ~ 1, F3_w1 %in% c(1,3) ~ 0),
    `2020-04-15` = case_when( Welle == 1 & F2_w12345 == 2 ~ 1, Welle == 1 & F2_w12345 %in% c(1,3) ~ 0),
    `2020-06-20` = case_when( Welle == 2 & F2_w12345 == 2 ~ 1, Welle == 2 & F2_w12345 %in% c(1,3) ~ 0),
    `2020-11-06` = case_when( Welle == 3 & F2_w12345 == 2 ~ 1, Welle == 3 & F2_w12345 %in% c(1,3) ~ 0),
    `2020-12-15` = case_when( Welle == 4 & W4_1_w4 == 2 ~ 1, Welle == 4 & W4_1_w4 %in% c(1,3) ~ 0),
    `2021-01-28` = case_when( Welle == 4 & F2_w12345 == 2 ~ 1, Welle == 4 & F2_w12345 %in% c(1,3) ~ 0),
    `2021-07-01` = case_when( Welle == 5 & F2_w12345 == 2 ~ 1, Welle == 5 & F2_w12345 %in% c(1,3) ~ 0),
    # Betrieb (vereinfacht)
    `betrieb_2020-01-15` = case_when(F3_w1 == 1 ~ 1, F3_w1 %in% c(2,3) ~ 0),
    `betrieb_2020-04-15` = case_when( Welle == 1 & F2_w12345 == 1 ~ 1, Welle == 1 & F2_w12345 %in% c(2,3) ~ 0),
    `betrieb_2020-06-20` = case_when( Welle == 2 & F2_w12345 == 1 ~ 1, Welle == 2 & F2_w12345 %in% c(2,3) ~ 0),
    `betrieb_2020-11-06` = case_when( Welle == 3 & F2_w12345 == 1 ~ 1, Welle == 3 & F2_w12345 %in% c(2,3) ~ 0),
    `betrieb_2020-12-15` = case_when( Welle == 4 & W4_1_w4 == 1 ~ 1, Welle == 4 & W4_1_w4 %in% c(2,3) ~ 0),
    `betrieb_2021-01-28` = case_when( Welle == 4 & F2_w12345 == 1 ~ 1, Welle == 4 & F2_w12345 %in% c(2,3) ~ 0),
    `betrieb_2021-07-01` = case_when( Welle == 5 & F2_w12345 == 1 ~ 1, Welle == 5 & F2_w12345 %in% c(2,3) ~ 0),
    # Haushaltsnettoeinkommen
    net_income = 
      cut(
        hh_income_vor_aeq_w5[5], 
        breaks=c(0, 1500, 3000, Inf),
        labels=c("unter 1.500", "1.500 bis unter 3.000", "über 3.000"),
        include.lowest=T, right=F),
    # Haushaltstypen
    haushaltstyp = 
      case_when(
        hh_ue18_w12345 == 1 & ((hh_u14_w12345) == 0) ~ "1 Erwachsener, keine Kinder",
        hh_ue18_w12345 == 1 & ((hh_u14_w12345) > 0) ~ "1 Erwachsener, Kinder",
        hh_ue18_w12345 > 1 & ((hh_u14_w12345) == 0) ~ "mind. Erwachsener, keine Kinder",
        hh_ue18_w12345 > 1 & ((hh_u14_w12345) > 0) ~ "mind. 2 Erwachsener, Kinder"
      ),
    # Potenzial
    potenzial = case_when(
      W4_2_w4[4] %in% 1:2 & !is.na(S1_w123456) ~ 1, 
      W4_2_w4[4] %in% 3:4 & !is.na(S1_w123456) ~ 0
      ),
    potenzial_diff = case_when(!is.na(S1_w123456) ~ W4_2_w4[4]),
    branche = case_when(!is.na(S1_w123456) ~ as_factor(S5_w1[1])),
    betriebsgroesse = case_when(!is.na(S1_w123456) ~ as_factor(F32_w1[1])),
    erwerbsform = case_when(!is.na(S1_w123456) ~ S6_w123[1]),
    arbeitszeit_1_2020 = if_else(A1co_w2[2] < 60 & Welle == 1, as.integer(A1co_w2[2]), NA_integer_),
    arbeitszeit_4_2020 = if_else(A1d_2_w2[2] < 60 & Welle == 1, as.integer(A1d_2_w2[2]), NA_integer_),
    arbeitszeit_6_2020 = if_else(A1d_4_w2 < 60, as.integer(A1d_4_w2), NA_integer_),
    arbeitszeit_11_2020 = if_else(A1d_1_w5[5] < 60 & Welle == 3, as.integer(A1d_1_w5[5]), NA_integer_),
    arbeitszeit_12_2020 = if_else(A1d_2_w5[5] < 60 & Welle == 4, as.integer(A1d_2_w5[5]), NA_integer_),
    arbeitszeit_2_2021 = if_else(A1d_4_w5[5] < 60 & Welle == 4, as.integer(A1d_4_w5[5]), NA_integer_),
    arbeitszeit_6_2021 = if_else(A1d_8_w5 < 60, as.integer(A1d_8_w5), NA_integer_),
    regelung = case_when(
      F20_3_w123 == 1 & Welle == 1 ~ 1, 
      F20_3_w123 == 2 & Welle == 1 ~ 0,  
      F20_3_w123 == 1 & Welle == 2 ~ 1, 
      F20_3_w123 == 2 & Welle == 2 ~ 0,
      F20_3_w123 == 1 & Welle == 3 ~ 1, 
      F20_3_w123 == 2 & Welle == 3 ~ 0,
      A2b_w5[5] == 1 & Welle %in% 4:5 ~ 1, 
      A2b_w5[5] == 2 & Welle %in% 4:5 ~ 0 ),
    betriebsrat = case_when(F34_w135[1] %in% 1:2  ~ as_factor(F34_w135[1])),
    tarifvertrag = case_when(F33_w1[1] %in% 1:2  ~ as_factor(F33_w1[1]))
  ) %>%
  ungroup() %>%
  mutate(
    `2020-01-15` = if_else(arbeitszeit_1_2020 == 0 & !is.na(arbeitszeit_1_2020), NA_real_, `2020-01-15`),
    `2020-04-15` = if_else(arbeitszeit_4_2020 == 0 & !is.na(arbeitszeit_4_2020), NA_real_, `2020-04-15`),
    `2020-06-20` = if_else(arbeitszeit_6_2020 == 0 & !is.na(arbeitszeit_6_2020), NA_real_, `2020-06-20`),
    `2020-11-06` = if_else(arbeitszeit_11_2020 == 0 & !is.na(arbeitszeit_11_2020), NA_real_, `2020-11-06`),
    `2020-12-15` = if_else(arbeitszeit_12_2020 == 0 & !is.na(arbeitszeit_12_2020), NA_real_, `2020-12-15`),
    `2021-01-28` = if_else(arbeitszeit_2_2021 == 0 & !is.na(arbeitszeit_2_2021), NA_real_, `2021-01-28`),
    `2021-07-01` = if_else(arbeitszeit_6_2021 == 0 & !is.na(arbeitszeit_6_2021), NA_real_, `2021-07-01`),
    `betrieb_2020-01-15` = if_else(arbeitszeit_1_2020 == 0 & !is.na(arbeitszeit_1_2020), NA_real_, `betrieb_2020-01-15`),
    `betrieb_2020-04-15` = if_else(arbeitszeit_4_2020 == 0 & !is.na(arbeitszeit_4_2020), NA_real_, `betrieb_2020-04-15`),
    `betrieb_2020-06-20` = if_else(arbeitszeit_6_2020 == 0 & !is.na(arbeitszeit_6_2020), NA_real_, `betrieb_2020-06-20`),
    `betrieb_2020-11-06` = if_else(arbeitszeit_11_2020 == 0 & !is.na(arbeitszeit_11_2020), NA_real_, `betrieb_2020-11-06`),
    `betrieb_2020-12-15` = if_else(arbeitszeit_12_2020 == 0 & !is.na(arbeitszeit_12_2020), NA_real_, `betrieb_2020-12-15`),
    `betrieb_2021-01-28` = if_else(arbeitszeit_2_2021 == 0 & !is.na(arbeitszeit_2_2021), NA_real_, `betrieb_2021-01-28`),
    `betrieb_2021-07-01` = if_else(arbeitszeit_6_2021 == 0 & !is.na(arbeitszeit_6_2021), NA_real_, `betrieb_2021-07-01`),
    sex = as_factor(S2_w123456)
  ) 

