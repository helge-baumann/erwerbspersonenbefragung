# F8_w1235: Einfluss auf persönliches Einkommen
# F9_w1235: Einfluss auf Haushaltseinkommen

# D2_w35: Individualeinkommen Welle 3 und 5
# D2_w2: Individualeinkommen Welle 2
# S10_w1: HHeinkommen Welle 1
# D4_w2: Haushaltseinkommen Welle 2
# D4_w35: Haushaltseinkommen Welle 3 und 5



dat <- dat_ges %>%
  select(F9__1, D5__3, D5__5, S10__1, D4__3, D4__5, W5_D4a__5) %>%
  mutate(
    verlust_1 = case_when(F9__1 %in% 1 ~ "Verlust", F9__1 %in% 2:3 ~ "kein Verlust"),
    verlust_3 = case_when(D5__3 %in% 1 ~ "Verlust", D5__3 %in% 2:3 ~ "kein Verlust"),
    verlust_5 = case_when(D5__5 %in% 1 ~ "Verlust", D5__5 %in% 2:3 ~ "kein Verlust"),
    ek_hh_0 = case_when(
      W5_D4a__5 %in% 1:8 ~ W5_D4a__5, W5_D4a__5 %in% 9:10 ~ 9, W5_D4a__5 %in% 11:12 ~ 10),
    ek_hh_1 = S10__1, 
    ek_hh_3 = case_when(
      D4__3 %in% 1:8 ~ D4__3, D4__3 %in% 9:10 ~ 9, D4__3 %in% 11:12 ~ 10),
    ek_hh_5 = case_when(
      D4__5 %in% 1:8 ~ D4__3, D4__5 %in% 9:10 ~ 9, D4__5 %in% 11:12 ~ 10)
    ) %>%
  select(starts_with("ek_"), starts_with("verlust")) %>%
  filter(complete.cases(.) & ek_hh_0 != 99 & ek_hh_1 != 99 & ek_hh_3 != 99 & ek_hh_5 != 99)

for(i in c(0, 1, 3, 5)) {
dat[[paste0("ek_num_", i)]] <- as.integer(dat[[paste0("ek_hh_", i)]])
dat[[paste0("ek_num_", i)]][dat[[paste0("ek_num_", i)]] == 1] <- 250
dat[[paste0("ek_num_", i)]][dat[[paste0("ek_num_", i)]] == 2] <- 700
dat[[paste0("ek_num_", i)]][dat[[paste0("ek_num_", i)]] == 3] <- 1100
dat[[paste0("ek_num_", i)]][dat[[paste0("ek_num_", i)]] == 4] <- 1400
dat[[paste0("ek_num_", i)]][dat[[paste0("ek_num_", i)]] == 5] <- 1600
dat[[paste0("ek_num_", i)]][dat[[paste0("ek_num_", i)]] == 6] <- 1850
dat[[paste0("ek_num_", i)]][dat[[paste0("ek_num_", i)]] == 7] <- 2300
dat[[paste0("ek_num_", i)]][dat[[paste0("ek_num_", i)]] == 8] <- 2900
dat[[paste0("ek_num_", i)]][dat[[paste0("ek_num_", i)]] == 9] <- 3850
dat[[paste0("ek_num_", i)]][dat[[paste0("ek_num_", i)]] == 10] <- 4500
}

dat %>%
  mutate(c1 = ek_num_1-ek_num_0, c3 = ek_num_3-ek_num_0, c5 = ek_num_5-ek_num_0) %>%
  group_by(ek_num_0) %>%
  summarise(n(), mean(c1), mean(c3), mean(c5))

# Sprünge
dat %>%
  group_by(ek_num_0) %>%
  summarise(s = mean(abs(ek_hh_1-ek_hh_0)+ abs(ek_hh_3-ek_hh_0) + abs(ek_hh_5-ek_hh_0)))

dat_check <- dat %>% 
  mutate(
    check1 = case_when(
      ek_hh_1 == ek_hh_0 ~ "Einkommensklasse gleich, keine Überprüfung möglich",
      ek_hh_1 < ek_hh_0 & verlust_1 == "Verlust" ~ "Einkommensklasse geringer, Verlust angegeben",
      ek_hh_1 < ek_hh_0 & verlust_1 == "kein Verlust" ~ "Einkommensklasse geringer, kein Verlust angegeben",
      ek_hh_1 > ek_hh_0 & verlust_1 == "kein Verlust" ~ "Einkommensklasse höher, kein Verlust angegeben",
      ek_hh_1 > ek_hh_0 & verlust_1 == "Verlust" ~ "Einkommensklasse höher, Verlust angegeben",
    ),
    check3 = case_when(
      ek_hh_3 == ek_hh_0 ~ "Einkommensklasse gleich, keine Überprüfung möglich",
      ek_hh_3 < ek_hh_0 & verlust_3 == "Verlust" ~ "Einkommensklasse geringer, Verlust angegeben",
      ek_hh_3 < ek_hh_0 & verlust_3 == "kein Verlust" ~ "Einkommensklasse geringer, kein Verlust angegeben",
      ek_hh_3 > ek_hh_0 & verlust_3 == "kein Verlust" ~ "Einkommensklasse höher, kein Verlust angegeben",
      ek_hh_3 > ek_hh_0 & verlust_3 == "Verlust" ~ "Einkommensklasse höher, Verlust angegeben",
    ),
    check5 = case_when(
      ek_hh_5 == ek_hh_0 ~ "Einkommensklasse gleich, keine Überprüfung möglich",
      ek_hh_5 < ek_hh_0 & verlust_5 == "Verlust" ~ "Einkommensklasse geringer, Verlust angegeben",
      ek_hh_5 < ek_hh_0 & verlust_5 == "kein Verlust" ~ "Einkommensklasse geringer, kein Verlust angegeben",
      ek_hh_5 > ek_hh_0 & verlust_5 == "kein Verlust" ~ "Einkommensklasse höher, kein Verlust angegeben",
      ek_hh_5 > ek_hh_0 & verlust_5 == "Verlust" ~ "Einkommensklasse höher, Verlust angegeben",
    ),
    check1_t = case_when(
      ek_hh_1 == ek_hh_0 | ek_hh_1 == ek_hh_0-1 | ek_hh_1 == ek_hh_0+1 ~ "Einkommensklasse gleich, keine Überprüfung möglich",
      ek_hh_1 < ek_hh_0-1 & verlust_1 == "Verlust" ~ "Einkommensklasse geringer, Verlust angegeben",
      ek_hh_1 < ek_hh_0-1 & verlust_1 == "kein Verlust" ~ "Einkommensklasse geringer, kein Verlust angegeben",
      ek_hh_1 > ek_hh_0+1 & verlust_1 == "kein Verlust" ~ "Einkommensklasse höher, kein Verlust angegeben",
      ek_hh_1 > ek_hh_0+1 & verlust_1 == "Verlust" ~ "Einkommensklasse höher, Verlust angegeben",
    ),
    check3_t = case_when(
      ek_hh_3 == ek_hh_0 | ek_hh_3 == ek_hh_0-1 | ek_hh_3 == ek_hh_0+1 ~ "Einkommensklasse gleich, keine Überprüfung möglich",
      ek_hh_3 < ek_hh_0-1 & verlust_3 == "Verlust" ~ "Einkommensklasse geringer, Verlust angegeben",
      ek_hh_3 < ek_hh_0-1 & verlust_3 == "kein Verlust" ~ "Einkommensklasse geringer, kein Verlust angegeben",
      ek_hh_3 > ek_hh_0+1 & verlust_3 == "kein Verlust" ~ "Einkommensklasse höher, kein Verlust angegeben",
      ek_hh_3 > ek_hh_0+1 & verlust_3 == "Verlust" ~ "Einkommensklasse höher, Verlust angegeben",
    ),
    check5_t = case_when(
      ek_hh_5 == ek_hh_0 | ek_hh_5 == ek_hh_0-1 | ek_hh_5 == ek_hh_0+1 ~ "Einkommensklasse gleich, keine Überprüfung möglich",
      ek_hh_5 < ek_hh_0-1 & verlust_5 == "Verlust" ~ "Einkommensklasse geringer, Verlust angegeben",
      ek_hh_5 < ek_hh_0-1 & verlust_5 == "kein Verlust" ~ "Einkommensklasse geringer, kein Verlust angegeben",
      ek_hh_5 > ek_hh_0+1 & verlust_5 == "kein Verlust" ~ "Einkommensklasse höher, kein Verlust angegeben",
      ek_hh_5 > ek_hh_0+1 & verlust_5 == "Verlust" ~ "Einkommensklasse höher, Verlust angegeben",
    )
    ) 
  

