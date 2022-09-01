dat_long_full %>%
  filter(ID %in% c(246, 247, 253, 260, 263, 271)) %>%
  select(ID, Welle, S10_w1, D4_w2, D4_w357, D2_w2, D2_w357, starts_with("S11"), 
         hh_u14_w123457, hh_ue14u18_w123457, hh_ue18_w123457, hh_income_w123457,
         hh_income_aeq_w123457) %>%
  filter(Welle %in% 1:3) %>%
  mutate_all(as_factor) %>%
  write.csv2("check.csv")
