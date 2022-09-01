# strata

dat <- dat %>%
  mutate(stratum_Alle = "Gesamt",
         stratum_Alter = 
           cut(S1_w1234, 
               breaks=c(0, 30, 45, 60, 100), 
               labels=c("1. bis 30", "2. 31 bis 45", "3. 46 bis 60", "4. 61 und älter")),
         stratum_Geschlecht = if_else(S2_w1234 == 1, "1. Männlich oder Divers", "2. Weiblich"),
         stratum_HHEinkommen = case_when(
           Welle == 1 & S10_w1 %in% 1:4 ~ "unter 1.500 Euro",
           Welle == 1 & S10_w1 %in% 4:7 ~ "1.500 bis unter 2.600 Euro",
           Welle == 1 & S10_w1 %in% 8:9 ~ "2.600 bis unter 4.500 Euro",
           Welle == 1 & S10_w1 %in% 10 ~ "4.500 Euro und mehr",
           Welle == 2 & D4_w2 %in% 1:4 ~ "unter 1.500 Euro",
           Welle == 2 & D4_w2 %in% 4:7 ~ "1.500 bis unter 2.600 Euro",
           Welle == 2 & D4_w2 %in% 8:9 ~ "2.600 bis unter 4.500 Euro",
           Welle == 2 & D4_w2 %in% 10:11 ~ "4.500 Euro und mehr",
           Welle == 3 & D4_w3 %in% 1:4 ~ "unter 1.500 Euro",
           Welle == 3 & D4_w3 %in% 4:7 ~ "1.500 bis unter 2.600 Euro",
           Welle == 3 & D4_w3 %in% 8:10 ~ "2.600 bis unter 4.500 Euro",
           Welle == 3 & D4_w3 %in% 11:12 ~ "4.500 Euro und mehr"
         ),
         stratum_Branche = if_else(
           S5_w1 %in% c(2:10, 12:13), as_factor(S5_w1), NA_integer_
         )
         )

  attributes(dat$stratum_Alle)$label <- "Alle"
  attributes(dat$stratum_Alter)$label <- "Alter"
  attributes(dat$stratum_Geschlecht)$label <- "Geschlecht"
  attributes(dat$stratum_HHEinkommen)$label <- "Haushaltsnettoeinkommen"
  attributes(dat$stratum_Branche)$label <- "Branche"

dat <- dat %>%
  group_by(lfdn_w1__1) %>%
  fill(stratum_Branche, stratum_HHEinkommen) %>% ungroup()
  
