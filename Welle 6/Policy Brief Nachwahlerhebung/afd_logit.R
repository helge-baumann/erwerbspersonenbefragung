# afd-Modell. 

# Institutionen: W6_5_1_w6:W6_5_8_w6
dat_model <-  
  dat %>%
  mutate(afd = 
           case_when(
             W6_2_w6 == 7 ~ 1,
             W6_2_w6 %in% 1:6 ~ 0
           ), 
         westost = case_when(bundesland %in% levels(bundesland)[1:10] ~ "West", bundesland %in% levels(bundesland)[11:16] ~ "Ost"), 
         bildung_kat = case_when(bildung %in% levels(bildung)[1] ~ levels(bildung)[1], 
                                 bildung %in% levels(bildung)[2] ~ levels(bildung)[2], 
                                 bildung %in% levels(bildung)[3] ~ levels(bildung)[3], 
                                 bildung %in% levels(bildung)[4:5] ~ "Sonstiges oder kein Abschluss")) %>%
  mutate(across(W6_5_1_w6:W6_5_8_w6, ~case_when(. %in% 1:2 ~ "wenig oder kein Vertrauen", 
                                                    . %in% 3 ~ "mittelmäßiges Vertrauen",
                                           . %in% 4:5 ~ "großes oder sehr großes Vertrauen"))) %>%
  filter(!is.na(afd) & !(gewerkschaft == "Keine Angabe") & !(is.na(bildung_kat)) & !(migration == "Keine Angabe") &
           (geschlecht != "Divers")) %>%
  mutate(
    geschlecht = droplevels(geschlecht), 
    bildung = droplevels(bildung), 
    migration = droplevels(migration), 
    gewerkschaft = droplevels(gewerkschaft), 
    gewerkschaft = fct_relevel(gewerkschaft, "Nein"), 
    wahllokal = fct_relevel(wahllokal, "Wahllokal"), 
    westost = fct_relevel(westost, "West"),
    bildung_kat = fct_relevel(bildung_kat, "Mittlere Reife oder Abschluss der polytechnischen Oberschule", "Haupt- oder Volksschulabschluss"), 
    across(W6_5_1_w6:W6_5_8_w6, ~fct_relevel(., "mittelmäßiges Vertrauen")), 
    sorgen_wirtschaft = fct_relevel(sorgen_wirtschaft, "Sorgen"), 
    sorgen_gesundheit = fct_relevel(sorgen_gesundheit, "Sorgen"), 
    einschraenkung_grundrechte = fct_relevel(einschraenkung_grundrechte, "absolut berechtigt", "berechtigt", "eher unberechtigt", "unberechtigt"), 
    
  nettoeinkommen_ka = as_factor(nettoeinkommen_ka), 
  nettoeinkommen_ka = fct_relevel(nettoeinkommen_ka, "1.500 bis unter 2.000 Euro", "bis unter 1.500 Euro")) %>%
  filter_at(vars(geschlecht, bildung_kat, alter, gewerkschaft, nettoeinkommen_ka, westost, einkommensverlust_hh, betroffenheit_corona, 
                 migration, wahllokal, zufriedenheit_regierung, impfung, W6_5_1_w6:W6_5_8_w6, 
                 einschraenkung_grundrechte, sorgen_wirtschaft, sorgen_gesundheit),all_vars(!is.na(.)))


model_1 <- 
  glm(
    afd ~ 
      
      impfung + einschraenkung_grundrechte + 
      sorgen_wirtschaft + sorgen_gesundheit + 
      W6_5_4_w6 + W6_5_7_w6,
    data= dat_model, family=binomial)


model_2 <- update(model_1, afd ~ . + wahllokal + 
                    geschlecht + bildung_kat + alter + gewerkschaft + nettoeinkommen_ka + einkommensverlust_hh + betroffenheit_corona)

model_3 <- update(model_2, afd ~ . + westost)



margins_1 <- margins(model_1, type="response")
margins_2 <- margins(model_2, type="response")
margins_3 <- margins(model_3, type="response")

  tibble(
    rownames = row.names(summary(model_3)$coefficients)
    ) %>%
  left_join(summary(margins_1), by=c("rownames"="factor")) %>%
  left_join(summary(margins_2), by=c("rownames"="factor")) %>%
    left_join(summary(margins_3), by=c("rownames"="factor")) %>%
  mutate(
    coef_1 = paste0(sprintf("%.2f", round(AME.x*100, 2)), "\n (", sprintf("%.2f", round(SE.x*100, 2)), ")"), 
    sign_1 = case_when(
      p.x > 0.05  ~ "",
      p.x >= 0.01 & p.x < 0.05 ~ "*",
      p.x < 0.01 & p.x >= 0.001 ~ "***",
      p.x < 0.001 ~ "***"
    ),
    coef_2 = paste0(sprintf("%.2f", round(AME.y*100, 2)), "\n (", sprintf("%.2f", round(SE.y*100, 2)), ")"), 
    sign_2 = case_when(
      p.y > 0.05  ~ "",
      p.y >= 0.01 & p.y < 0.05 ~ "*",
      p.y < 0.01 & p.y >= 0.001 ~ "***",
      p.y < 0.001 ~ "***"
    ), 
    coef_3 = paste0(sprintf("%.2f", round(AME*100, 2)), "\n (", sprintf("%.2f", round(SE*100, 2)), ")"), 
    sign_3 = case_when(
      p > 0.05  ~ "",
      p >= 0.01 & p < 0.05 ~ "*",
      p < 0.01 & p >= 0.001 ~ "***",
      p < 0.001 ~ "***"
    )
  ) %>%
  select(rownames, coef_1, sign_1, coef_2, sign_2, coef_3, sign_3) %>%
  write.csv2(paste0("Output/", Sys.Date(), "_afd_logit.csv"))
  
length(model_1$residuals)
length(model_3$residuals)




