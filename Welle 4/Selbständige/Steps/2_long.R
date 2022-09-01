# Analyse Längsschnitt 4. Welle

Ergebnis <- list()

as_factor_date <- function(x) {
  
  if(class(x)[1] == "Date") return(x) else return(as_factor(x))
  
}

# Welle 4 übertragen
dat_taetigkeit <- dat %>% select(lfdn_w1__1, Welle, S6_w4) %>%
  pivot_wider(id_cols=c(lfdn_w1__1), names_from=Welle, values_from=S6_w4) %>%
  mutate(`1` = `4`, `2` = `4`, `3` = `4`) %>%
  pivot_longer(cols=-lfdn_w1__1, names_to="Welle", values_to="taetigkeit")
dat_faktor <- dat %>% select(lfdn_w1__1, Welle, Faktor_w1234) %>%
  pivot_wider(id_cols=c(lfdn_w1__1), names_from=Welle, values_from=Faktor_w1234) %>%
  mutate(`1` = `4`, `2` = `4`, `3` = `4`) %>%
  pivot_longer(cols=-lfdn_w1__1, names_to="Welle", values_to="faktor_w4")
dat <- dat %>% 
  left_join(dat_taetigkeit) %>%
  left_join(dat_faktor) %>%
  mutate(selbst = case_when(taetigkeit %in% c(1:3) ~ "Arbeiter/in, Angestellte/r, Beamte/r",
                            taetigkeit %in% c(4:6) ~ "Selbständig",
                            taetigkeit %in% c(7, 99) | is.na(S6_w4) ~ NA_character_))

for(b in setdiff(names(dat), c("ldfn_w1", "Welle", "faktor_w4", "selbst"))) {
  
  # noch Mittelwerte für numerische Variablen ausweisen!
  # Keine Angabe rausnehmen!
  
  if(length(unique(dat[[b]])) < 20 & class(dat[[b]])[1] != "character") {
  Ergebnis[[b]]$table <- dat %>%
    select(starts_with("selbst"), b, faktor_w4, Welle) %>%
    pivot_longer(cols=-c(b, "faktor_w4", "Welle"), names_to="Variable", values_to="Wert") %>%
    mutate(!!b := as_factor_date(get(b))) %>%
    #filter(!is.na(get(b)) ) %>%
    group_by(Variable, Wert, Welle, get(b)) %>%
    summarise(
      anteil = sum(faktor_w4[
        !is.na(get(b)) & 
          !str_detect(tolower(get(b)), "^keine angabe") & 
          !str_detect(tolower(get(b)), "^weiß nicht")]), 
      n=sum(!is.na(get(b))), 
      nv = sum(!is.na(get(b)) & 
                 !str_detect(tolower(get(b)), "^keine angabe") & 
                 !str_detect(tolower(get(b)), "^weiß nicht"))) %>%
    mutate(anteil=round((anteil/sum(anteil)*100), digits=0), 
           n=sum(n), 
           nv=sum(nv), nv=n-nv) %>%
    rename(!!b := `get(b)`) %>%
    filter(!is.na(get(b)) & !str_detect(tolower(get(b)), "^keine angabe") & !str_detect(tolower(get(b)), "^weiß nicht")) %>%
    filter(!is.na(Wert)) %>%
    select(-Variable)
  
  Ergebnis[[b]]$label <- attributes(dat[[b]])$label
 
  }
  
  # numerisch
  if(length(unique(dat[[b]])) > 20 & class(dat[[b]])[1] != "character") {
    
    # missings
    miss <- attributes(dat[[b]])$labels[
      str_detect(tolower(names(attributes(dat[[b]])$labels)), "^keine angabe") |
        str_detect(tolower(names(attributes(dat[[b]])$labels)), "^weiß nicht") 
    ]
    
    Ergebnis[[b]]$table <- dat %>%
      select(starts_with("selbst"), b, faktor_w4, Welle) %>%
      pivot_longer(cols=-c(b, "faktor_w4", "Welle"), names_to="Variable", values_to="Wert") %>%
      #filter(!is.na(get(b))) %>%
      group_by(Variable, Wert, Welle) %>%
      summarise(median = 
                  round(wtd.quantile(get(b)[!get(b) %in% miss & !is.na(get(b))], 
                                     faktor_w4[!get(b) %in% miss & !is.na(get(b))], probs=0.5), digits=0), 
                mean=round(wtd.mean(get(b)[!get(b) %in% miss & !is.na(get(b))], 
                                    faktor_w4[!get(b) %in% miss & !is.na(get(b))]), digits=1),
                n=sum(!is.na(get(b))),
                nv=sum(!get(b) %in% miss & !is.na(get(b)))) %>%
      mutate(nv = n-nv) %>%
      filter(!is.na(Wert) & !is.na(median))
    
    
    Ergebnis[[b]]$label <- attributes(dat[[b]])$label
    
  }
  
  print(b)
  
}

saveRDS(Ergebnis, "./Output/Ergebnisse_Selbstaendigkeit.rds")
