# Analyse Querschnitt 5. Welle

Ergebnis_quer <- list()

as_factor_date <- function(x) {
  
  if(class(x)[1] == "Date") return(x) else return(as_factor(x))
  
}


dat5 <- dat5 %>% 
  mutate(
    selbst = case_when(
      S6 %in% c(1:3) ~ "Arbeiter/in, Angestellte/r, Beamte/r",
      S6 %in% c(4:6) ~ "Selbständig",
      S6 %in% c(7, 99) | is.na(S6) ~ NA_character_),
    selbst_solo = 
      case_when(
        S6 %in% c(1:3) ~ "Arbeiter/in, Angestellte/r, Beamte/r",
        S6 %in% c(4:6) & W5_S6h == 1 ~ "Selbständig ohne Mitarbeiter:innen",
        S6 %in% c(4:6) & W5_S6h == 2 ~ "Selbständig mit Mitarbeiter:innen",
        S6 %in% c(7, 99) | is.na(S6) ~ NA_character_)
    )

for(b in setdiff(names(dat5), 
                 c("ldfn_W1", "Faktor", "selbst", "selbst_solo", "Interviewtag"))) {
  
  # noch Mittelwerte für numerische Variablen ausweisen!
  # Keine Angabe rausnehmen!
  
  if(length(unique(dat5[[b]])) < 20 & class(dat5[[b]])[1] != "character") {
    Ergebnis_quer[[b]]$table <- dat5 %>%
      select(starts_with("selbst"), b, Faktor) %>%
      pivot_longer(cols=-c(b, "Faktor"), names_to="Variable", values_to="Wert") %>%
      mutate(!!b := as_factor_date(get(b))) %>%
      #filter(!is.na(get(b)) ) %>%
      group_by(Variable, Wert, get(b)) %>%
      summarise(
        anteil = sum(Faktor[
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
      filter(!is.na(get(b)) & !str_detect(tolower(get(b)), "^keine angabe") & 
               !str_detect(tolower(get(b)), "^weiß nicht")) %>%
      filter(!is.na(Wert)) %>%
      select(-Variable)
    
    Ergebnis_quer[[b]]$label <- attributes(dat5[[b]])$label
    
  }
  
  # numerisch
  if(length(unique(dat5[[b]])) > 20 & class(dat5[[b]])[1] != "character") {
    
    # missings
    miss <- attributes(dat5[[b]])$labels[
      str_detect(tolower(names(attributes(dat5[[b]])$labels)), "^keine angabe") |
        str_detect(tolower(names(attributes(dat5[[b]])$labels)), "^weiß nicht") 
    ]
    
    Ergebnis_quer[[b]]$table <- dat5 %>%
      select(starts_with("selbst"), b, Faktor) %>%
      pivot_longer(cols=-c(b, "Faktor"), names_to="Variable", values_to="Wert") %>%
      #filter(!is.na(get(b))) %>%
      group_by(Variable, Wert) %>%
      summarise(median = 
                  round(wtd.quantile(get(b)[!get(b) %in% miss & !is.na(get(b))], 
                                     Faktor[!get(b) %in% miss & !is.na(get(b))], probs=0.5), digits=0), 
                mean=round(wtd.mean(get(b)[!get(b) %in% miss & !is.na(get(b))], 
                                    Faktor[!get(b) %in% miss & !is.na(get(b))]), digits=1),
                n=sum(!is.na(get(b))),
                nv=sum(!get(b) %in% miss & !is.na(get(b)))) %>%
      mutate(nv = n-nv) %>%
      filter(!is.na(Wert) & !is.na(median))
    
    
    Ergebnis_quer[[b]]$label <- attributes(dat5[[b]])$label
    
  }
  
  print(b)
  
}

saveRDS(Ergebnis_quer, "./Output/Ergebnisse_Selbstaendigkeit_quer.rds")
