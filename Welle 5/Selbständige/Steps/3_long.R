# Analyse Längsschnitt 5. Welle

Ergebnis_laengs <- list()

as_factor_date <- function(x) {
  
  if(class(x)[1] == "Date") return(x) else return(as_factor(x))
  
}

dat <- dat %>% 
  mutate(Welle = as.character(Welle)) %>%
  group_by(lfdn_w1__1) %>%
  arrange(lfdn_w1__1, Welle) %>%
  mutate(t = S6_w45[5], faktor_w5 = Faktor2_w5[5], solo=W5_S6h_w5[5]) %>%
  mutate(
    selbst = case_when(
      t %in% c(1:3) ~ "Arbeiter/in, Angestellte/r, Beamte/r",
      t %in% c(4:6) ~ "Selbständig",
      t %in% c(7, 99) | is.na(t) ~ NA_character_),
    selbst_solo = 
      case_when(
        t %in% c(1:3) ~ "Arbeiter/in, Angestellte/r, Beamte/r",
        t %in% c(4:6) & solo == 1 ~ "Selbständig ohne Mitarbeiter:innen",
        t %in% c(4:6) & solo == 2 ~ "Selbständig mit Mitarbeiter:innen",
        t %in% c(7, 99) | is.na(t) ~ NA_character_)
  ) %>%
  ungroup()

for(b in setdiff(
  names(dat), 
  c("ldfn_w1", "Welle", "faktor_w5", "selbst", "selbst_solo", "Interviewtag_w2345"))
  ) {
  
  # kategorial
  if(length(unique(dat[[b]])) < 20 & class(dat[[b]])[1] != "character") {
    
  Ergebnis_laengs[[b]]$table <- dat %>%
    select(starts_with("selbst"), b, faktor_w5, Welle) %>%
    pivot_longer(
      cols=-c(b, "faktor_w5", "Welle"), names_to="Variable", values_to="Wert"
      ) %>%
    mutate(!!b := as_factor_date(get(b))) %>%
    group_by(Variable, Wert, Welle, get(b)) %>%
    summarise(
      anteil = sum(faktor_w5[
        !is.na(get(b)) & 
          !str_detect(tolower(get(b)), "^keine angabe") & 
          !str_detect(tolower(get(b)), "^weiß nicht")], na.rm=T), 
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
  
  Ergebnis_laengs[[b]]$label <- attributes(dat[[b]])$label
 
  }
  
  # numerisch
  if(length(unique(dat[[b]])) >= 20 & class(dat[[b]])[1] != "character") {
    
    # missings
    miss <- attributes(dat[[b]])$labels[
      str_detect(tolower(names(attributes(dat[[b]])$labels)), "^keine angabe") |
        str_detect(tolower(names(attributes(dat[[b]])$labels)), "^weiß nicht") 
    ]
    
    Ergebnis_laengs[[b]]$table <- dat %>%
      select(starts_with("selbst"), b, faktor_w5, Welle) %>%
      pivot_longer(
        cols=-c(b, "faktor_w5", "Welle"), names_to="Variable", values_to="Wert"
        ) %>%
      group_by(Variable, Wert, Welle) %>%
      summarise(median = 
                  round(
                    wtd.quantile(
                      get(b)[!get(b) %in% miss & !is.na(get(b))], 
                      faktor_w5[!get(b) %in% miss & !is.na(get(b))], probs=0.5), 
                    digits=0), 
                mean=
                  round(
                    wtd.mean(get(b)[!get(b) %in% miss & !is.na(get(b))], 
                             faktor_w5[!get(b) %in% miss & !is.na(get(b))]), 
                    digits=1),
                n=sum(!is.na(get(b))),
                nv=sum(!get(b) %in% miss & !is.na(get(b)))) %>%
      mutate(nv = n-nv) %>%
      filter(!is.na(Wert) & !is.na(median))
    
    Ergebnis_laengs[[b]]$label <- attributes(dat[[b]])$label
    
  }
  
  print(b)
  
}

saveRDS(Ergebnis_laengs, "./Output/Ergebnisse_Selbstaendigkeit_laengs.rds")
