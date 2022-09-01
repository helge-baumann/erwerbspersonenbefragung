# Vorarbeit----

# Metadaten im seperaten Excel-Dokument
meta <- read.xlsx(
  "../Fragebogen/Metadaten/Abgleich_Variablen_Welle1_2.xlsx",
  sheet = 1,
  startRow = 1, colNames=T)

# Ergebnisframes
Ergebnis <- list()


# welche missings?
miss <- tolower(c("Keine Angabe", "Weiß nicht"))

# welche Variablen analysieren?
vars <- paste0(meta$n2[(meta$erhoben == "beide" | meta$erhoben == "2") &
                         !(meta$Anmerkung == "aus Welle 1 zugespielt") %in% T], 
                       "_w2")
vars <- vars[vars %in% names(dat)]

# Labels und Namen matchen
labs <- sapply(dat, function(x) attributes(x)$label)
labs[sapply(labs, is.null)] <- NA
nameslabs <- data.frame(x=names(labs), y=unlist(labs))

dat$S1_w1 <- as.numeric(as.character(dat$S1_w1))
attributes(dat$S1_w1)$label <- "S1 Alter"
dat$S1_w2 <- as.numeric(as.character(dat$S1_w2))
attributes(dat$S1_w2)$label <- "S2 Alter"


# Analyse----
for(b in vars) {
  
  # alle Werte gelabelt?
  fac <- all(na.omit(dat[[b]]) %in% attributes(dat[[b]])$labels) &
    (all(nchar(names(attributes(dat[[b]])$labels)) > 1) |
    length(table(dat[[b]]) < 6)) | 
    class(dat[[b]]) == "Date"
  
  b1 <- meta$n1[(meta$n2 == str_remove(b, fixed("_w2"))) %in% T]
  
  # Faktorvariablen
  if(fac) {     
    
    if(class(dat[[b]]) == "Date") dat[[b]] <- as.character(dat[[b]])
    dat[[b]] <- as_factor(dat[[b]])

    # Ergebnisframe
    
    # Welle 2
    Ergebnis[[b]] <- dat %>% 
      filter(!is.na(Faktor_w2))  %>%
      select(b, strata2, Faktor_w2) %>%
      gather(key = Variable, value = Ausprägung, -b, -Faktor_w2) %>%
      group_by(Variable, Ausprägung, !!b := as_factor(get(b))) %>%
      summarise(weighted_n = sum(Faktor_w2[!tolower(get(b)) %in% miss & !is.na(get(b))]),
                n=sum(!is.na(get(b))),
                g=sum(!tolower(get(b)) %in% miss & !is.na(get(b))),
                un=sum(tolower(get(b)) %in% miss),
                miss=sum(is.na(get(b)))) %>%
      mutate(N = sum(n),
             N_miss = un, # für etwaige separate Auswertung der Missings
             weighted_group_size = sum(weighted_n),
             weighted_estimate = weighted_n/weighted_group_size,
             Gültig = sum(g), Ungültig = sum(un), miss=sum(miss)) %>%
      select(Variable, Ausprägung, b, weighted_estimate, N, Gültig, Ungültig, miss) %>%
      spread(get(b)[!is.na(get(b))], weighted_estimate) %>%
      select(-one_of(miss)) %>% 
      filter(!(is.na(Ausprägung)))
    
    Ergebnis[[b]]$Variable <- 
      plyr::mapvalues(Ergebnis[[b]]$Variable, 
                      from = as.character(nameslabs$x), 
                      to = as.character(nameslabs$y),
                      warn_missing = F)
    
    # Welle 1
    
    if(!is.na(b1)) {
      b1 <- paste0(b1, fixed("_w1"))
      print(b1)
      
      
    Ergebnis1 <- dat %>% 
      filter(!is.na(Faktor_w1))  %>%
      select(b1, strata1, Faktor_w1) %>%
      gather(key = Variable, value = Ausprägung, -b1, -Faktor_w1) %>%
      group_by(Variable, Ausprägung, !!b1 := as_factor(get(b1))) %>%
      summarise(weighted_n = sum(Faktor_w1[!tolower(get(b1)) %in% miss & !is.na(get(b1))]),
                n=sum(!is.na(get(b1))),
                g=sum(!tolower(get(b1)) %in% miss & !is.na(get(b1))),
                un=sum(tolower(get(b1)) %in% miss),
                miss=sum(is.na(get(b1)))) %>%
      mutate(N = sum(n),
             N_miss = un, # für etwaige separate Auswertung der Missings
             weighted_group_size = sum(weighted_n),
             weighted_estimate = weighted_n/weighted_group_size,
             Gültig = sum(g), Ungültig = sum(un), miss=sum(miss)) %>%
      select(Variable, Ausprägung, b1, weighted_estimate, N, Gültig, Ungültig, miss) %>%
      spread(get(b1)[!is.na(get(b1))], weighted_estimate) %>%
      select(-one_of(miss)) %>% 
      filter(!(is.na(Ausprägung)))
    
    Ergebnis1$Variable <- 
      plyr::mapvalues(Ergebnis1$Variable, 
                      from = as.character(nameslabs$x), 
                      to = as.character(nameslabs$y),
                      warn_missing = F)
    
    Ergebnis[[b]] <- 
      merge(Ergebnis[[b]], Ergebnis1, by=c("Variable", "Ausprägung"), all=T)
    
    }
    
    # numerische Variablen
  } else {
      
    if(!(class(dat[[b]]) == "character")) { # vs offene Nennungen (S7)
      
      if(any(attributes(dat[[b]])$labels == 2 
             & names(attributes(dat[[b]])$labels) == "1")) {
        
        dat[[b]] <- dat[[b]] - 1
        
      }
      
    Ergebnis[[b]] <- 
      dat %>% 
      filter(!is.na(Faktor_w2))  %>%
      select(b, strata2, Faktor_w2) %>%
      gather(key = Variable, value = Ausprägung, -b, -Faktor_w2) %>%
      filter(!is.na(Ausprägung)) %>%
      group_by(Variable, Ausprägung) %>%
      mutate(n = n()) %>%
      filter(n > 5) %>%
      summarise(
        N=sum(!is.na(get(b))),
        Ungültig=sum(tolower(as_factor(get(b))) %in% miss & !is.na(get(b))),
        Gültig=sum(!tolower(as_factor(get(b))) %in% miss & !is.na(get(b))),
        miss=sum(is.na(get(b))),
        Minimum = min(get(b), na.rm=T),
        Quartil_1 = wtd.quantile(
          get(b)[!tolower(as_factor(get(b))) %in% miss], 
          Faktor_w2[!tolower(as_factor(get(b))) %in% miss], 
          na.rm=T
        )[2],
        Median = wtd.quantile(
          get(b)[!tolower(as_factor(get(b))) %in% miss], 
          Faktor_w2[!tolower(as_factor(get(b))) %in% miss], 
          na.rm=T
        )[3],
        Mittelwert = wtd.mean(
          get(b)[!tolower(as_factor(get(b))) %in% miss], 
          Faktor_w2[!tolower(as_factor(get(b))) %in% miss], 
          na.rm=T),
        Quartil_3 = wtd.quantile(
          get(b)[!tolower(as_factor(get(b))) %in% miss], 
          Faktor_w2[!tolower(as_factor(get(b))) %in% miss], 
          na.rm=T
        )[4],
        Maximum = max(get(b)[!tolower(as_factor(get(b))) %in% miss], na.rm=T)) %>%
      select(Variable, Ausprägung, N, Ungültig, Gültig, miss,
             Minimum, Quartil_1, Median, Mittelwert, Quartil_3, Maximum) %>%
      filter(!(is.na(Ausprägung))) %>%
      ungroup()
    
    Ergebnis[[b]]$Variable <- 
      plyr::mapvalues(Ergebnis[[b]]$Variable, 
                      from = as.character(nameslabs$x), 
                      to = as.character(nameslabs$y),
                      warn_missing = F)
    
    if(!is.na(b1)) {
      b1 <- paste0(b1, fixed("_w1"))
      print(b1)
      
      if(any(attributes(dat[[b1]])$labels == 2 
             & names(attributes(dat[[b1]])$labels) == "1")) {
        
        dat[[b1]] <- dat[[b1]] - 1
        
      }
      
      Ergebnis1 <- 
        dat %>% 
        filter(!is.na(Faktor_w1))  %>%
        select(b1, strata1, Faktor_w1) %>%
        gather(key = Variable, value = Ausprägung, -b1, -Faktor_w1) %>%
        filter(!is.na(Ausprägung)) %>%
        group_by(Variable, Ausprägung) %>%
        mutate(n = n()) %>%
        filter(n > 5) %>%
        summarise(
          N=sum(!is.na(get(b1))),
          Ungültig=sum(tolower(as_factor(get(b1))) %in% miss & !is.na(get(b1))),
          Gültig=sum(!tolower(as_factor(get(b1))) %in% miss & !is.na(get(b1))),
          miss=sum(is.na(get(b1))),
          Minimum = min(get(b1), na.rm=T),
          Quartil_1 = wtd.quantile(
            get(b1)[!tolower(as_factor(get(b1))) %in% miss], 
            Faktor_w1[!tolower(as_factor(get(b1))) %in% miss], 
            na.rm=T
          )[2],
          Median = wtd.quantile(
            get(b1)[!tolower(as_factor(get(b1))) %in% miss], 
            Faktor_w1[!tolower(as_factor(get(b1))) %in% miss], 
            na.rm=T
          )[3],
          Mittelwert = wtd.mean(
            get(b1)[!tolower(as_factor(get(b1))) %in% miss], 
            Faktor_w1[!tolower(as_factor(get(b1))) %in% miss], 
            na.rm=T),
          Quartil_3 = wtd.quantile(
            get(b1)[!tolower(as_factor(get(b1))) %in% miss], 
            Faktor_w1[!tolower(as_factor(get(b1))) %in% miss], 
            na.rm=T
          )[4],
          Maximum = max(get(b1)[!tolower(as_factor(get(b1))) %in% miss], na.rm=T)) %>%
        select(Variable, Ausprägung, N, Ungültig, Gültig, miss,
               Minimum, Quartil_1, Median, Mittelwert, Quartil_3, Maximum) %>%
        filter(!(is.na(Ausprägung))) %>%
        ungroup()
      
      Ergebnis1$Variable <- 
        plyr::mapvalues(Ergebnis1$Variable, 
                        from = as.character(nameslabs$x), 
                        to = as.character(nameslabs$y),
                        warn_missing = F)
      
      Ergebnis[[b]] <- 
        merge(Ergebnis[[b]], Ergebnis1, by=c("Variable", "Ausprägung"), all=T)
      
      
    }
    
    }
    
    
  }
  print(b)
}