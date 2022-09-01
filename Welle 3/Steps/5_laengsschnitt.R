# Längsschnitt

# Querschnittsvergleich
Laengsschnitt <- 
  list(
    Varnames = list(),
    Label = list(),
    Labels = list(),
    Hinweis = list(),
    Tabelle = list(),
    Plot = list()
)

# Größe
dat_long <- dat_long %>% 
  mutate(Alter= cut(S1_w123, breaks=c(0, 30, 45, 60, 100),
                    labels=c("(1) bis 30", "(2) 31 bis 45", "(3) 46 bis 60", "(4) 61 und älter")
  ), 
  Geschlecht = as_factor(S2_w123),
  Alle = "Gesamt"
  )
dat_long$Geschlecht <- recode_factor(dat_long$Geschlecht, Divers = NA_character_)

attributes(dat_long$Alle)$label <- "Alle Personen"
attributes(dat_long$Alter)$label <- "Alter"
attributes(dat_long$Geschlecht)$label <- "Geschlecht"

strata <- c("Alle", "Alter", "Geschlecht")

# missings
for(b in names(dat_long)) {
  
 
    
  labs <- attributes(dat_long[[b]])$labels
  
  ka <- labs[tolower(names(labs)) == "keine angabe"]
  wn <- labs[tolower(names(labs)) == "weiß nicht"]
  
  if(length(ka) > 0) dat_long[[b]][dat_long[[b]] == ka] <- -7
  if(length(wn) > 0) dat_long[[b]][dat_long[[b]] == wn] <- -8
  
  attributes(dat_long[[b]])$labels[attributes(dat_long[[b]])$labels == ka] <- -7
  attributes(dat_long[[b]])$labels[attributes(dat_long[[b]])$labels == wn] <- -8
  
}

for(b in names(dat_long)[-1][!names(dat_long) %in% strata]) {
  
  waves <- str_remove(str_extract(b, "_w[[:digit:]].*"), "_w")
  waves <- unlist(str_split(waves, ""))
  waves <- as.numeric(waves)
  
  if(length(waves) > 1) {
  for(j in waves) {
    
     dat <- dat_long %>%
       filter(Welle == j)
     
  labs <- attributes(dat[[b]])$labels
  x <- dat[[b]]
  gew <- "Faktor_w123"
    #paste0("Faktor__", as.numeric(str_remove(str_extract(b, "__."), "__")))
  
  if(length(unique(dat[[b]])) >= 1) {
    
    if(!is.null(attributes(dat[[b]])$labels)) {
      Laengsschnitt$Labels[[paste0(b, "_t", j)]] <- t(attributes(dat[[b]])$labels)
    }
    
    if(class(dat[[b]]) != "character") {
      
      if(
        (any(nchar(names(attributes(x)$labels)) == 1) |
         (all(is.na(attributes(x)$labels)) & typeof(x) != "character") |
         (length(na.omit(unique(x))) > length(attributes(x)$labels) & typeof(x) != "character")) &
        class(x) != "factor" & class(x) != "Date"
      ){
        Laengsschnitt$Tabelle[[paste0(b, "_t", j)]] <- 
          dat %>% 
          mutate(gewicht = get(gew)) %>%
          filter(!is.na(get(b)))  %>%
          select(b, strata, gewicht) %>%
          gather(key = Variable, value = Ausprägung, -b, -gewicht) %>%
          group_by(Variable, Ausprägung) %>%
          summarise(
            N=sum(get(b) >= 0),
            Ungültig=sum(get(b) <= -7),
            Minimum = min(get(b)[get(b) >= 0], na.rm=T),
            Quartil_1 = wtd.quantile(
              get(b)[get(b) >= 0], gewicht[get(b) >= 0], 
              na.rm=T
            )[2],
            Median = wtd.quantile(
              get(b)[get(b) >= 0], gewicht[get(b) >= 0], 
              na.rm=T
            )[3],
            Mittelwert = wtd.mean(
              get(b)[get(b) >= 0], gewicht[get(b) >= 0], na.rm=T),
            Quartil_3 = wtd.quantile(
              get(b)[get(b) >= 0], gewicht[get(b) >= 0], 
              na.rm=T
            )[4],
            Maximum = max(get(b)[get(b) >= 0], na.rm=T)) %>%
          select(Variable, Ausprägung, N, Ungültig, 
                 Minimum, Quartil_1, Median, Mittelwert, Quartil_3, Maximum) %>%
          filter(!(is.na(Ausprägung))) %>%
          ungroup()
        
        Laengsschnitt$Plot[[paste0(b, "_t", j)]] <- 
          Laengsschnitt$Tabelle[[paste0(b, "_t", j)]] %>%
          filter(Variable %in% strata[1:2]) %>% 
          select(Ausprägung, Median, Mittelwert) %>%
          gather(key = Statistik, value = Wert, -Ausprägung) %>%
          arrange(desc(Ausprägung)) %>%
          ggplot(aes(x=Ausprägung, y=Wert, fill=Statistik)) + 
          
          geom_bar(position=position_dodge2(reverse=F), stat="identity")  +
          labs(x="", y="", title="Werte nach Betriebsgröße") +
          #coord_flip() +
          scale_fill_brewer(palette="Blues") + theme_light()
      } else {
        # Faktoren----
        
        # Umwandeln (Faktor)
        
        miss <- 
          names(attributes(dat[[b]])$labels[attributes(dat[[b]])$labels < 0])
        
        if(is.null(miss)) miss <- "platzhalter"
        
        if(class(dat[[b]]) == "Date") dat[[b]] <- as.factor(dat[[b]])
        dat[[b]] <- as_factor(dat[[b]])
        
        Laengsschnitt$Tabelle[[paste0(b, "_t", j)]] <- 
          dat %>% 
          #mutate(get(b) = as_factor(get(b))) %>%
          mutate(gewicht = get(gew)) %>%
          filter(!is.na(get(b)))  %>%
          select(b, strata, gewicht) %>%
          gather(key = Variable, value = Ausprägung, -b, -gewicht) %>%
          group_by(Variable, Ausprägung, !!b := as_factor(get(b))) %>%
          summarise(weighted_n = sum(gewicht[!get(b) %in% miss]),
                    n=sum(!get(b) %in% miss),
                    un=sum(get(b) %in% miss)) %>%
          mutate(N = sum(n),
                 N_miss = un, # für etwaige separate Auswertung der Missings
                 weighted_group_size = sum(weighted_n),
                 weighted_estimate = weighted_n/weighted_group_size,
                 Ungültig = sum(un)) %>%
          select(Variable, Ausprägung, b, weighted_estimate, N, Ungültig) %>%
          spread(get(b), weighted_estimate) %>%
          select(-one_of(miss)) %>% 
          filter(!(is.na(Ausprägung)))
        
        Laengsschnitt$Plot[[paste0(b, "_t", j)]] <- 
          Laengsschnitt$Tabelle[[paste0(b, "_t", j)]] %>%
          filter(Variable %in% strata[1:2]) %>% 
          select(-N, -Ungültig) %>%
          gather(key = Kategorie, value = Wert, -Ausprägung,- Variable) %>%
          ggplot(aes(x=Ausprägung, y=Wert, fill=Kategorie)) + 
          geom_bar(position="dodge", stat="identity")  +
          labs(x="", y="", title="Anteile nach Betriebsgröße") +
          #coord_flip() +
          scale_fill_brewer(palette="Blues") + theme_light()
        
      }
      
      num <- 1
      if(nrow(Laengsschnitt$Tabelle[[paste0(b, "_t", j)]][,1])) Laengsschnitt$Tabelle[[paste0(b, "_t", j)]][,1] <- ""
      for(i in 1:length(strata)) {
        
        len <- length(unique(na.omit(dat[[strata[i]]][!is.na(dat[[b]])])))
        Laengsschnitt$Tabelle[[paste0(b, "_t", j)]][num,1] <- attributes(dat[[strata[i]]])$label
        num <- num + len
        
      }
      
    }
    
  }
  
  Laengsschnitt$Varnames[[paste0(b, "_t", j)]] <- b
  Laengsschnitt$Label[[paste0(b, "_t", j)]] <- attributes(dat[[b]])$label
  Laengsschnitt$Hinweis[[paste0(b, "_t", j)]] <- attributes(dat[[b]])$quelle
  
  print(c(b, which(names(dat_long) == b)))
}

  }
  
}
saveRDS(Laengsschnitt, paste0("./Output/Laengsschnittse_", Sys.Date(), ".rds")  )


