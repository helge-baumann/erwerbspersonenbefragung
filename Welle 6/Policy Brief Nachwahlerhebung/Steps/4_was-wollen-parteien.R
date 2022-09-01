Ergebnis <- list()

wb <- createWorkbook(wb)

for(b in names(dat)[
  str_detect(names(dat), "^W6_[3-8]")]) {
  
  valid <- attributes(dat[[b]])$labels[
    !str_detect(tolower(names(attributes(dat[[b]])$labels)), "weiß nich|keine an")]
  invalid <- attributes(dat[[b]])$labels[
    str_detect(tolower(names(attributes(dat[[b]])$labels)), "weiß nich|keine an")]
  
  # numerisch
  if(length(unique(dat[[b]])) > 6) {
    
    Ergebnis[[b]]$table <- 
      dat %>%
      pivot_longer(
        c(`(1) gesamt`, gewerkschaft), 
        names_to="Variable", values_to = "Wert") %>%
      group_by(as_factor(W6_2_w6), Variable, Wert) %>%
      summarise(
        mean = round(wtd.mean(
          get(b)[get(b) %in% valid], Faktor_w123456[get(b) %in% valid]), 
          2),
        N = sum(get(b) %in% valid, na.rm=T),
        invalid = sum(get(b) %in% invalid, na.rm=T)) %>%
      filter(Wert != "Keine Angabe")
    
  } else {
    
    Ergebnis[[b]]$table <- 
      dat %>%
      pivot_longer(
        c(`(1) gesamt`, gewerkschaft), 
        names_to="Variable", values_to = "Wert") %>%
      mutate(Antwort = as_factor(get(b))) %>%
      group_by(as_factor(W6_2_w6), Variable, Wert, Antwort) %>%
      summarise(
        n = sum(Faktor_w123456[get(b) %in% valid]), 
        invalid = sum(get(b) %in% invalid, na.rm=T),
        valid= sum(get(b) %in% valid, na.rm=T)) %>%
      mutate(
        p = round(n/sum(n)*100, 1), 
        N=sum(valid), 
        invalid=sum(invalid)) %>%
      filter(!str_detect(tolower(Antwort), "eiß ni|keine an")) %>%
      select(-c(n, valid)) %>%
      pivot_wider(names_from=Antwort, values_from=p)
    
  }
  
  addWorksheet(wb, b)
  writeData(wb, b, paste0(b, ": ", attributes(dat[[b]])$label))
  writeData(wb, b, t(attributes(dat[[b]])$labels), startRow=3)
  
  writeDataTable(wb, b, Ergebnis[[b]]$table, startRow=6)
  
}

saveWorkbook(wb, paste0("Output/", Sys.Date(), "/", Sys.Date(), 
                        "_Partei_politikpräferenzen.xlsx"), overwrite=T)
