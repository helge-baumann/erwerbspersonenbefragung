# Analyse Querschnitt 5. Welle

style_text <- createStyle(textDecoration="bold", fontColour="blue")
style_flag <- createStyle(textDecoration="bold", fontColour="red")
style_bold <- createStyle(textDecoration="bold")

wb <- createWorkbook()

dat$Aufstockung[is.na(dat$Aufstockung)] <- 0
Ergebnis <- list()
vars <- names(dat)
vars <- vars[vars != "Faktor" & vars != "Faktor2"]

as_factor_date <- function(x) {
  
  if(class(x)[1] == "Date") return(x) else return(as_factor(x))
  
}


for(b in vars) {
  
  if(length(unique(dat[[b]])) < 20) {
    
    Ergebnis[[b]]$table1 <- dat %>%
      filter(!is.na(lfdn_W1)) %>%
      select(b, Faktor) %>%
      filter(!is.na(get(b))) %>%
      mutate(!!b := as_factor_date(get(b))) %>%
      group_by(get(b)) %>%
      summarise(anteil = sum(Faktor), n=n()) %>%
      mutate(anteil=(anteil/sum(anteil)*100), n=sum(n)) %>%
      rename(!!b := `get(b)`) 
    
    Ergebnis[[b]]$table2 <- dat %>%
      filter(!is.na(lfdn_W1)) %>%
      select(b, Faktor2) %>%
      filter(!is.na(get(b))) %>%
      mutate(!!b := as_factor_date(get(b))) %>%
      group_by(get(b)) %>%
      summarise(anteil = sum(Faktor2), n=n()) %>%
      mutate(anteil=(anteil/sum(anteil)*100), n=sum(n)) %>%
      rename(!!b := `get(b)`) 
    
    Ergebnis[[b]]$table3 <- dat %>%
      select(b, Faktor) %>%
      filter(!is.na(get(b))) %>%
      #filter(!is.na(lfdn_W1)) %>%
      mutate(!!b := as_factor_date(get(b))) %>%
      group_by(get(b)) %>%
      summarise(anteil = sum(Faktor), n=n()) %>%
      mutate(anteil=(anteil/sum(anteil)*100), n=sum(n)) %>%
      rename(!!b := `get(b)`) 
    
    Ergebnis[[b]]$label <- attributes(dat[[b]])$label
    Ergebnis[[b]]$name <- b
    
    Ergebnis[[b]]$flag <- integer(0)
    num <- 0
    for(i in 1:nrow(Ergebnis[[b]]$table1)) {
      
      if(abs(Ergebnis[[b]]$table1$anteil[i] - Ergebnis[[b]]$table2$anteil[i]) >= 0.5) {
        num <- num+1
      Ergebnis[[b]]$flag[num] <- i
      
      }
    }
    
  }
  
  if(length(unique(dat[[b]])) >= 20 & class(dat[[b]])[1] != "character") {
    
    miss <- attributes(dat[[b]])$labels[
      str_detect(tolower(names(attributes(dat[[b]])$labels)), "^keine angabe") |
        str_detect(tolower(names(attributes(dat[[b]])$labels)), "^weiß nicht") 
    ]
    
    Ergebnis[[b]]$table1 <- dat %>%
      filter(!is.na(lfdn_W1)) %>%
      select(b, Faktor) %>%
      filter(!is.na(get(b))) %>%
      summarise(median = 
                  round(wtd.quantile(get(b)[!get(b) %in% miss & !is.na(get(b))], 
                                     Faktor[!get(b) %in% miss & !is.na(get(b))], probs=0.5), digits=0), 
                mean=round(wtd.mean(get(b)[!get(b) %in% miss & !is.na(get(b))], 
                                    Faktor[!get(b) %in% miss & !is.na(get(b))]), digits=1),
                n=sum(!is.na(get(b))),
                nv=sum(!get(b) %in% miss & !is.na(get(b)))) %>%
      mutate(nv = n-nv) %>%
      filter(!is.na(median))
    
    Ergebnis[[b]]$table2 <- dat %>%
      filter(!is.na(lfdn_W1)) %>%
      select(b, Faktor2) %>%
      filter(!is.na(get(b))) %>%
      summarise(median = 
                  round(wtd.quantile(get(b)[!get(b) %in% miss & !is.na(get(b))], 
                                     Faktor2[!get(b) %in% miss & !is.na(get(b))], probs=0.5), digits=0), 
                mean=round(wtd.mean(get(b)[!get(b) %in% miss & !is.na(get(b))], 
                                    Faktor2[!get(b) %in% miss & !is.na(get(b))]), digits=1),
                n=sum(!is.na(get(b))),
                nv=sum(!get(b) %in% miss & !is.na(get(b)))) %>%
      mutate(nv = n-nv) %>%
      filter(!is.na(median)) 
    
    Ergebnis[[b]]$table3 <- dat %>%
      select(b, Faktor) %>%
      filter(!is.na(get(b))) %>%
      #filter(!is.na(lfdn_W1)) %>%
      summarise(median = 
                  round(wtd.quantile(get(b)[!get(b) %in% miss & !is.na(get(b))], 
                                     Faktor[!get(b) %in% miss & !is.na(get(b))], probs=0.5), digits=0), 
                mean=round(wtd.mean(get(b)[!get(b) %in% miss & !is.na(get(b))], 
                                    Faktor[!get(b) %in% miss & !is.na(get(b))]), digits=1),
                n=sum(!is.na(get(b))),
                nv=sum(!get(b) %in% miss & !is.na(get(b)))) %>%
      mutate(nv = n-nv) %>%
      filter(!is.na(median))
    
    Ergebnis[[b]]$label <- attributes(dat[[b]])$label
    Ergebnis[[b]]$name <- b
    
    print(paste0(b, " (numeric)"))
    
    Ergebnis[[b]]$flag <- integer(0)
    num <- 0
    for(i in Ergebnis[[b]]$table1[2]) {
      
      if(abs(1-Ergebnis[[b]]$table1[2] /Ergebnis[[b]]$table2[2]) > 0.01)
        num <- num+1
      Ergebnis[[b]]$flag[num] <- i
      
    }
    
  }
  
  
  print(b)
  
  if(!is.null(Ergebnis[[b]])) {
  addWorksheet(wb, b)
  n <- ncol(Ergebnis[[b]]$table1)
  writeData(wb, b, Ergebnis[[b]]$name, startCol= 1, startRow=1)
  addStyle(wb, b, style_text, rows=1, cols=1, gridExpand=T)
  writeData(wb, b, Ergebnis[[b]]$label, startCol= 1, startRow=2)
  addStyle(wb, b, style_text, rows=2, cols=1, gridExpand=T)
  writeData(wb, b, "alter Faktor, alte Panelisten:", startCol= 1, startRow=4)
  writeData(wb, b, Ergebnis[[b]]$table1, startCol= 1, startRow=6, borders="surrounding")
  writeData(wb, b, "neuer Faktor, alte Panelisten:", startCol= n+2, startRow=4)
  writeData(wb, b, Ergebnis[[b]]$table2, startCol= n+2, startRow=6, borders="surrounding")
  writeData(wb, b, "alter Faktor, alle Befragten:", startCol= n*2+3, startRow=4)
  writeData(wb, b, Ergebnis[[b]]$table3, startCol= n*2+3, startRow=6, borders="surrounding")
  
  addStyle(wb, b, style_bold, rows=6, cols=1:(n*4), gridExpand=T)
  
  if(length(Ergebnis[[b]]$flag > 0)) {
    
    for(j in Ergebnis[[b]]$flag) {
      
      addStyle(wb, b, style_flag, rows=6+j, cols=n+3, gridExpand=T)
      
    }
    
  }
  }
  
}

saveWorkbook(wb, "Output/Test_Gewichte.xlsx", overwrite=T)




