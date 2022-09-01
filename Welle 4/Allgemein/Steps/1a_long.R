# Longformat aus Abgleich (manuell statt automatisch)

abgleich <- read.xlsx(
  paste0(
  "../../../_Finales Material/Erläuterungen und Metadaten/Abgleich Variablen/", 
  "2021-02-12_Erwerbspersonenbefragung_Abgleich-Welle1234.xlsx"
  ), 
  startRow=2
)

names(abgleich) <- c("n1", "l1", "n2", "l2", "n3", "l3", "n4", "l4", "erh", "iden", "anm")

for(i in 1:nrow(abgleich)) {
  
  for(j in 1:4) {
    
    # replace identisch: nur, wenn auch erhoben!
   abgleich$iden[i] <- 
     paste(intersect(unlist(str_split(abgleich$iden[i], "")), unlist(str_split(abgleich$erh[i], ""))), 
           collapse="")
    
    # Um welche Variable geht es?
    var <- abgleich[i, paste0("n", j)]
    
    # welche Variable im Datensatz heißt so?
    n <- which(names(dat_ges) == paste0(var, "__", j))
    
    # Namen hinterlegen für leichtere Programmierung
    name <- names(dat_ges)[n]
    
    # Namen ändern
    
    # Wenn j in iden (identisch) 
    if(length(n) > 0 & str_detect(abgleich$iden[i], as.character(j))) {
      
      names(dat_ges)[n] <- 
        str_replace(name, paste0("__", j), paste0("_w", abgleich$iden[i], "__", j))
      
      # Namen anpassen an vorherige Welle (bei Namensänderung)
      first <- substr(abgleich$iden[i], 1, 1)
      
        names(dat_ges)[n] <- str_replace(names(dat_ges)[n], abgleich[i,paste0("n", j)], abgleich[i, paste0("n", first)])
        attributes(dat_ges[[names(dat_ges)[n]]]) <- 
          attributes(dat_ges[[str_replace(names(dat_ges)[n], paste0("__", j), paste0("__", first))]])
      
    }
    
    # Wenn nicht: eigene Variable
    if(length(n) > 0 & !str_detect(abgleich$iden[i], as.character(j))) {
      
      names(dat_ges)[n] <- 
        str_replace(name, paste0("__", j), paste0("_w", j, "__", j))
      
    }
    
    # Wenn Variable existiert, aber nicht erhoben wurde: flag!
    if(length(n) > 0 & !str_detect(abgleich$erh[i], as.character(j))) {
      
      names(dat_ges)[n] <- paste(names(dat_ges)[n], "_flag")
      
    }
    
    
  }
  
  print(i)
  
}

dat_ges <- dat_ges %>%
  select(-ends_with("flag"))



#attributes(dat_ges$A2d_w234__4) <- attributes(dat_ges$A2d_w234__3)

dat_long <- dat_ges %>% 
  pivot_longer(
    cols = -lfdn_w1__1, 
    names_to = c('.value', 'Welle'), 
    names_sep="__")


dat_long$Welle <- as.numeric(dat_long$Welle)

# fehlende Attribute?
which(sapply(dat_long, function(x) is.null(attributes(x))))

attributes(dat_long$lfdn_w1__1)$label <- "Personen-ID (unveränderlich)"
attributes(dat_long$Welle)$label <- "Befragungswelle"
attributes(dat_long$S7_w12)$label <- "Berufsbezeichnung (offen)"
attributes(dat_long$Faktor_w1234)$label <- "Wellenspezifischer Gewichtungsfaktor"
attributes(dat_long$lfdn_W2_w234)$label <- "Wellenspezifische ID"
attributes(dat_long$Interviewtag_w234)$label <- "Datum des Interviews"

write_dta(dat_long, "./Daten_Export/Erwerbspersonenbefragung_Wellen1-4_long_v1-1.dta")
write_sav(dat_long, "./Daten_Export/Erwerbspersonenbefragung_Wellen1-4_long_v1-1.sav")
