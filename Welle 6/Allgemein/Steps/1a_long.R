# Longformat aus Abgleich (manuell statt automatisch)

abgleich <- openxlsx::read.xlsx(
  paste0(
    "Input/", 
    "2021-10-21_Erwerbspersonenbefragung_Abgleich-Welle123456.xlsx"
  ), sheet=1, 
  startRow=2
)

names(abgleich) <- c("n1", "l1", "n2", "l2", "n3", "l3", "n4", "l4", "n5", "l5",
                     "n6", "l6", "erh", "iden", "anm")

for(i in 1:nrow(abgleich)) {
  
  for(j in 1:6) {
    
    # replace identisch: nur, wenn auch erhoben!
    abgleich$iden[i] <- 
      paste(intersect(unlist(str_split(abgleich$iden[i], "")), unlist(str_split(abgleich$erh[i], ""))), 
            collapse="")
    
    # Um welche Variable geht es?
    var <- abgleich[i, paste0("n", j)]
    
    # welche Variable im Datensatz heißt so?
    n <- which(names(dat_full) == paste0(var, "__", j))
    
    # Namen hinterlegen für leichtere Programmierung
    name <- names(dat_full)[n]
    
    # Namen ändern
    
    # Wenn j in iden (identisch) 
    if(length(n) > 0 & str_detect(abgleich$iden[i], as.character(j))) {
      
      names(dat_full)[n] <- 
        str_replace(name, paste0("__", j), paste0("_w", abgleich$iden[i], "__", j))
      
      # Namen anpassen an vorherige Welle (bei Namensänderung)
      first <- substr(abgleich$iden[i], 1, 1)
      
      names(dat_full)[n] <- str_replace(names(dat_full)[n], abgleich[i,paste0("n", j)], abgleich[i, paste0("n", first)])
      attributes(dat_full[[names(dat_full)[n]]]) <- 
        attributes(dat_full[[str_replace(names(dat_full)[n], paste0("__", j), paste0("__", first))]])
      
    }
    
    # Wenn nicht: eigene Variable
    if(length(n) > 0 & !str_detect(abgleich$iden[i], as.character(j))) {
      
      names(dat_full)[n] <- 
        str_replace(name, paste0("__", j), paste0("_w", j, "__", j))
      
    }
    
    # Wenn Variable existiert, aber nicht erhoben wurde: flag!
    if(length(n) > 0 & !str_detect(abgleich$erh[i], as.character(j))) {
      
      names(dat_full)[n] <- paste(names(dat_full)[n], "_flag")
      
    }
    
    
  }
  
  print(i)
  
}

dat_full <- dat_full %>%
  select(-ends_with("flag"))


#dat_long 
dat_long_full <- dat_full %>% 
  pivot_longer(
    cols = -ID, 
    names_to = c('.value', 'Welle'), 
    names_sep="__")

dat_long_full$Welle <- as.numeric(dat_long_full$Welle)

# fehlende Attribute?
which(sapply(dat_long_full, function(x) is.null(attributes(x))))
which(sapply(dat_long_full, function(x) is.null(attributes(x)$label)))

attributes(dat_long_full$ID)$label <- "Personen-ID (unveränderlich)"
attributes(dat_long_full$Welle)$label <- "Befragungswelle"
attributes(dat_long_full$S7_w12)$label <- "Berufsbezeichnung (offen)"
attributes(dat_long_full$Faktor_w123456)$label <- "Wellenspezifischer Gewichtungsfaktor"
attributes(dat_long_full$lfdn_W2_w23456)$label <- "Wellenspezifische ID"
attributes(dat_long_full$Interviewtag_w23456)$label <- "Datum des Interviews"
# etwas erstaunlich: A2o_1_w35 und A2o3_1_w35 ohne Label
attributes(dat_long_full$A2o2_1_w35)$label <- "A2o2 Kinderkrankentage: (Anzahl an Tagen)"
attributes(dat_long_full$A2o3_1_w35)$label <- "A2o3 Kinderkrankentage Partner: (Anzahl an Tagen)"
attributes(dat_long_full$ziehung_w5)$label <- "Nachziehung"

attributes(dat_long_full$ziehung_w5)$labels <- 
  setNames(c(0,1), c("Ursprüngliche Stichprobe (Welle 1)", "Nachziehung (Welle 5)")) 

# Faktor-Verwechslung vermeiden





