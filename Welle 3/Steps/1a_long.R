# Longformat----


# Identische Variablen finden----
dat_long <- dat

for(b in names(dat_long)) {
  
  attributes(dat_long[[b]])$label <- 
    str_remove(attributes(dat_long[[b]])$label, "^[^\\s]*\\s")
  
}

panel <- data.frame(
  w1 = names(dat_long),
  w2 = character(length(names(dat_long))),
  w3 = character(length(names(dat_long)))
)


num <- 0

for(b in panel$w1) {
  
  i <- which(names(dat_long) == b)
  num <- num+1
  att <- attributes(dat_long[[b]])$label 
  labels <- attributes(dat_long[[b]])$labels 
  warn <- 1
  
  for(d in names(dat_long)[(i+1):nrow(panel)]){
    
    if(!is.na(d)) {
      
      att2 <- attributes(dat_long[[d]])$label 
      
      if(att == att2) {
        
        if(identical(labels, attributes(dat_long[[d]])$labels) ) {
          warn <- warn+1
          
          welle <- as.numeric(str_remove(d, ".*__"))
          panel[num, welle] <- d
        }
        
        if(warn > 3) print(c(b,d))
        
      }
      
    }
    
  }
  
}

# Datensatz verändern:
# welle 2 in Spalte 2
pos2 <- as.numeric(str_remove(panel$w1, ".*__"))
panel[pos2== 2 & !is.na(pos2), c(1,2)] <- panel[pos2== 2 & !is.na(pos2), c(2,1)]
panel[pos2== 3 & !is.na(pos2), c(1,3)] <- panel[pos2== 3 & !is.na(pos2), c(3,1)]



# Namen ersetzen 
num <- 
  vars <- c(panel$w1, panel$w2, panel$w3)
vars <- unique(vars)
vars <- vars[vars!=""]

for(b in vars) {
  
  
  wave <- na.omit(as.numeric(str_remove(b, ".*__")))
  print(c(wave, b))
  # Welle 1
  if(wave == 1) {
    
    i <- which(panel$w1 == b)
    j <- which(names(dat_long) == b)
    
    if(panel[i,2] == "" & panel[i,3] == "") {
      names(dat_long)[j] <- str_replace(names(dat_long)[j], "__", "_w1__")
    }
    if(panel[i,2] != "" & panel[i,3] == "") {
      names(dat_long)[j] <- str_replace(names(dat_long)[j], "__", "_w12__")
    }
    if(panel[i,2] == "" & panel[i,3] != "") {
      names(dat_long)[j] <- str_replace(names(dat_long)[j], "__", "_w13__")
    }
    if(panel[i,2] != "" & panel[i,3] != "") {
      names(dat_long)[j] <- str_replace(names(dat_long)[j], "__", "_w123__")
    }
    
  }
  
  # Welle 2
  if(wave == 2) {
    
    i <- which(panel$w2 == b)[1]
    j <- which(names(dat_long) == b)
    
    if(panel[i,1] == "" & panel[i,3] == "") {
      names(dat_long)[j] <- str_replace(names(dat_long)[j], "__", "_w2__")
    }
    if(panel[i,1] != "" & panel[i,3] == "") {
      name <- panel[i,1]
      names(dat_long)[j] <- str_replace(name, "__1", "_w12__2")
    }
    if(panel[i,1] != "" & panel[i,3] != "") {
      name <- panel[i,1]
      names(dat_long)[j] <- str_replace(name, "__1", "_w123__2")
    }
    if(panel[i,1] == "" & panel[i,3] != "") {
      names(dat_long)[j] <- str_replace(names(dat_long)[j], "__", "_w23__")
    }
    
  }
  
  # Welle 2
  if(wave == 3) {
    
    i <- which(panel$w3 == b)[1]
    j <- which(names(dat_long) == b)
    
    if(panel[i,1] == "" & panel[i,2] == "") {
      names(dat_long)[j] <- str_replace(names(dat_long)[j], "__", "_w3__")
    }
    if(panel[i,1] != "" & panel[i,2] == "") {
      name <- panel[i,1]
      names(dat_long)[j] <- str_replace(name, "__1", "_w13__3")
    }
    if(panel[i,1] != "" & panel[i,2] != "") {
      name <- panel[i,1]
      names(dat_long)[j] <- str_replace(name, "__1", "_w123__3")
    }
    if(panel[i,1] == "" & panel[i,2] != "") {
      name <- panel[i,2]
      names(dat_long)[j] <- str_replace(name, "__2", "_w23__3")
    }
    
  }
  
  
  
}

dat_long <- dat_long %>% 
  pivot_longer(
    cols = -lfdn_w1__1, 
    names_to = c('.value', 'Welle'), 
    names_sep="__")

dat_long$Welle <- as.numeric(dat_long$Welle)

write_dta(dat_long, "./Daten/Erwerbspersonenbefragung_Wellen1-3_long.dta")
write_sav(dat_long, "./Daten/Erwerbspersonenbefragung_Wellen1-3_long.sav")

dat_long2 <- dat_long

rm(list=setdiff(ls(), c("dat", "dat_long2")))