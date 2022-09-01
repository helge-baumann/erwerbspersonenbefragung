# Längsschnitt

crosstab <- list()


dat$D4_w2 <- as_factor(dat$D4_w2)
attributes(dat$D4_w2)$levels[10] <- "4.500 EURO und mehr"
dat$D4_w2[dat$D4_w2 == "6.000 Euro und mehr"] <- "4.500 EURO und mehr"
attributes(dat$D4_w2)$levels <- attributes(dat$D4_w2)$levels[-11]


for(b in vars) {
  
  name <- str_remove(b, fixed("_w2"))
  
  if(meta$identisch[(meta$n2 == name) %in% T] == 1 & !class(dat[[b]]) == "character") {
    
  
  
  name1 <- meta$n1[(meta$n2 == name) %in% T]
  b1 <- paste0(meta$n1[(meta$n2 == name) %in% T], fixed("_w1"))
  
  if(length(unique(dat[[b]])) > 20) {
   
    dat[[b]][tolower(as_factor(dat[[b]])) %in% miss] <- NA
    q <- quantile(dat[[b]], na.rm=T)
    dat[[b]] <- cut(dat[[b]], breaks=unname(q))
    dat[[b1]] <- cut(dat[[b1]], breaks=q)
     
  }
    
  
  
  
    
  crosstab[[b]] <- table(as_factor(dat[[b1]]), as_factor(dat[[b]]))
  crosstab[[b]] <- crosstab[[b]][, !str_detect(colnames(crosstab[[b]]), "Zuspiel aus")]
  if(nrow(crosstab[[b]]) != ncol(crosstab[[b]])) print(b)
  }
  
}
  
  