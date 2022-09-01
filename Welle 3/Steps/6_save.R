wb <- createWorkbook()

headerStyle <- createStyle(fontSize = 12, fontColour = "red", halign = "left",
                           textDecoration="bold")
genStyle <-  createStyle(fontSize = 10, fontColour = "blue", halign = "left",
                         textDecoration="italic")

posStyle <- createStyle(fontSize = 10, fontColour = "blue", halign = "right")
negStyle <- createStyle(fontSize = 10, fontColour = "red", halign = "right")

posBorder <- createStyle(borderColour = "green", border="TopBottomLeftRight", borderStyle="double")
negBorder <- createStyle(borderColour = "red", border="TopBottomLeftRight", borderStyle="double")
  
  

# welche sollen abgedruckt werden?

selection <- names(data)[!names(data) %in% strata]
  #str_detect(names(data), "^C")
  #]

# Inhaltsverzeichnis
addWorksheet(wb, "Inhalt")

writeData(
  wb, "Inhalt",
   "Inhaltsverzeichnis",
  startCol=1, startRow=1
)

num <- 0

for(b in names(data)[!names(data) %in% strata]) {
  
  addWorksheet(wb, paste0(num, "_", substr(b, 1, 16)))

  writeData(wb, paste0(num, "_", substr(b, 1, 16)), Ergebnis$Label[[b]], startCol=1, startRow=1)
  writeData(wb, paste0(num, "_", substr(b, 1, 16)), Ergebnis$Hinweis[[b]], startCol=1, startRow =2)
  writeData(wb, paste0(num, "_", substr(b, 1, 16)), "Wertelabels:", startCol=1, startRow =4)
  writeData(wb, paste0(num, "_", substr(b, 1, 16)), Ergebnis$Labels[[b]], startCol=2, startRow =5)
  # Tabelle
  if(!is.null(Ergebnis$Tabelle[[b]])) {
    while(any(duplicated(tolower(names(Ergebnis$Tabelle[[b]]))))) {
    names(Ergebnis$Tabelle[[b]])[duplicated(tolower(names(Ergebnis$Tabelle[[b]])))] <- 
      paste0(names(Ergebnis$Tabelle[[b]])[duplicated(tolower(names(Ergebnis$Tabelle[[b]])))], "_x")
    }
    writeDataTable(wb, paste0(num, "_", substr(b, 1, 16)), Ergebnis$Tabelle[[b]], startCol=1, startRow =26,
                 firstColumn = T, tableStyle="TableStyleMedium2")
  addStyle(wb, paste0(num, "_", substr(b, 1, 16)), negStyle, rows=26:45, cols=4)
  }
  
  print(Ergebnis$Plot[[b]])
  
  insertPlot(wb, paste0(num, "_", substr(b, 1, 16)), 
             startRow = 8, startCol = 1, 
             width = 7, height = 3, fileType = "png", units = "in", dpi=180)

  addStyle(wb, paste0(num, "_", substr(b, 1, 16)), headerStyle, rows=1, cols=1)
  addStyle(wb, paste0(num, "_", substr(b, 1, 16)), genStyle, rows=c(3), cols=1:100)
  
  # set style for value labels
  if(!is.null(Ergebnis$Labels[[b]])) {
    for(j in 1:length(Ergebnis$Labels[[b]])) {
      if(Ergebnis$Labels[[b]][j] < 0) {
      addStyle(wb, paste0(num, "_", substr(b, 1, 16)), negStyle, rows=5:6, cols=j+1)
    } else {
      addStyle(wb, paste0(num, "_", substr(b, 1, 16)), posStyle, rows=5:6, cols=j+1)
    }
    }
  }
  
  # set style for cells
  if(!is.null(Ergebnis$Tabelle[[b]])) {
  check <- Ergebnis$Tabelle[[b]]
  check[,1:4] <- NA
  for(i in 2:nrow(check)) {
    for(j in 5:ncol(check)) {
      
      abstand <- (check[i,j]-check[1,j])/check[i,j]
      
      if(abstand <= -0.2 & !is.na(abstand)) addStyle(wb, paste0(num, "_", substr(b, 1, 16)), negBorder, rows=i+26, cols=j)
      if(abstand >= 0.2 & !is.na(abstand)) addStyle(wb, paste0(num, "_", substr(b, 1, 16)), posBorder, rows=i+26, cols=j)
      
      }
    }
  
  
  setColWidths(wb, paste0(num, "_", substr(b, 1, 16)), cols=1, widths = 16)
  setColWidths(wb, paste0(num, "_", substr(b, 1, 16)), cols=2, widths = 32)
  }
  print(c(b, which(names(data) == b)))
  
  num <- num+1
  
  if(num %in% seq(from=1, to=ncol(data)-20, by=20)) {
    
    num <- num+1
    
    writeData(wb, "Inhalt", 
              paste0(paste0(num, "_", substr(b, 1, 16)), " - ", selection[which(selection == b)+17]), startCol=1, startRow=num+2)
    
    num <- num+1
    
  }
  
  
  writeFormula(
    wb, "Inhalt", 
    makeHyperlinkString(paste0(num, "_", substr(b, 1, 16)), row = 1, col = 1, text = paste0(num, "_", substr(b, 1, 16)), file = NULL),
    startCol = 1, startRow = num+2
  )
  writeData(
    wb, "Inhalt",
    Ergebnis$Label[[b]], 
    startCol=2, startRow=num+2
  )
  
  
}



saveWorkbook(wb, "./Output/Output_20201124.xlsx", overwrite=T)

names_labels <- data.frame(name=NA, label=NA)
num <- 0

for(b in selection) {
  
  num <- num+1
  names_labels[num,1] <- b
  if(!is.null(attributes(data[[b]])$label)) {
  names_labels[num,2] <- attributes(data[[b]])$label
  }
  
}

write.csv2(names_labels, "./Output/namen.csv")

