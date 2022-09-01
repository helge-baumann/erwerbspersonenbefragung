wb <- createWorkbook()

for(b in names(Ergebnis)) {
  
  addWorksheet(wb, b) 
  
  nonselbst <- which(Ergebnis[[b]]$table[,2] == "Arbeiter/in, Angestellte/r, Beamte/r")
  selbst <- which(Ergebnis[[b]]$table[,2] == "Selbständig")
  
  #createStyle
  writeData(wb, b, Ergebnis[[b]]$label, startCol=1, startRow=1)
  writeDataTable(wb, b, Ergebnis[[b]]$table, startCol=1, startRow =3)
  
}

saveWorkbook(wb, file=paste0("./Output/", Sys.Date(), "_Selbstaendige.xlsx"), overwrite=T)
