wb <- createWorkbook()

for(b in names(Ergebnis)) {
  
  addWorksheet(wb, b) 
  
  #createStyle
  writeData(wb, b, Ergebnis[[b]]$label, startCol=1, startRow=1)
  writeDataTable(wb, b, Ergebnis[[b]]$table, startCol=1, startRow =3)
  
}

saveWorkbook(wb, file=paste0("./Output/", Sys.Date(), "_Impuls.xlsx"), overwrite=T)

