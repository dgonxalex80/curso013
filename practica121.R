# 1. crear una carpeta : actividad121
# 2. descargar el archivo beer2.zip
# 3. descomprimir archivo beer2.csv
# 4. abrir el archivo en RStudio
# 5. Importar archivo beer2.csv
# 6. Cargar paquete : summarytools, 
# 7. visualizar dataset   : head(beer2)  summarytools::dfSummary
#--------variables cualitativas -------------------------
# 8. crear tabla de la variable beer2$tipo  : table, summarytools::freq
# 9. crear tabla de las variables beer2$tipo, beer2$origen,  summarytoos::ctable
#-------variables cuantitativas ------------------------- 
# 10. indicadores descriptivos variable beer2$precio : summary,  summarytool::desc
# 11. indicadores descrptivos por factor: summarytools::stby(data = beer2$precio,  INDICES = beer2$tipo, FUN = descr)

#===============================================================================
# Graficos
# 12. crea un archivo Rmd 
# graficos variables cualitativas
# 13. t1= table(beer2$tipo) ; pie(t)
# 14. t2 =table(beer2$tipo, beer$origen); barplot(t2) 
# 15. t2 =table(beer2$tipo, beer2$origen); mosaico(t(t2), horiz = TRUE, las=1) 
# graficas variables cuantitativas
# 16. hist(beer2$precio)
# 17. density(beer2$precio)
# 18. boxplot(beer2$precio)
# 19. boxplot(beer2$precio ~ beer2$tipo)


