library(readr)
beer <- read_csv("data/beer_reviews.csv")
names(beer)
beer1 <- beer[c(13, 1, 2, 4, 5, 6, 8, 9,
  10, 12, 17, 18, 19, 21, 22, 23, 24, 25, 26)]
names(beer1) <- c(
"id_cerveza",                          
"id_cerveceria",                       
"nombre_cerveceria",                   
"calificacion_general",                
"calificacion_aroma",                  
"calificacion_apariencia",             
"estilo_cerveza",                      
"calificacion_paladar",                
"calificacion_sabor",                  
"grado_alcohol_cerveza",            
"origen_geografico_estilo",            
"grado_alcohol_fue_imputado",       
"grado_alcohol",           
"alcohol_puro_gramos_355ml",  
"carbohidratos_gramos_355ml",
"calorías_totales_355ml",    
"nacionalidad_cervecería",    
"origen",                              
"tipo_cerveza" 
)