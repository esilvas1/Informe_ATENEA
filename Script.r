
# Cargar librerías necesarias
library(dplyr)
library(readr)

# Leer el archivos CS2 y TC1 para enero con locale para manejar encoding
cs2_enero_2024 <- read_csv("DATA/CENS/2024/CS2_ENE_2024_OR_604.CSV", 
                           show_col_types = FALSE,
                           locale = locale(encoding = "Latin1"))

tc1_enero_2024 <- read_csv("DATA/CENS/2024/TC1_OR_604_ENE_2024.csv", 
                           show_col_types = FALSE,
                           locale = locale(encoding = "Latin1"))


#crear dataframe solo con dos columnas NIU y DIUM
cs2_enero_2024_subset <- cs2_enero_2024 %>%
  select(NIU, DIUM)
#eliminar duplicados de cs2_enero_2024_subset
cs2_enero_2024_subset <- cs2_enero_2024_subset %>%
  distinct()    

#sumar la columna DIUM
total_dium <- sum(cs2_enero_2024_subset$DIUM, na.rm = TRUE) 

saidi <- total_dium/nrow(tc1_enero_2024)

saidi
