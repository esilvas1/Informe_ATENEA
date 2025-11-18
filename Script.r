
# Cargar librerías necesarias
library(dplyr)
library(readr)

# Definir los meses
meses <- c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")
nombres_meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                   "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Crear un dataframe vacío para almacenar los resultados
resultados_saidi <- data.frame(
  periodo = character(),
  saidi = numeric(),
  stringsAsFactors = FALSE
)

# Calcular SAIDI para cada mes
for (i in seq_along(meses)) {
  mes <- meses[i]
  nombre_mes <- nombres_meses[i]
  
  # Leer archivos CS2 y TC1 para el mes actual
  cs2_file <- paste0("DATA/CENS/2024/CS2_", mes, "_2024_OR_604.CSV")
  tc1_file <- paste0("DATA/CENS/2024/TC1_OR_604_", mes, "_2024.csv")
  
  # Verificar si los archivos existen
  if (file.exists(cs2_file) && file.exists(tc1_file)) {
    # Leer archivos suprimiendo warnings
    cs2_mes <- suppressWarnings(read_csv(cs2_file, 
                        show_col_types = FALSE,
                        locale = locale(encoding = "Latin1")))
    
    tc1_mes <- suppressWarnings(read_csv(tc1_file, 
                        show_col_types = FALSE,
                        locale = locale(encoding = "Latin1")))
    
    # Crear dataframe con NIU y DIUM
    cs2_subset <- cs2_mes %>%
      select(NIU, DIUM) %>%
      distinct()
    
    # Sumar DIUM
    total_dium <- sum(cs2_subset$DIUM, na.rm = TRUE)
    
    # Calcular SAIDI
    saidi <- total_dium / nrow(tc1_mes)
    
    # Agregar resultado al dataframe
    resultados_saidi <- rbind(resultados_saidi, 
                              data.frame(periodo = nombre_mes, 
                                       saidi = saidi))
    
    cat("Procesado:", nombre_mes, "- SAIDI:", round(saidi, 4), "\n")
  } else {
    cat("Advertencia: Archivos no encontrados para", nombre_mes, "\n")
  }
}

# Mostrar resultados finales
print(resultados_saidi)


QA_TTC1_DIVIPOLA <- read_csv(paste0("DATA/CENS/QA_TTC1_DIVIPOLA.CSV"),
                             show_col_types = FALSE,
                             locale = locale(encoding = "Latin1"))


