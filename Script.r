# nolint start: line_length_linter, object_name_linter, trailing_whitespace_linter.

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

# Ver los nombres de las columnas de QA_TTC1_DIVIPOLA
cat("Columnas de QA_TTC1_DIVIPOLA:\n")
print(names(QA_TTC1_DIVIPOLA))

#Identificar grupos, cantidad y usuarios en los formatos TC1 de atenea

# Leer archivo TC1 de ATENEA para enero 2024
tc1_atenea_enero <- suppressWarnings(read_csv("DATA/ATENEA/2024/2024_01_AFA_FORMATO_TC1.csv",
                                              show_col_types = FALSE,
                                              locale = locale(encoding = "Latin1")))

# Crear dataframe con NIU y CODIGO_DANE_NIU
df_atenea_enero <- tc1_atenea_enero %>%
  select(NIU, CODIGO_DANE_NIU)

# Extraer los primeros 5 caracteres del CODIGO_DANE_NIU para hacer el cruce
df_atenea_enero <- df_atenea_enero %>%
  mutate(COD_MUNICIPIO = as.numeric(substr(CODIGO_DANE_NIU, 1, 5)))

df_atenea_enero <- df_atenea_enero %>%
  select(NIU, COD_MUNICIPIO)


# Realizar el cruce con QA_TTC1_DIVIPOLA para agregar el nombre del municipio
df_atenea_enero <- df_atenea_enero %>%
  left_join(QA_TTC1_DIVIPOLA %>% 
              mutate(TC1_COD_MUNICIPIO = as.numeric(TC1_COD_MUNICIPIO)) %>%
              select(TC1_COD_MUNICIPIO, TC1_MUNICIPIO) %>%
              distinct(TC1_COD_MUNICIPIO, .keep_all = TRUE),
            by = c("COD_MUNICIPIO" = "TC1_COD_MUNICIPIO"))





# Leer archivo municipios_grupos
municipios_grupos <- suppressWarnings(read_csv("DATA/ATENEA/municipios_grupos.csv",
                                               show_col_types = FALSE,
                                               locale = locale(encoding = "Latin1")))



# Realizar el cruce con municipios_grupos para agregar el grupo al municipio
df_atenea_enero <- df_atenea_enero %>%
  left_join(municipios_grupos %>%
              mutate(COD_MUNICIPIO = as.numeric(COD_MUNICIPIO)) %>%
              select(COD_MUNICIPIO, Grupo) %>%
              distinct(COD_MUNICIPIO, .keep_all = TRUE),
            by = c("COD_MUNICIPIO" = "COD_MUNICIPIO"))

df_atenea_enero  <- df_atenea_enero %>%
  filter(!is.na(Grupo))

df_atenea_enero



# nolint end

