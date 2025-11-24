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

# === ANÁLISIS DE ESCENARIOS POR GRUPOS ===

# Función para calcular SAIDI de un mes específico
calcular_saidi_mes <- function(mes, incluir_grupos = NULL) {
  
  mes_abrev <- meses[mes]
  nombre_mes <- nombres_meses[mes]
  
  # Archivos CENS
  cs2_cens_file <- paste0("DATA/CENS/2024/CS2_", mes_abrev, "_2024_OR_604.CSV")
  tc1_cens_file <- paste0("DATA/CENS/2024/TC1_OR_604_", mes_abrev, "_2024.csv")
  
  # Archivos ATENEA
  mes_num <- sprintf("%02d", mes)
  cs2_atenea_file <- paste0("DATA/ATENEA/2024/2024_", mes_num, "_AFA_FORMATO_CS2.csv")
  tc1_atenea_file <- paste0("DATA/ATENEA/2024/2024_", mes_num, "_AFA_FORMATO_TC1.csv")
  
  # Leer archivos CENS
  cs2_cens <- suppressWarnings(read_csv(cs2_cens_file,
                                        show_col_types = FALSE,
                                        locale = locale(encoding = "Latin1")))
  
  tc1_cens <- suppressWarnings(read_csv(tc1_cens_file,
                                        show_col_types = FALSE,
                                        locale = locale(encoding = "Latin1")))
  
  # Calcular SAIDI base (sin ATENEA)
  cs2_cens_subset <- cs2_cens %>%
    select(NIU, DIUM) %>%
    distinct()
  
  total_dium_base <- sum(cs2_cens_subset$DIUM, na.rm = TRUE)
  saidi_base <- total_dium_base / nrow(tc1_cens)
  
  # Si no hay grupos a incluir, retornar solo el SAIDI base
  if (is.null(incluir_grupos)) {
    return(list(
      mes = nombre_mes,
      saidi_base = saidi_base,
      usuarios_base = nrow(tc1_cens),
      dium_base = total_dium_base
    ))
  }
  
  # Leer archivos ATENEA
  if (!file.exists(cs2_atenea_file) || !file.exists(tc1_atenea_file)) {
    cat("Advertencia: Archivos ATENEA no encontrados para", nombre_mes, "\n")
    return(NULL)
  }
  
  cs2_atenea <- suppressWarnings(read_csv(cs2_atenea_file,
                                          show_col_types = FALSE,
                                          locale = locale(encoding = "Latin1")))
  
  tc1_atenea <- suppressWarnings(read_csv(tc1_atenea_file,
                                          show_col_types = FALSE,
                                          locale = locale(encoding = "Latin1")))
  
  # Identificar NIUs de los grupos seleccionados
  # Primero ver cuántos registros hay en TC1 ATENEA
  cat("  - Total NIUs en TC1 ATENEA:", nrow(tc1_atenea), "\n")
  
  # Preparar datos con municipio
  tc1_con_muni <- tc1_atenea %>%
    select(NIU, CODIGO_DANE_NIU) %>%
    mutate(COD_MUNICIPIO = as.numeric(substr(CODIGO_DANE_NIU, 1, 5)))
  
  cat("  - NIUs con COD_MUNICIPIO:", nrow(tc1_con_muni), "\n")
  
  # Agregar grupo
  tc1_atenea_con_grupo <- tc1_con_muni %>%
    left_join(municipios_grupos %>%
                mutate(COD_MUNICIPIO = as.numeric(COD_MUNICIPIO)) %>%
                select(COD_MUNICIPIO, Grupo) %>%
                distinct(COD_MUNICIPIO, .keep_all = TRUE),
              by = "COD_MUNICIPIO")
  
  cat("  - NIUs con Grupo asignado:", sum(!is.na(tc1_atenea_con_grupo$Grupo)), "\n")
  cat("  - Grupos encontrados:", paste(unique(tc1_atenea_con_grupo$Grupo[!is.na(tc1_atenea_con_grupo$Grupo)]), collapse = ", "), "\n")
  cat("  - Buscando grupos:", paste(incluir_grupos, collapse = ", "), "\n")
  
  # Filtrar por grupos seleccionados
  tc1_atenea_con_grupo <- tc1_atenea_con_grupo %>%
    filter(!is.na(Grupo) & Grupo %in% incluir_grupos)
  
  cat("  - NIUs encontrados en grupos seleccionados:", nrow(tc1_atenea_con_grupo), "\n")
  
  nius_grupos <- tc1_atenea_con_grupo$NIU
  
  # Filtrar CS2 de ATENEA con los NIUs de los grupos seleccionados
  cs2_atenea_grupos <- cs2_atenea %>%
    filter(NIU %in% nius_grupos) %>%
    select(NIU, DIUM)
  
  cat("  - Registros CS2 ATENEA encontrados:", nrow(cs2_atenea_grupos), "\n")
  
  # Combinar CS2: primero agrupar por NIU para evitar duplicados
  cs2_cens_agrupado <- cs2_cens_subset %>%
    group_by(NIU) %>%
    summarise(DIUM = sum(DIUM, na.rm = TRUE), .groups = "drop")
  
  cs2_atenea_agrupado <- cs2_atenea_grupos %>%
    group_by(NIU) %>%
    summarise(DIUM = sum(DIUM, na.rm = TRUE), .groups = "drop")
  
  # Combinar y sumar DIUM total
  cs2_combinado <- bind_rows(cs2_cens_agrupado, cs2_atenea_agrupado)
  
  # Contar usuarios base CENS
  usuarios_base_cens <- nrow(tc1_cens)
  
  # Contar usuarios ATENEA de los grupos seleccionados
  usuarios_atenea_grupos <- nrow(tc1_atenea_con_grupo)
  
  # Total de usuarios con grupos
  usuarios_total <- usuarios_base_cens + usuarios_atenea_grupos
  
  # Calcular SAIDI con grupos incluidos
  total_dium_con_grupos <- sum(cs2_combinado$DIUM, na.rm = TRUE)
  saidi_con_grupos <- total_dium_con_grupos / usuarios_total
  
  return(list(
    mes = nombre_mes,
    grupos_incluidos = paste(incluir_grupos, collapse = ", "),
    saidi_base = saidi_base,
    saidi_con_grupos = saidi_con_grupos,
    diferencia = saidi_con_grupos - saidi_base,
    diferencia_porcentual = ((saidi_con_grupos - saidi_base) / saidi_base) * 100,
    usuarios_base = usuarios_base_cens,
    usuarios_con_grupos = usuarios_total,
    usuarios_agregados = usuarios_atenea_grupos,
    dium_base = total_dium_base,
    dium_con_grupos = total_dium_con_grupos,
    dium_agregado = total_dium_con_grupos - total_dium_base
  ))
}

# Función para analizar escenarios múltiples
analizar_escenarios <- function(grupos_seleccionados) {
  
  cat("\n=== ANÁLISIS DE ESCENARIO ===\n")
  cat("Grupos incluidos:", paste(grupos_seleccionados, collapse = ", "), "\n\n")
  
  resultados <- data.frame()
  
  for (mes in 1:12) {
    resultado <- calcular_saidi_mes(mes, grupos_seleccionados)
    
    if (!is.null(resultado)) {
      resultados <- bind_rows(resultados, as.data.frame(resultado))
      cat("Procesado:", resultado$mes, "\n")
    }
  }
  
  return(resultados)
}

# Ver grupos disponibles
cat("\n=== GRUPOS DISPONIBLES ===\n")
grupos_disponibles <- unique(municipios_grupos$Grupo)
print(grupos_disponibles)

# === EJEMPLOS DE USO ===

# IMPORTANTE: Los grupos son NÚMEROS, no texto
# Grupos disponibles: 1, 2, 3, 4, 5, 6, 7, 8, 9

# Ejemplo 1: Analizar con el grupo 1 (VALLEDUPAR y PUEBLO BELLO)
escenario_1 <- analizar_escenarios(c(1))

# Ejemplo 2: Analizar con múltiples grupos
# escenario_2 <- analizar_escenarios(c(1, 2))

# Ejemplo 3: Analizar con todos los grupos
# escenario_completo <- analizar_escenarios(grupos_disponibles)

# Para ejecutar un escenario específico:
# escenario_ejemplo <- analizar_escenarios(c(1, 2, 3))

print(escenario_1)

rmarkdown::render("Informe_ATENEA.Rmd", params = list(grupos_seleccionados = c(6)))

########################## Writing for amount of users for each format ################################

# Cargar librerías necesarias
library(dplyr)
library(readr)
library(knitr)

# Definir meses
meses_abrev <- c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")
meses_num <- sprintf("%02d", 1:12)
nombres_meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                   "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# === PROCESAR ATENEA ===
cat("\n=== PROCESANDO ATENEA ===\n\n")
tabla_conteo_atenea <- data.frame()

for (i in 1:12) {
  mes_num <- meses_num[i]
  nombre_mes <- nombres_meses[i]
  
  # Archivos ATENEA
  tc1_file <- paste0("DATA/ATENEA/2024/2024_", mes_num, "_AFA_FORMATO_TC1.csv")
  cs2_file <- paste0("DATA/ATENEA/2024/2024_", mes_num, "_AFA_FORMATO_CS2.csv")
  
  if (file.exists(tc1_file) && file.exists(cs2_file)) {
    
    # Procesar TC1 ATENEA
    tc1_mes <- suppressWarnings(read_csv(tc1_file,
                                         show_col_types = FALSE,
                                         locale = locale(encoding = "Latin1")))
    
    tc1_filtrado <- tc1_mes %>%
      filter(TIPO_CONEXION == "2") %>%
      filter(!grepl("^ALPM", CODIGO_CONEXION)) %>%
      distinct(NIU, .keep_all = TRUE)
    
    usuarios_tc1 <- nrow(tc1_filtrado)
    
    # Procesar CS2 ATENEA
    cs2_mes <- suppressWarnings(read_csv(cs2_file,
                                         show_col_types = FALSE,
                                         locale = locale(encoding = "Latin1")))
    
    cs2_filtrado <- cs2_mes %>%
      distinct(NIU)
    
    usuarios_cs2 <- nrow(cs2_filtrado)
    
    # Agregar a tabla ATENEA
    tabla_conteo_atenea <- bind_rows(tabla_conteo_atenea, data.frame(
      EMPRESA = "ATENEA",
      AÑO = 2024,
      MES = nombre_mes,
      USUARIOS_TC1 = usuarios_tc1,
      USUARIOS_CS2 = usuarios_cs2
    ))
    
    cat("ATENEA -", nombre_mes, ": TC1 =", usuarios_tc1, "| CS2 =", usuarios_cs2, "\n")
    
  } else {
    cat("ATENEA - Advertencia: Archivos no encontrados para", nombre_mes, "\n")
  }
}

# === PROCESAR CENS ===
cat("\n=== PROCESANDO CENS ===\n\n")
tabla_conteo_cens <- data.frame()

for (i in 1:12) {
  mes_abrev <- meses_abrev[i]
  nombre_mes <- nombres_meses[i]
  
  # Archivos CENS
  tc1_file <- paste0("DATA/CENS/2024/TC1_OR_604_", mes_abrev, "_2024.csv")
  cs2_file <- paste0("DATA/CENS/2024/CS2_", mes_abrev, "_2024_OR_604.CSV")
  
  if (file.exists(tc1_file) && file.exists(cs2_file)) {
    
    # Procesar TC1 CENS
    tc1_mes <- suppressWarnings(read_csv(tc1_file,
                                         show_col_types = FALSE,
                                         locale = locale(encoding = "Latin1")))
    
    # Para CENS no aplicamos filtros adicionales, solo distinct
    tc1_filtrado <- tc1_mes %>%
      distinct(NIU, .keep_all = TRUE)
    
    usuarios_tc1 <- nrow(tc1_filtrado)
    
    # Procesar CS2 CENS
    cs2_mes <- suppressWarnings(read_csv(cs2_file,
                                         show_col_types = FALSE,
                                         locale = locale(encoding = "Latin1")))
    
    cs2_filtrado <- cs2_mes %>%
      select(NIU, DIUM) %>%
      distinct()
    
    usuarios_cs2 <- nrow(cs2_filtrado)
    
    # Agregar a tabla CENS
    tabla_conteo_cens <- bind_rows(tabla_conteo_cens, data.frame(
      EMPRESA = "CENS",
      AÑO = 2024,
      MES = nombre_mes,
      USUARIOS_TC1 = usuarios_tc1,
      USUARIOS_CS2 = usuarios_cs2
    ))
    
    cat("CENS -", nombre_mes, ": TC1 =", usuarios_tc1, "| CS2 =", usuarios_cs2, "\n")
    
  } else {
    cat("CENS - Advertencia: Archivos no encontrados para", nombre_mes, "\n")
  }
}

# === CONSOLIDAR TABLAS ===
tabla_conteo_usuarios <- bind_rows(tabla_conteo_atenea, tabla_conteo_cens)

# Mostrar tablas
cat("\n=== TABLA CONSOLIDADA ===\n")
print(tabla_conteo_usuarios)

# Guardar en archivo CSV
write_csv(tabla_conteo_usuarios, "DATA/tabla_conteo_usuarios_ATENEA_CENS.csv")
cat("\n✓ Tabla guardada en: DATA/tabla_conteo_usuarios_ATENEA_CENS.csv\n")

# Mostrar resumen por empresa
cat("\n=== RESUMEN POR EMPRESA ===\n")
resumen_empresas <- tabla_conteo_usuarios %>%
  group_by(EMPRESA) %>%
  summarise(
    TC1_Promedio = round(mean(USUARIOS_TC1), 0),
    CS2_Promedio = round(mean(USUARIOS_CS2), 0),
    TC1_Total = sum(USUARIOS_TC1),
    CS2_Total = sum(USUARIOS_CS2)
  )
print(resumen_empresas)

# Vista previa
View(head(tabla_conteo_usuarios, n = 100))



# nolint end