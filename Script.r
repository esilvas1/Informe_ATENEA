
# Cargar librerías necesarias
library(dplyr)
library(readr)

#calcular indicador SAIDI enero




# Definir la ruta de los archivos
ruta_cs2 <- "\\\\CENS-AD02.cens.corp.epm.com.co\\Administrativa\\UO01\\CISO01\\CISO01\\atenea\\nueva descarga 18-11-2025\\CS2\\2025"

# Listar todos los archivos CSV en la carpeta
archivos_csv <- list.files(path = ruta_cs2, 
                           pattern = "\\.csv$", 
                           full.names = TRUE)


# Leer y consolidar todos los archivos CSV en uno solo
datos_cs2 <- archivos_csv %>%
  lapply(read_csv, show_col_types = FALSE) %>%
  bind_rows()

# Mostrar información del dataset consolidado
cat("Total de archivos leídos:", length(archivos_csv), "\n")
cat("Total de registros consolidados:", nrow(datos_cs2), "\n")
cat("Total de columnas:", ncol(datos_cs2), "\n")

# Mostrar primeras filas
head(datos_cs2)




#rmarkdown::render("Informe_ATENEA.Rmd")
