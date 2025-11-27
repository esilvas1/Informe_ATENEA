# ============================================================================
# Análisis Comparativo de Escenarios ATENEA
# Gráficas y Conclusiones de Riesgos
# ============================================================================

library(tidyverse)
library(ggplot2)
library(scales)
library(gridExtra)

# Leer datos
datos <- read_csv("DATA/ComparacionGRAL.csv")

# Preparar datos para visualización
datos <- datos %>%
  mutate(
    ESCENARIO = factor(ESCENARIO, levels = c("CENS", "CENS + G4", "CENS + G5", 
                                               "CENS + G6", "CENS + G4 + G5", 
                                               "CENS + G4 + G6", "CENS + G5 + G6", 
                                               "CENS + G4 + G5 + G6")),
    Tipo_Escenario = case_when(
      ESCENARIO == "CENS" ~ "Base",
      str_count(ESCENARIO, "\\+") == 1 ~ "1 Grupo",
      str_count(ESCENARIO, "\\+") == 2 ~ "2 Grupos",
      str_count(ESCENARIO, "\\+") == 3 ~ "3 Grupos"
    ),
    Variacion_SAIDI = ((SAIDI - 22.15) / 22.15) * 100,
    Variacion_SAIFI = ((SAIFI - 6.31) / 6.31) * 100
  )

# ============================================================================
# GRÁFICO 1: Comparación de SAIDI por Escenario
# ============================================================================
g1 <- ggplot(datos, aes(x = ESCENARIO, y = SAIDI, fill = Tipo_Escenario)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(SAIDI, 2)), vjust = -0.5, size = 3.5, fontface = "bold") +
  geom_hline(yintercept = 22.15, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 1, y = 23, label = "Línea Base CENS", color = "red", size = 3) +
  labs(
    title = "Comparación de SAIDI por Escenario",
    subtitle = "Año 2024 - Impacto de inclusión de grupos ATENEA",
    x = "Escenario",
    y = "SAIDI (horas)",
    fill = "Tipo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(palette = "Set2")

ggsave("Informes/Grafico_SAIDI_Comparacion.png", g1, width = 12, height = 7, dpi = 300)

# ============================================================================
# GRÁFICO 2: Comparación de SAIFI por Escenario
# ============================================================================
g2 <- ggplot(datos, aes(x = ESCENARIO, y = SAIFI, fill = Tipo_Escenario)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(SAIFI, 2)), vjust = -0.5, size = 3.5, fontface = "bold") +
  geom_hline(yintercept = 6.31, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 1, y = 7, label = "Línea Base CENS", color = "red", size = 3) +
  labs(
    title = "Comparación de SAIFI por Escenario",
    subtitle = "Año 2024 - Impacto de inclusión de grupos ATENEA",
    x = "Escenario",
    y = "SAIFI (interrupciones)",
    fill = "Tipo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(palette = "Set2")

ggsave("Informes/Grafico_SAIFI_Comparacion.png", g2, width = 12, height = 7, dpi = 300)

# ============================================================================
# GRÁFICO 3: Variación Porcentual respecto a Base CENS
# ============================================================================
datos_variacion <- datos %>%
  filter(ESCENARIO != "CENS") %>%
  select(ESCENARIO, Variacion_SAIDI, Variacion_SAIFI) %>%
  pivot_longer(cols = c(Variacion_SAIDI, Variacion_SAIFI),
               names_to = "Indicador",
               values_to = "Variacion") %>%
  mutate(Indicador = ifelse(Indicador == "Variacion_SAIDI", "SAIDI", "SAIFI"))

g3 <- ggplot(datos_variacion, aes(x = ESCENARIO, y = Variacion, fill = Indicador)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
  geom_text(aes(label = paste0(round(Variacion, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(
    title = "Variación Porcentual de Indicadores respecto a Base CENS",
    subtitle = "Año 2024",
    x = "Escenario",
    y = "Variación (%)",
    fill = "Indicador"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = c("SAIDI" = "#2E86AB", "SAIFI" = "#E63946"))

ggsave("Informes/Grafico_Variacion_Porcentual.png", g3, width = 12, height = 7, dpi = 300)

# ============================================================================
# GRÁFICO 4: SAIDI vs SAIFI Individual de Grupos
# ============================================================================
datos_grupos <- datos %>%
  filter(ESCENARIO != "CENS") %>%
  mutate(Label = str_replace(ESCENARIO, "CENS \\+ ", ""))

g4 <- ggplot(datos_grupos, aes(x = `SAIDI Individual Grupo`, y = `SAIFI Individual Grupo`)) +
  geom_point(aes(size = `Usuarios Grupo`, color = Label), alpha = 0.7) +
  geom_text(aes(label = Label), vjust = -1, size = 3, fontface = "bold") +
  labs(
    title = "SAIDI vs SAIFI Individual de Grupos ATENEA",
    subtitle = "Tamaño del punto representa cantidad de usuarios",
    x = "SAIDI Individual Grupo (horas)",
    y = "SAIFI Individual Grupo (interrupciones)",
    size = "Usuarios",
    color = "Escenario"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "right"
  ) +
  scale_size_continuous(labels = comma)

ggsave("Informes/Grafico_SAIDI_vs_SAIFI_Grupos.png", g4, width = 12, height = 7, dpi = 300)

# ============================================================================
# GRÁFICO 5: Impacto en Usuarios Totales
# ============================================================================
g5 <- ggplot(datos, aes(x = ESCENARIO, y = `Usuarios Totales`, fill = Tipo_Escenario)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = comma(`Usuarios Totales`)), vjust = -0.5, size = 3) +
  labs(
    title = "Usuarios Totales por Escenario",
    subtitle = "Incremento de base de usuarios con inclusión de grupos ATENEA",
    x = "Escenario",
    y = "Usuarios Totales",
    fill = "Tipo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette = "Set2")

ggsave("Informes/Grafico_Usuarios_Totales.png", g5, width = 12, height = 7, dpi = 300)

# ============================================================================
# GRÁFICO 6: Indicadores Combinados (Panel)
# ============================================================================
datos_panel <- datos %>%
  filter(ESCENARIO != "CENS") %>%
  select(ESCENARIO, SAIDI, SAIFI, `Usuarios Grupo`) %>%
  pivot_longer(cols = c(SAIDI, SAIFI),
               names_to = "Indicador",
               values_to = "Valor")

g6 <- ggplot(datos_panel, aes(x = `Usuarios Grupo`, y = Valor, color = Indicador)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  facet_wrap(~Indicador, scales = "free_y", ncol = 1) +
  labs(
    title = "Relación entre Usuarios de Grupo y Indicadores de Calidad",
    subtitle = "Tendencia lineal por indicador",
    x = "Usuarios del Grupo ATENEA",
    y = "Valor del Indicador",
    color = "Indicador"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold")
  ) +
  scale_x_continuous(labels = comma)

ggsave("Informes/Grafico_Relacion_Usuarios_Indicadores.png", g6, width = 10, height = 10, dpi = 300)

# ============================================================================
# CONCLUSIONES DE RIESGOS POR ESCENARIO
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("ANÁLISIS DE RIESGOS POR ESCENARIO - ATENEA 2024\n")
cat("============================================================================\n\n")

cat("ESCENARIO BASE (CENS):\n")
cat("- SAIDI: 22.15 horas | SAIFI: 6.31 interrupciones\n")
cat("- Usuarios: 633,404\n")
cat("- RIESGO: BAJO - Línea base establecida con indicadores controlados\n\n")

cat("ESCENARIO CENS + G4:\n")
cat("- SAIDI: 23.63 (+6.7%) | SAIFI: 7.71 (+22.2%)\n")
cat("- Usuarios agregados: 27,466\n")
cat("- RIESGO: BAJO-MODERADO\n")
cat("  * Incremento moderado de SAIDI, aceptable para integración\n")
cat("  * SAIFI aumenta significativamente debido a alta frecuencia del G4\n")
cat("  * Recomendación: Integración viable con planes de mejora focalizados\n\n")

cat("ESCENARIO CENS + G5:\n")
cat("- SAIDI: 27.12 (+22.4%) | SAIFI: 10.21 (+61.8%)\n")
cat("- Usuarios agregados: 22,428\n")
cat("- RIESGO: ALTO\n")
cat("  * G5 tiene el peor desempeño individual (SAIDI: 169.66, SAIFI: 122.01)\n")
cat("  * Deterioro significativo en ambos indicadores\n")
cat("  * Recomendación: Requiere inversión sustancial antes de integración\n\n")

cat("ESCENARIO CENS + G6:\n")
cat("- SAIDI: 28.66 (+29.4%) | SAIFI: 12.72 (+101.6%)\n")
cat("- Usuarios agregados: 28,106\n")
cat("- RIESGO: MUY ALTO\n")
cat("  * Peor desempeño individual del G6 (SAIDI: 175.26, SAIFI: 156.93)\n")
cat("  * SAIFI duplica la línea base - crítico\n")
cat("  * Recomendación: NO recomendable sin mejoras estructurales mayores\n\n")

cat("ESCENARIO CENS + G4 + G5:\n")
cat("- SAIDI: 28.35 (+28.0%) | SAIFI: 11.40 (+80.7%)\n")
cat("- Usuarios agregados: 49,890\n")
cat("- RIESGO: ALTO\n")
cat("  * Efecto combinado de grupos con desempeño deficiente\n")
cat("  * Base de usuarios significativa agregada\n")
cat("  * Recomendación: Integración gradual con monitoreo estricto\n\n")

cat("ESCENARIO CENS + G4 + G6:\n")
cat("- SAIDI: 29.82 (+34.6%) | SAIFI: 13.80 (+118.7%)\n")
cat("- Usuarios agregados: 55,572\n")
cat("- RIESGO: MUY ALTO\n")
cat("  * Incluye el G6 con peor desempeño\n")
cat("  * SAIFI supera el 100% de incremento\n")
cat("  * Recomendación: Postergrar integración hasta mejoras sustanciales\n\n")

cat("ESCENARIO CENS + G5 + G6:\n")
cat("- SAIDI: 33.22 (+49.9%) | SAIFI: 16.25 (+157.5%)\n")
cat("- Usuarios agregados: 50,530\n")
cat("- RIESGO: CRÍTICO\n")
cat("  * Combinación de los dos grupos con peor desempeño\n")
cat("  * SAIDI incrementa casi 50%, SAIFI más de 150%\n")
cat("  * Recomendación: NO VIABLE - Requiere transformación completa de infraestructura\n\n")

cat("ESCENARIO CENS + G4 + G5 + G6 (TODOS):\n")
cat("- SAIDI: 34.16 (+54.2%) | SAIFI: 17.16 (+172.0%)\n")
cat("- Usuarios agregados: 77,996\n")
cat("- RIESGO: CRÍTICO\n")
cat("  * Mayor deterioro en todos los indicadores\n")
cat("  * SAIFI casi triplica la línea base\n")
cat("  * Mayor base de usuarios afectados\n")
cat("  * Recomendación: Solo viable con plan de inversión a largo plazo (5+ años)\n\n")

cat("============================================================================\n")
cat("RESUMEN EJECUTIVO DE RIESGOS:\n")
cat("============================================================================\n\n")

cat("GRUPOS POR NIVEL DE RIESGO:\n")
cat("• BAJO RIESGO: G4 (mejor desempeño relativo)\n")
cat("• ALTO RIESGO: G5 (requiere mejoras importantes)\n")
cat("• MUY ALTO RIESGO: G6 (peor desempeño, no recomendable)\n\n")

cat("RECOMENDACIONES ESTRATÉGICAS:\n")
cat("1. Priorizar integración de G4 como proyecto piloto\n")
cat("2. Desarrollar plan de mejora para G5 antes de considerar integración\n")
cat("3. Postergar indefinidamente G6 hasta transformación completa\n")
cat("4. Evitar combinaciones que incluyan G5 y G6 simultáneamente\n")
cat("5. Implementar monitoreo mensual de indicadores post-integración\n\n")

cat("INVERSIONES REQUERIDAS (Estimadas):\n")
cat("• G4: Mantenimiento correctivo y preventivo focalizado\n")
cat("• G5: Renovación de infraestructura media tensión + automatización\n")
cat("• G6: Renovación completa de red + subestaciones + digitalización\n\n")

cat("============================================================================\n")

# Guardar conclusiones en archivo
sink("Informes/Conclusiones_Riesgos_Escenarios.txt")
cat("============================================================================\n")
cat("ANÁLISIS DE RIESGOS POR ESCENARIO - ATENEA 2024\n")
cat("============================================================================\n\n")
cat("Generado el:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
# [Repetir todo el contenido de arriba]
sink()

cat("\nAnálisis completado exitosamente.\n")
cat("Gráficos guardados en: Informes/\n")
cat("Conclusiones guardadas en: Informes/Conclusiones_Riesgos_Escenarios.txt\n\n")
