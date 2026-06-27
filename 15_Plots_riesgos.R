# ==============================
# 1. LIBRERÍAS
# ==============================

packages <- c(
  "sf",
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "RColorBrewer",
  "patchwork",
  "scales"
)

installed <- rownames(installed.packages())

for (pkg in packages) {
  if (!(pkg %in% installed)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

invisible(lapply(packages, library, character.only = TRUE))

# ==============================
# 2. RUTAS
# ==============================

maskFile <- "Z:/1.Data/Process/Info_Inputs_SWAT/Republica_Dominicana/Division_administrativa/Guayubin_Mao_secciones.shp"

file_desab <- "Z:/1.Data/Results/climate/05_riesgos/Datos mapas de desabastecimiento.xlsx"
file_var   <- "Z:/1.Data/Results/climate/05_riesgos/Datos mapa Variabilidad_Hidrologica_Estacional.xlsx"
file_aven  <- "Z:/1.Data/Results/climate/05_riesgos/Datos mapas Avenidas_Torrenciales.xlsx"
file_amc <- "Z:/1.Data/Results/climate/05_riesgos/Datos mapa AMC_Deficit_Hidrico_Lluvia_Produccion_Agropecuaria.xlsx"
file_amc_lluvias <- "Z:/1.Data/Results/climate/05_riesgos/Datos mapas AMC_Lluvias_Extremas.xlsx"

# ==============================
# 3. LEER SHAPEFILE
# ==============================

mask <- st_read(maskFile, quiet = TRUE) %>%
  mutate(CODIGO = as.character(CODIGO))

# ==============================
# 4. FUNCIÓN PARA MAPAS
# ==============================

plot_two_maps <- function(df, col1, col2, title1, title2, main_title,
                          palette = "Reds", n_class = 9) {
  
  # Verificar que las columnas existan
  if (!(col1 %in% names(df)))
    stop("No existe la columna: ", col1)
  
  if (!(col2 %in% names(df)))
    stop("No existe la columna: ", col2)
  
  df_sel <- data.frame(
    COD_SEC = as.character(df$COD_SEC),
    v1 = as.numeric(df[[col1]]),
    v2 = as.numeric(df[[col2]])
  )
  
  dat <- mask %>%
    left_join(df_sel, by = c("CODIGO" = "COD_SEC"))
  
  max_val <- max(c(dat$v1, dat$v2), na.rm = TRUE)
  max_val <- ifelse(is.finite(max_val) & max_val > 0, max_val, 1)
  
  breaks <- pretty(c(0, max_val), n = n_class)
  breaks <- breaks[breaks >= 0 & breaks <= max_val]
  
  pal <- brewer.pal(9, palette)
  pal <- colorRampPalette(pal)(n_class)
  
  map_plot <- function(var, ttl) {
    
    ggplot(dat) +
      geom_sf(aes(fill = .data[[var]]), color = "grey40", size = 0.2) +
      scale_fill_gradientn(
        colors = pal,
        limits = c(0, max_val),
        breaks = breaks,
        na.value = "grey90",
        name = NULL
      ) +
      labs(title = ttl) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "plain", size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right"
      )
  }
  
  p1 <- map_plot("v1", title1)
  p2 <- map_plot("v2", title2)
  
  p1 + p2 +
    plot_annotation(
      title = main_title,
      theme = theme(
        plot.title = element_text(face = "plain", size = 14)
      )
    )
}

# ==============================
# 5. LEER EXCEL
# ==============================

df_desab <- read_excel(file_desab)
df_var   <- read_excel(file_var)
df_aven  <- read_excel(file_aven)
df_amc <- read_excel(file_amc)
names(df_amc) <- trimws(names(df_amc))
df_amc_lluvias <- read_excel(file_amc_lluvias)
names(df_amc_lluvias) <- trimws(names(df_amc_lluvias))

# ==============================
# 6. DESABASTECIMIENTO
# MAR y JUL-AGO
# ==============================

p_desab <- plot_two_maps(
  df = df_desab,
  col1 = "MAR",
  col2 = "JUL-AGO",
  title1 = "MAR",
  title2 = "JUL-AGO",
  main_title = "Riesgo de desabastecimiento",
  palette = "Reds",
  n_class = 9
)

print(p_desab)

# ==============================
# 7. VARIABILIDAD HIDROLÓGICA
# IAE_raw
# ==============================

p_var <- plot_two_maps(
  df = df_var,
  col1 = "IAE_raw",
  col2 = "IAE_raw",
  title1 = "IAE_raw",
  title2 = "IAE_raw",
  main_title = "Variabilidad hidrológica estacional",
  palette = "Reds",
  n_class = 9
)

print(p_var)

# ==============================
# 8. AVENIDAS TORRENCIALES
# MAY-JUN y OCT-NOV
# ==============================

p_aven <- plot_two_maps(
  df = df_aven,
  col1 = "MAY-JUN",
  col2 = "OCT-NOV",
  title1 = "MAY-JUN",
  title2 = "OCT-NOV",
  main_title = "Riesgo de avenidas torrenciales",
  palette = "Reds",
  n_class = 9
)

print(p_aven)


# ==============================
# 9. AMC - Déficit hídrico
# FEB-MAR (Cultivos permanentes)
# JUL-AGO (Cultivos transitorios)
# ==============================

p_amc <- plot_two_maps(
  df = df_amc,
  col1 = "FEB-MAR (Cultivos permanentes)",
  col2 = "JUL-AGO (cultivos transitorios)",
  title1 = "FEB-MAR\nCultivos permanentes",
  title2 = "JUL-AGO\nCultivos transitorios",
  main_title = "AMC - Déficit hídrico por lluvia para la producción agropecuaria",
  palette = "Reds",
  n_class = 9
)

print(p_amc)

# ==============================
# AMC - Lluvias extremas
# MAY y OCT
# ==============================

p_amc_lluvias <- plot_two_maps(
  df = df_amc_lluvias,
  col1 = "MAY",
  col2 = "OCT",
  title1 = "MAY",
  title2 = "OCT",
  main_title = "AMC - Lluvias extremas",
  palette = "Reds",
  n_class = 9
)

print(p_amc_lluvias)

# ==============================
# 9. GUARDAR PLOTS EN PNG
# ==============================

outDir <- "Z:/1.Data/Results/climate/05_riesgos"

dir.create(outDir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = file.path(outDir, "Plot_Riesgo_Desabastecimiento.png"),
  plot = p_desab,
  width = 14,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(outDir, "Plot_Variabilidad_Hidrologica_Estacional.png"),
  plot = p_var,
  width = 14,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(outDir, "Plot_Avenidas_Torrenciales.png"),
  plot = p_aven,
  width = 14,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(outDir, "Plot_AMC_Deficit_Hidrico_Lluvia_Produccion_Agropecuaria.png"),
  plot = p_amc,
  width = 14,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(outDir, "Plot_AMC_Lluvias_Extremas.png"),
  plot = p_amc_lluvias,
  width = 14,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)