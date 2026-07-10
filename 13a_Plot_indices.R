library(data.table)
library(dplyr)
library(ggplot2)
library(readr)

oDir <- "Z:/1.Data/Results/climate/02_climate_change/hnd_indices"
plotDir <- file.path(oDir, "plots_cuenca_gcm_shading")
dir.create(plotDir, recursive = TRUE, showWarnings = FALSE)

sspList <- c("ssp245", "ssp585")
modelList <- c("ACCESS-ESM1-5", "EC-Earth3", "INM-CM5-0", "MPI-ESM1-2-HR", "MRI-ESM2-0")
region <- "Choluteca"

index_labels <- c(
  annual_prec = "Precipitación anual (mm/año)",
  annual_dry_days = "Días secos anuales (días/año)",
  annual_wet_days = "Días húmedos anuales (días/año)",
  annual_dry_spells = "Periodos secos anuales (eventos/año)",
  annual_wet_spells = "Periodos húmedos anuales (eventos/año)",
  mean_dry_duration = "Duración media de periodos secos (días)",
  mean_wet_duration = "Duración media de periodos húmedos (días)",
  mean_wet_intensity = "Intensidad media de periodos húmedos (mm/día)",
  max_dry_spell = "Máxima duración de periodo seco (días)",
  max_wet_spell = "Máxima duración de periodo húmedo (días)",
  mean_hurs = "Humedad relativa media (%)",
  mean_rsds = "Radiación solar media",
  mean_wspd = "Velocidad media del viento (m/s)",
  mean_tasmax = "Temperatura máxima media (°C)",
  mean_tasmin = "Temperatura mínima media (°C)",
  mean_tmean = "Temperatura media (°C)",
  mean_vpd = "Déficit de presión de vapor medio (kPa)",
  max_vpd = "Déficit de presión de vapor máximo (kPa)"
)

all_model_indices <- list()

for (ssp in sspList) {
  for (model in modelList) {
    
    f <- file.path(
      oDir,
      paste0("indices_", ssp, "_", model, "_2026_2080_corregimiento_ann.csv")
    )
    
    if (!file.exists(f)) next
    
    tmp <- fread(f)
    tmp[, SSP := toupper(ssp)]
    tmp[, Model := model]
    
    all_model_indices[[paste(ssp, model, sep = "_")]] <- tmp
  }
}

df <- rbindlist(all_model_indices, fill = TRUE)

for (index_var in names(index_labels)) {
  
  if (!index_var %in% names(df)) next
  
  # Promedio de toda la cuenca por GCM, SSP y año usando corregimientos
  df_model <- df[, .(
    value = mean(get(index_var), na.rm = TRUE)
  ), by = .(SSP, Model, year)]
  
  # Ensemble y rango entre GCMs
  df_plot <- df_model[, .(
    ensemble = mean(value, na.rm = TRUE),
    p10 = quantile(value, 0.10, na.rm = TRUE),
    p90 = quantile(value, 0.90, na.rm = TRUE),
    min_gcm = min(value, na.rm = TRUE),
    max_gcm = max(value, na.rm = TRUE)
  ), by = .(SSP, year)]
  
  p <- ggplot(df_plot, aes(x = year)) +
    geom_ribbon(
      aes(ymin = p10, ymax = p90, fill = SSP),
      alpha = 0.25,
      color = NA
    ) +
    geom_line(
      aes(y = ensemble, color = SSP),
      linewidth = 1.3
    ) +
    geom_smooth(
      aes(y = ensemble, color = SSP),
      method = "lm",
      se = FALSE,
      linewidth = 0.8,
      linetype = "dotted"
    ) +
    facet_wrap(~SSP, ncol = 1) +
    labs(
      x = "Año",
      y = index_labels[[index_var]],
      title = paste0("Evolución anual: ", index_labels[[index_var]], " - ", region),
      subtitle = "Línea gruesa = ensemble GCM; sombra = percentiles 10-90 entre GCMs; promedio espacial basado en corregimientos"
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )
  
  print(p)
  
  ggsave(
    filename = file.path(plotDir, paste0("timeseries_", index_var, "_cuenca_gcm_shading.png")),
    plot = p,
    width = 10,
    height = 5,
    dpi = 300
  )
}



library(data.table)
library(dplyr)
library(ggplot2)
library(readr)

oDir <- "Z:/1.Data/Results/climate/02_climate_change/dom_indices_v2"
sspList <- c("ssp245", "ssp585")
region <- "Guayubin-Mao"

# Leer archivos anuales ensemble por distrito
all_indices <- list()

for (ssp in sspList) {
  
  f <- file.path(
    oDir,
    paste0("indices_", ssp, "_ensemble_2026_2080_distrito_ann.csv")
  )
  
  tmp <- read_csv(f, show_col_types = FALSE) %>%
    mutate(
      SSP = toupper(ssp),
      Period = "2026-2080"
    )
  
  all_indices[[ssp]] <- tmp
}

df <- bind_rows(all_indices)

# Índices a graficar
index_labels <- c(
  annual_prec = "Precipitación anual (mm/año)",
  annual_dry_days = "Días secos anuales (días/año)",
  annual_wet_days = "Días húmedos anuales (días/año)",
  annual_dry_spells = "Periodos secos anuales (eventos/año)",
  annual_wet_spells = "Periodos húmedos anuales (eventos/año)",
  mean_dry_duration = "Duración media de periodos secos (días)",
  mean_wet_duration = "Duración media de periodos húmedos (días)",
  mean_wet_intensity = "Intensidad media de periodos húmedos (mm/día)",
  max_dry_spell = "Máxima duración de periodo seco (días)",
  max_wet_spell = "Máxima duración de periodo húmedo (días)",
  mean_hurs = "Humedad relativa media (%)",
  mean_rsds = "Radiación solar media",
  mean_wspd = "Velocidad media del viento (m/s)",
  mean_tasmax = "Temperatura máxima media (°C)",
  mean_tasmin = "Temperatura mínima media (°C)",
  mean_tmean = "Temperatura media (°C)",
  mean_vpd = "Déficit de presión de vapor medio (kPa)",
  max_vpd = "Déficit de presión de vapor máximo (kPa)"
)

# Crear carpeta para plots
plotDir <- file.path(oDir, "plots_distrito")
dir.create(plotDir, recursive = TRUE, showWarnings = FALSE)

for (index_var in names(index_labels)) {
  
  if (!index_var %in% names(df)) next
  
  df_plot <- df %>%
    group_by(SSP, year) %>%
    summarise(
      mean_value = mean(.data[[index_var]], na.rm = TRUE),
      p10 = quantile(.data[[index_var]], 0.10, na.rm = TRUE),
      p90 = quantile(.data[[index_var]], 0.90, na.rm = TRUE),
      .groups = "drop"
    )
  
  p <- ggplot(df_plot, aes(x = year, y = mean_value, color = SSP, fill = SSP)) +
    geom_ribbon(
      aes(ymin = p10, ymax = p90),
      alpha = 0.20,
      color = NA
    ) +
    geom_line(linewidth = 1.2) +
    geom_smooth(
      method = "lm",
      se = FALSE,
      linewidth = 0.8,
      linetype = "dotted"
    ) +
    facet_wrap(~SSP, ncol = 1) +
    labs(
      x = "Año",
      y = index_labels[[index_var]],
      title = paste0("Evolución anual: ", index_labels[[index_var]], " - ", region),
      subtitle = "Línea continua = promedio distrital; sombra = percentiles 10-90 entre distritos; línea punteada = tendencia"
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )
  
  print(p)
  
  ggsave(
    filename = file.path(plotDir, paste0("timeseries_", index_var, "_distrito.png")),
    plot = p,
    width = 8,
    height = 7,
    dpi = 300
  )
}

