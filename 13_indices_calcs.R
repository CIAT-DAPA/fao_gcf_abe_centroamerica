library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringi)

bslDir  <- "Y:/1.Data/RAW/Clima/Escenarios CC_IDEAM/Informacion_Departamental/HUILA/Series de Datos/Precipitacion"
oDir <- "Y:/1.Data/Results/Cambio_Climatico/indices" 
sspList <- c("SSP126", "SSP245", "SSP585")
perList <- c("2021-2040", "2041-2060")
region <- c("HUILA")

if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}

for (ssp in sspList){
  
  for (per in perList) {
  
    # Leer archivo
    f <- file.path(
      bslDir,
      paste0(ssp, "/DatosDiarios_Precipitacion_", ssp, "_", per, "_", region, ".txt")
    )
    
    df <- fread(f,encoding = "Latin-1")
    
    # df$Municipios <- iconv(
    #   df$Municipios,
    #   from = "latin1",
    #   to = "UTF-8"
    # )
    # 
    # df$Municipios <- stri_trans_general(
    #   df$Municipios,
    #   "Latin-ASCII"
    # )
    # 
    # df$Municipios <- toupper(trimws(df$Municipios))
    
    # Identificar columnas de fechas
    date_cols <- grep("^\\d{4}-\\d{2}-\\d{2}$", names(df), value = TRUE)
    # Pasar de wide a long
    df_long <- df %>%
      pivot_longer(
        cols = all_of(date_cols),
        names_to = "date",
        values_to = "prec"
      ) %>%
      mutate(
        date = as.Date(date),
        year = year(date),
        month = month(date),
        prec = as.numeric(prec),
        dry = prec <= 1,
        wet = prec > 1
      )
    
    # Función para rachas
    spell_stats <- function(x, min_len = 5) {
      r <- rle(x)
      spells <- r$lengths[r$values == TRUE]
      spells <- spells[spells >= min_len]
      c(
        n_spells = length(spells),
        mean_duration = ifelse(length(spells) > 0, mean(spells), 0),
        max_duration = ifelse(length(spells) > 0, max(spells), 0)
      )
    }
    
    # Calcular índices mensuales
    indices_monthly <- df_long %>%
      group_by(ID, Longitud, Latitud, Departamentos, Municipios,
               `Areas Hidrograficas`, `Zonas Hidrograficas`,
               `SubZonas Hidrograficas`, year, month) %>%
      summarise(
        total_prec = sum(prec, na.rm = TRUE),
        total_dry_days = sum(dry, na.rm = TRUE),
        total_wet_days = sum(wet, na.rm = TRUE),
        dry_n_spells = spell_stats(dry)["n_spells"],
        dry_mean_duration = spell_stats(dry)["mean_duration"],
        dry_max_duration = spell_stats(dry)["max_duration"],
        wet_n_spells = spell_stats(wet)["n_spells"],
        wet_mean_duration = spell_stats(wet)["mean_duration"],
        wet_max_duration = spell_stats(wet)["max_duration"],
        wet_spell_intensity = ifelse(
          sum(wet, na.rm = TRUE) > 0,
          mean(prec[wet], na.rm = TRUE),
          0
        ),
        .groups = "drop"
      )
    # Guardar resultados
    write.csv(
      indices_monthly,
      paste0(oDir, "/indices_", tolower(ssp), "_", per, "_", tolower(region), "_mon.csv"),
      row.names = FALSE
    )
    
  
  
  # Calcular índices anuales
  indices_annual <- indices_monthly %>%
    group_by(
      ID,
      Longitud,
      Latitud,
      Departamentos,
      Municipios,
      year
    ) %>%
    summarise(
      # acumulativos
      annual_prec = sum(total_prec, na.rm = TRUE),
      annual_dry_days = sum(total_dry_days, na.rm = TRUE),
      annual_wet_days = sum(total_wet_days, na.rm = TRUE),
      annual_dry_spells = sum(dry_n_spells, na.rm = TRUE),
      annual_wet_spells = sum(wet_n_spells, na.rm = TRUE),
      # promedios
      mean_dry_duration = mean(dry_mean_duration, na.rm = TRUE),
      mean_wet_duration = mean(wet_mean_duration, na.rm = TRUE),
      mean_wet_intensity = mean(wet_spell_intensity, na.rm = TRUE),
      # máximos
      max_dry_spell = max(dry_max_duration, na.rm = TRUE),
      max_wet_spell = max(wet_max_duration, na.rm = TRUE),
      .groups = "drop"
    )
  
  write.csv(
    indices_annual,
    paste0(oDir, "/indices_", tolower(ssp), "_", per, "_", tolower(region), "_ann.csv"),
    row.names = FALSE
    
  )
  
  # Calcular índices municipales
  indices_municipal_annual <- indices_annual %>%
    group_by(Departamentos, Municipios, year) %>%
    summarise(
      annual_prec = mean(annual_prec, na.rm = TRUE),
      annual_dry_days = mean(annual_dry_days, na.rm = TRUE),
      annual_wet_days = mean(annual_wet_days, na.rm = TRUE),
      annual_dry_spells = mean(annual_dry_spells, na.rm = TRUE),
      annual_wet_spells = mean(annual_wet_spells, na.rm = TRUE),
      mean_dry_duration = mean(mean_dry_duration, na.rm = TRUE),
      mean_wet_duration = mean(mean_wet_duration, na.rm = TRUE),
      mean_wet_intensity = mean(mean_wet_intensity, na.rm = TRUE),
      max_dry_spell = max(max_dry_spell, na.rm = TRUE),
      max_wet_spell = max(max_wet_spell, na.rm = TRUE),
      n_points = n(),
      .groups = "drop"
    )
    
  indices_municipal_period <- indices_municipal_annual %>%
    group_by(Departamentos, Municipios) %>%
    summarise(
      annual_prec = mean(annual_prec, na.rm = TRUE),
      annual_dry_days = mean(annual_dry_days, na.rm = TRUE),
      annual_wet_days = mean(annual_wet_days, na.rm = TRUE),
      annual_dry_spells = mean(annual_dry_spells, na.rm = TRUE),
      annual_wet_spells = mean(annual_wet_spells, na.rm = TRUE),
      mean_dry_duration = mean(mean_dry_duration, na.rm = TRUE),
      mean_wet_duration = mean(mean_wet_duration, na.rm = TRUE),
      mean_wet_intensity = mean(mean_wet_intensity, na.rm = TRUE),
      max_dry_spell = max(max_dry_spell, na.rm = TRUE),
      max_wet_spell = max(max_wet_spell, na.rm = TRUE),
      n_years = n_distinct(year),
      n_points_mean = mean(n_points, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_mun <- indices_municipal_period %>%
    mutate(
      Municipios = iconv(Municipios, from = "latin1", to = "UTF-8"),
      Municipios = stri_trans_general(Municipios, "Latin-ASCII"),
      Municipios = toupper(Municipios)
    ) %>%
    separate_rows(Municipios, sep = ";") %>%
    mutate(Municipios = trimws(Municipios))
  
  indices_37_mun <- df_mun %>%
    group_by(Municipios) %>%
    summarise(
      annual_prec = mean(annual_prec, na.rm = TRUE),
      annual_dry_days = mean(annual_dry_days, na.rm = TRUE),
      annual_wet_days = mean(annual_wet_days, na.rm = TRUE),
      annual_dry_spells = mean(annual_dry_spells, na.rm = TRUE),
      annual_wet_spells = mean(annual_wet_spells, na.rm = TRUE),
      mean_dry_duration = mean(mean_dry_duration, na.rm = TRUE),
      mean_wet_duration = mean(mean_wet_duration, na.rm = TRUE),
      mean_wet_intensity = mean(mean_wet_intensity, na.rm = TRUE),
      max_dry_spell = max(max_dry_spell, na.rm = TRUE),
      max_wet_spell = max(max_wet_spell, na.rm = TRUE),
      n_records = n(),
      .groups = "drop"
    )
  
  indices_37_mun$Municipios <- gsub(".*QUIRA.*", "IQUIRA", indices_37_mun$Municipios)
  indices_37_mun$Municipios <- gsub(".*ELA.*AS.*", "ELIAS", indices_37_mun$Municipios)
  indices_37_mun$Municipios <- gsub(".*GARZ.*N.*", "GARZON", indices_37_mun$Municipios)
  indices_37_mun$Municipios <- gsub(".*NATAGA.*|.*NAA.*TAGA.*", "NATAGA", indices_37_mun$Municipios)
  indices_37_mun$Municipios <- gsub(".*SAN AGUST.*N.*", "SAN AGUSTIN", indices_37_mun$Municipios)
  indices_37_mun$Municipios <- gsub(".*SANTA MAR.*A.*", "SANTA MARIA", indices_37_mun$Municipios)
  indices_37_mun$Municipios <- gsub(".*TIMANA.*", "TIMANA", indices_37_mun$Municipios)
  indices_37_mun$Municipios <- gsub(".*YAGUARA.*", "YAGUARA", indices_37_mun$Municipios)
  
  write.csv(indices_37_mun, 
            paste0(oDir, "/indices_", tolower(ssp), "_", per, "_", tolower(region), "_mun.csv"),
            row.names = FALSE)

  }
}



library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
oDir <- "Y:/1.Data/Results/Cambio_Climatico/indices"
sspList <- c("SSP126", "SSP245", "SSP585")
perList <- c("2021-2040", "2041-2060")
region <- "HUILA"
dir.create(oDir, recursive = TRUE, showWarnings = FALSE)

# leer todos los archivos
all_indices <- list()
for (ssp in sspList) {
  for (per in perList) {
    
    f <- file.path(
      oDir,
      paste0("indices_", tolower(ssp), "_", per, "_huila_ann.csv")
    )
    
    tmp <- read_csv(f, show_col_types = FALSE) %>%
      mutate(
        SSP = ssp,
        Period = per
      )
    
    all_indices[[paste(ssp, per, sep = "_")]] <- tmp
  }
}
df <- bind_rows(all_indices)
# índice a graficar
index_var_ls <- c("annual_prec","annual_dry_days","annual_dry_spells","mean_wet_intensity","max_dry_spell")

# índices y nombres en español
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
  max_wet_spell = "Máxima duración de periodo húmedo (días)"
)

for (index_var in names(index_labels)) {
  
  df_plot <- df %>%
    filter(Departamentos == region) %>%
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
    geom_line(linewidth = 1.3) +
    geom_smooth(
      method = "lm",
      se = FALSE,
      linewidth = 0.9,
      linetype = "dotted"
    ) +
    facet_wrap(~SSP, ncol = 1) +
    labs(
      x = "Año",
      y = index_labels[[index_var]],
      title = paste0("Evolución anual: ", index_labels[[index_var]], " - ", region),
      subtitle = "Línea continua = promedio; sombra = percentiles 10-90; línea punteada = tendencia"
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )
  
  print(p)
  
  ggsave(
    filename = file.path(oDir, paste0("timeseries_", index_var, "_", region, ".png")),
    plot = p,
    width = 8,
    height = 7,
    dpi = 300
  )
}



for (index_var in index_var_ls){
  # resumen anual: promedio + dispersión
  df_plot <- df %>%
    filter(Departamentos == region) %>%
    group_by(SSP, year) %>%
    summarise(
      mean_value = mean(.data[[index_var]], na.rm = TRUE),
      p10 = quantile(.data[[index_var]], 0.10, na.rm = TRUE),
      p90 = quantile(.data[[index_var]], 0.90, na.rm = TRUE),
      min_value = min(.data[[index_var]], na.rm = TRUE),
      max_value = max(.data[[index_var]], na.rm = TRUE),
      .groups = "drop"
    )
  # gráfico con shade p10-p90
  p <- ggplot(df_plot, aes(x = year, y = mean_value, color = SSP, fill = SSP)) +
    geom_ribbon(
      aes(ymin = p10, ymax = p90),
      alpha = 0.20,
      color = NA
    ) +
    geom_line(linewidth = 1.3) +
    facet_wrap(~SSP, ncol = 1) +
    labs(
      x = "Año",
      y = index_var,
      title = paste0("Evolución anual de ", index_var, " - ", region),
      subtitle = "Línea = promedio; sombra = percentiles 10-90"
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )
  
  
  
  p <- ggplot(df_plot, aes(x = year, y = mean_value, color = SSP, fill = SSP)) +
    # shade
    geom_ribbon(
      aes(ymin = p10, ymax = p90),
      alpha = 0.20,
      color = NA
    ) +
    # main line
    geom_line(linewidth = 1.3) +
    # dotted trend line
    geom_smooth(
      method = "lm",
      se = FALSE,
      linewidth = 0.9,
      linetype = "dotted"
    ) +
    facet_wrap(~SSP, ncol = 1) +
    labs(
      x = "Año",
      y = index_var,
      title = paste0("Evolución anual de ", index_var, " - ", region),
      subtitle = "Línea = promedio; sombra = percentiles 10-90"
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )
  
  print(p)
  ggsave(
    filename = file.path(oDir, paste0("timeseries_", index_var, "_", region, ".png")),
    plot = p,
    width = 8,
    height = 7,
    dpi = 300
  )
  
}



