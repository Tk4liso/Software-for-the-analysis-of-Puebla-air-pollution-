#Series de tiempo con intervalos de confianza

library(dplyr)
library(ggplot2)
library(forecast)

# ==== Load and Prepare Data ====
data <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\Dataset\\NINFAS\\NINFAS_2020-2024_combinado.csv")
data <- data %>% filter(complete.cases(.))

# Combine date and time columns
data$DateTime <- as.POSIXct(paste(data$FECHA, data$Horas), format="%d/%m/%Y %H:%M:%S")

# Aggregate daily means
daily_data <- data %>% 
  group_by(Date = as.Date(DateTime)) %>%
  summarize(
    O3 = mean(O3), 
    NO2 = mean(NO2), 
    CO = mean(CO), 
    SO2 = mean(SO2), 
    PM10 = mean(PM.10), 
    PM25 = mean(PM.2.5)
  )

#Filter complete cases
daily_data <- daily_data %>% filter(complete.cases(.))

#Define start date for time series
start_date <- min(daily_data$Date)
start_year <- as.numeric(format(start_date, "%Y"))
start_month <- as.numeric(format(start_date, "%j")) / 30  # Approximate to month

#Create list of daily time series
ts_data <- list(
  O3 = ts(daily_data$O3, start = c(start_year, start_month), frequency = 365),
  NO2 = ts(daily_data$NO2, start = c(start_year, start_month), frequency = 365),
  CO = ts(daily_data$CO, start = c(start_year, start_month), frequency = 365),
  SO2 = ts(daily_data$SO2, start = c(start_year, start_month), frequency = 365),
  PM10 = ts(daily_data$PM10, start = c(start_year, start_month), frequency = 365),
  PM25 = ts(daily_data$PM25, start = c(start_year, start_month), frequency = 365)
)

# -> Modeling and Forecasting <-

# Apply auto.arima and forecast (15 days to keep intervals tighter)
forecasts <- lapply(ts_data, function(ts_series) {
  model <- auto.arima(ts_series)
  forecast(model, h = 15)
})

#Create Forecast Dataframes

forecast_dfs <- lapply(names(forecasts), function(name) {
  forecast_data <- forecasts[[name]]
  forecast_dates <- seq(max(daily_data$Date) + 1, length.out = 15, by = "days")
  
  data.frame(
    Date = c(daily_data$Date, forecast_dates),
    Concentration = c(daily_data[[name]], as.numeric(forecast_data$mean)),
    Lower = c(rep(NA, nrow(daily_data)), as.numeric(forecast_data$lower[, 2])),
    Upper = c(rep(NA, nrow(daily_data)), as.numeric(forecast_data$upper[, 2])),
    Pollutant = name,
    Type = rep(c("Observed", "Forecast"), c(nrow(daily_data), 15))
  )
})

# Combine all forecast dataframes
df_facet_pred <- do.call(rbind, forecast_dfs)

# ==== Faceted Plot with Confidence Intervals ====

ggplot(df_facet_pred, aes(x = Date, y = Concentration, color = Type)) + 
  geom_line() +
  geom_ribbon(data = df_facet_pred %>% filter(Type == "Forecast"),
              aes(x = Date, ymin = Lower, ymax = Upper, fill = Type), 
              alpha = 0.2, inherit.aes = FALSE) +
  facet_wrap(~ Pollutant, scales = "free_y") + 
  labs(
    x = "Date", 
    y = "Concentration", 
    title = "Time Series of Pollutants with Forecasts and Confidence Intervals for the next 15 days"
  ) +
  scale_color_manual(values = c("Observed" = "gray", "Forecast" = "red")) +
  scale_fill_manual(values = c("Forecast" = "red")) +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5)
  )

# ==== Individual Plots for PM2.5 and PM10 ====

# Function to plot an individual pollutant
plot_individual <- function(df, pollutant, color = "steelblue") {
  df_filtered <- df %>% filter(Pollutant == pollutant)
  
  ggplot(df_filtered, aes(x = Date, y = Concentration)) +
    geom_line(aes(color = Type)) +
    geom_ribbon(data = df_filtered %>% filter(Type == "Forecast"),
                aes(x = Date, ymin = Lower, ymax = Upper, fill = Type),
                alpha = 0.2, inherit.aes = FALSE) +
    labs(
      title = paste("Forecast for", pollutant),
      x = "Date", 
      y = "Concentration"
    ) +
    scale_color_manual(values = c("Observed" = "gray", "Forecast" = color)) +
    scale_fill_manual(values = c("Forecast" = color)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")
}

# PM2.5 plot
plot_individual(df_facet_pred, "PM25", color = "darkgreen")

# PM10 plot
plot_individual(df_facet_pred, "PM10", color = "darkorange")


# ESPAÑOL
# ==== Librerías necesarias ====
library(dplyr)
library(ggplot2)
library(forecast)

# ==== Cargar y preparar los datos ====
data <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\Dataset\\NINFAS\\NINFAS_2020-2024_combinado.csv")
data <- data %>% filter(complete.cases(.))

# Combinar columnas de fecha y hora
data$DateTime <- as.POSIXct(paste(data$FECHA, data$Horas), format="%d/%m/%Y %H:%M:%S")

# Agrupar por fecha diaria y calcular medias
daily_data <- data %>% 
  group_by(Date = as.Date(DateTime)) %>%
  summarize(
    O3 = mean(O3), 
    NO2 = mean(NO2), 
    CO = mean(CO), 
    SO2 = mean(SO2), 
    PM10 = mean(PM.10), 
    PM25 = mean(PM.2.5)
  )

# Filtrar datos completos
daily_data <- daily_data %>% filter(complete.cases(.))

# Definir fecha de inicio para las series de tiempo
start_date <- min(daily_data$Date)
start_year <- as.numeric(format(start_date, "%Y"))
start_month <- as.numeric(format(start_date, "%j")) / 30  # Aproximación al mes

# Crear lista de series de tiempo diarias
ts_data <- list(
  O3 = ts(daily_data$O3, start = c(start_year, start_month), frequency = 365),
  NO2 = ts(daily_data$NO2, start = c(start_year, start_month), frequency = 365),
  CO = ts(daily_data$CO, start = c(start_year, start_month), frequency = 365),
  SO2 = ts(daily_data$SO2, start = c(start_year, start_month), frequency = 365),
  PM10 = ts(daily_data$PM10, start = c(start_year, start_month), frequency = 365),
  PM25 = ts(daily_data$PM25, start = c(start_year, start_month), frequency = 365)
)

# ==== Modelado ARIMA y predicción ====

# Aplicar auto.arima y forecast (15 días para intervalos más confiables)
forecasts <- lapply(ts_data, function(ts_series) {
  model <- auto.arima(ts_series)
  forecast(model, h = 15)
})

# ==== Crear dataframes de predicciones e intervalos ====

forecast_dfs <- lapply(names(forecasts), function(name) {
  forecast_data <- forecasts[[name]]
  forecast_dates <- seq(max(daily_data$Date) + 1, length.out = 15, by = "days")
  
  data.frame(
    Date = c(daily_data$Date, forecast_dates),
    Concentration = c(daily_data[[name]], as.numeric(forecast_data$mean)),
    Lower = c(rep(NA, nrow(daily_data)), as.numeric(forecast_data$lower[, 2])),
    Upper = c(rep(NA, nrow(daily_data)), as.numeric(forecast_data$upper[, 2])),
    Contaminante = name,
    Tipo = rep(c("Observado", "Predicción"), c(nrow(daily_data), 15))
  )
})

# Combinar todos los dataframes
df_facet_pred <- do.call(rbind, forecast_dfs)

# ==== Gráfico facetado con intervalos de confianza ====

ggplot(df_facet_pred, aes(x = Date, y = Concentration, color = Tipo)) + 
  geom_line() +
  geom_ribbon(data = df_facet_pred %>% filter(Tipo == "Predicción"),
              aes(x = Date, ymin = Lower, ymax = Upper, fill = Tipo), 
              alpha = 0.2, inherit.aes = FALSE) +
  facet_wrap(~ Contaminante, scales = "free_y") + 
  labs(x = "Fecha", y = "Concentración", title = "Series de tiempo de contaminantes con predicciones e intervalos de confianza") +
  scale_color_manual(values = c("Observado" = "gray", "Predicción" = "red")) +
  scale_fill_manual(values = c("Predicción" = "red")) +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5)
  )

# ==== Gráficos individuales para PM2.5 y PM10 ====

# Función para plotear un contaminante específico
plot_individual <- function(df, contaminante, color = "steelblue") {
  df_filtrado <- df %>% filter(Contaminante == contaminante)
  
  ggplot(df_filtrado, aes(x = Date, y = Concentration)) +
    geom_line(aes(color = Tipo)) +
    geom_ribbon(data = df_filtrado %>% filter(Tipo == "Predicción"),
                aes(x = Date, ymin = Lower, ymax = Upper, fill = Tipo),
                alpha = 0.2, inherit.aes = FALSE) +
    labs(title = paste("Predicción de", contaminante),
         x = "Fecha", y = "Concentración") +
    scale_color_manual(values = c("Observado" = "gray", "Predicción" = color)) +
    scale_fill_manual(values = c("Predicción" = color)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          plot.title = element_text(hjust = 0.5)) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")
}

# PM2.5
plot_individual(df_facet_pred, "PM25", color = "darkgreen")

# PM10
plot_individual(df_facet_pred, "PM10", color = "darkorange")
