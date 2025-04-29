#Presentación - Arima vs TBats

# ==== TBats ====

library(dplyr)
library(ggplot2)
library(tidyr)
library(forecast)
library(lubridate)
library(readr)

data <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\Dataset\\NINFAS\\NINFAS_2020-2024_combinado.csv")
data$DateTime <- as.POSIXct(paste(data$FECHA, data$Horas), format="%d/%m/%Y %H:%M:%S")
data <- na.omit(data)

# Agrupar por día y calcular la media de cada contaminante
data_daily <- data %>%
  group_by(Date = as.Date(DateTime)) %>%
  summarise(
    O3_daily_mean = mean(O3, na.rm = TRUE),
    NO2_daily_mean = mean(NO2, na.rm = TRUE),
    CO_daily_mean = mean(CO, na.rm = TRUE),
    SO2_daily_mean = mean(SO2, na.rm = TRUE),
    PM10_daily_mean = mean(PM.10, na.rm = TRUE),
    PM2.5_daily_mean = mean(PM.2.5, na.rm = TRUE)
  )

#Ajustar modelos TBATS y generar pronósticos para cada contaminante
tbats_forecasts <- list()

for (contaminante in c("O3_daily_mean", "NO2_daily_mean", "CO_daily_mean", "SO2_daily_mean", "PM10_daily_mean", "PM2.5_daily_mean")) {
  #Convertir a TS (asumiendo frecuencia diaria)
  ts_data <- ts(data_daily[[contaminante]], frequency = 365)
  
  fit_tbats <- tbats(ts_data)
  
  #Generar pronóstico para los próximos 30 días
  forecast_tbats <- forecast(fit_tbats, h = 30)
  
  tbats_forecasts[[contaminante]] <- forecast_tbats
}

#Convertir el dataset a formato largo para graficar con ggplot
data_long <- data_daily %>%
  pivot_longer(cols = c("O3_daily_mean", "NO2_daily_mean", "CO_daily_mean", "SO2_daily_mean", "PM10_daily_mean", "PM2.5_daily_mean"),
               names_to = "Contaminante",
               values_to = "Concentracion") %>%
  mutate(Tipo = "Histórico")

#Agregar los pronósticos al dataset para graficar
data_with_forecast <- data_long

for (contaminante in names(tbats_forecasts)) {
  forecast_dates <- seq(max(data_daily$Date) + 1, by = "day", length.out = 30)
  
  forecast_data <- data.frame(
    Date = forecast_dates,
    Concentracion = tbats_forecasts[[contaminante]]$mean,
    Contaminante = contaminante,
    Tipo = "Pronóstico"  #Campo para distinguir los pronósticos
  )
  
  # Añadir las predicciones al dataset original
  data_with_forecast <- rbind(data_with_forecast, forecast_data)
}

data_with_forecast$Tipo <- ifelse(is.na(data_with_forecast$Tipo), "Histórico", data_with_forecast$Tipo)
ggplot(data_with_forecast, aes(x = Date, y = Concentracion, color = Contaminante)) +
  geom_line(aes(linetype = Tipo, color = ifelse(Tipo == "Pronóstico", "Pronóstico", Contaminante)), linewidth = 1)+  
  scale_color_manual(values = c("Pronóstico" = "red",  # Color rojo para los pronósticos
                                "CO_daily_mean" = "salmon", 
                                "NO2_daily_mean" = "goldenrod",
                                "O3_daily_mean" = "green",
                                "PM10_daily_mean" = "cyan",
                                "PM2.5_daily_mean" = "lightblue",
                                "SO2_daily_mean" = "magenta")) +
  scale_linetype_manual(values = c("Histórico" = "solid", "Pronóstico" = "solid")) +  # Líneas punteadas para pronósticos
  facet_wrap(~Contaminante, scales = "free_y") +
  labs(title = "Concentración Diaria Promedio de Contaminantes con Pronóstico TBATS",
       x = "Fecha", y = "Concentración") +
  theme_minimal()


#Graficas por separado
plots <- list()
contaminantes <- c("CO_daily_mean", "NO2_daily_mean", "O3_daily_mean", 
                   "PM10_daily_mean", "PM2.5_daily_mean", "SO2_daily_mean")

for (contaminante in contaminantes) {
  plot <- ggplot(subset(data_with_forecast, Contaminante == contaminante), 
                 aes(x = Date, y = Concentracion, color = Contaminante)) +
    geom_line(aes(linetype = Tipo, color = ifelse(Tipo == "Pronóstico", "Pronóstico", Contaminante)), size = 1) +
    scale_color_manual(values = c("Pronóstico" = "red",  # Color rojo para pronósticos
                                  contaminante = scales::hue_pal()(1))) +  # Mantener el color único para cada contaminante
    scale_linetype_manual(values = c("Histórico" = "solid", "Pronóstico" = "solid")) +  
    labs(title = paste("Concentración Diaria Promedio de", contaminante),
         x = "Fecha", y = "Concentración") +
    theme_minimal()
  
  # Almacenar la gráfica en la lista
  plots[[contaminante]] <- plot
}

print(plots$CO_daily_mean)
print(plots$NO2_daily_mean)
print(plots$O3_daily_mean)
print(plots$SO2_daily_mean)
print(plots$PM10_daily_mean)
print(plots$PM2.5_daily_mean)

accuracy(tbats_forecasts$CO_daily_mean)
accuracy(tbats_forecasts$NO2_daily_mean)
accuracy(tbats_forecasts$O3_daily_mean)
accuracy(tbats_forecasts$SO2_daily_mean)
accuracy(tbats_forecasts$PM10_daily_mean)
accuracy(tbats_forecasts$PM2.5_daily_mean)


#OTRA FORMA DE CALCULAR LA PRECISION

# Calcular la precisión de las predicciones para cada contaminante
accuracy_tbats_list <- list()

for (contaminante in c("O3_daily_mean", "NO2_daily_mean", "CO_daily_mean", "SO2_daily_mean", "PM10_daily_mean", "PM2.5_daily_mean")) {
  # Convertir a serie de tiempo (TS) asumiendo frecuencia diaria
  ts_data <- ts(data_daily[[contaminante]], frequency = 365)
  
  # Ajustar el modelo TBATS
  fit_tbats <- tbats(ts_data)
  
  # Generar pronóstico para los próximos 30 días
  forecast_tbats <- forecast(fit_tbats, h = 30)
  
  # Comparar las predicciones con los últimos 30 días de datos observados (si existen)
  if (nrow(data_daily) >= 30) {
    # Extraer los últimos 30 días observados
    real_values <- tail(ts_data, 30)
    
    # Tomar las primeras 30 predicciones
    predicted_values <- forecast_tbats$mean[1:30]
    
    # Calcular precisión y almacenarla en la lista
    accuracy_tbats <- accuracy(predicted_values, real_values)
    accuracy_tbats_list[[contaminante]] <- accuracy_tbats
  }
}

# Mostrar las métricas de precisión para cada contaminante
accuracy_tbats_list

# ==== ARIMA ====

library(dplyr)
library(ggplot2)
library(forecast)

data <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\Dataset\\NINFAS\\NINFAS_2020-2024_combinado.csv")
data <- data %>% filter(complete.cases(.))

# Combinar columnas fecha y hora
data$DateTime <- as.POSIXct(paste(data$FECHA, data$Horas), format="%d/%m/%Y %H:%M:%S")

# Convertir a serie de tiempo diaria
daily_data <- data %>% 
  group_by(Date = as.Date(DateTime)) %>%
  summarize(O3 = mean(O3), NO2 = mean(NO2), CO = mean(CO), SO2 = mean(SO2), PM10 = mean(PM.10), PM25 = mean(PM.2.5))

# Filtrar datos completos
daily_data <- daily_data %>% filter(complete.cases(.))

# Determinar la fecha de inicio (primer día en los datos)
start_date <- min(daily_data$Date)
start_year <- as.numeric(format(start_date, "%Y"))
start_month <- as.numeric(format(start_date, "%j")) / 30  # Aproximación al mes

# Crear series de tiempo diarias, usando la fecha de inicio detectada
ts_data <- list(
  O3 = ts(daily_data$O3, start = c(start_year, start_month), frequency = 365),
  NO2 = ts(daily_data$NO2, start = c(start_year, start_month), frequency = 365),
  CO = ts(daily_data$CO, start = c(start_year, start_month), frequency = 365),
  SO2 = ts(daily_data$SO2, start = c(start_year, start_month), frequency = 365),
  PM10 = ts(daily_data$PM10, start = c(start_year, start_month), frequency = 365),
  PM25 = ts(daily_data$PM25, start = c(start_year, start_month), frequency = 365)
)

# Aplicar auto.arima y forecast para cada contaminante
forecasts <- lapply(ts_data, function(ts_data) {
  model <- auto.arima(ts_data)
  forecast(model, h = 30)
})

# Crear dataframes de predicciones
forecast_dfs <- lapply(names(forecasts), function(name) {
  forecast_data <- forecasts[[name]]
  data.frame(
    Date = c(daily_data$Date, seq(max(daily_data$Date) + 1, length.out = 30, by = "days")),
    Concentration = c(daily_data[[name]], as.numeric(forecast_data$mean)),
    Contaminante = name,
    Tipo = rep(c("Observado", "Predicción"), c(nrow(daily_data), 30))
  )
})

# Combinar todos los dataframes de predicciones
df_facet_pred <- do.call(rbind, forecast_dfs)

# Graficar con facetado 
ggplot(df_facet_pred, aes(x = Date, y = Concentration, color = Tipo)) + 
  geom_line() + 
  facet_wrap(~ Contaminante, scales = "free_y") + 
  labs(x = "Fecha", y = "Concentración", title = "Series de tiempo de contaminantes con predicciones") +
  scale_color_manual(values = c("Observado" = "blue", "Predicción" = "red")) +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") + # Mostrar etiquetas cada 2 meses
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Rotar etiquetas y reducir tamaño de fuente
    plot.title = element_text(hjust = 0.5)  # Centrar el título
  )


#CALCULAR LA PRECICIÓN

# Calcular la precisión de las predicciones para cada contaminante
accuracy_list <- lapply(names(forecasts), function(name) {
  # Extraer el modelo y los datos
  forecast_data <- forecasts[[name]]
  real_values <- tail(daily_data[[name]], 30)  # Últimos 30 días observados
  predicted_values <- head(as.numeric(forecast_data$mean), length(real_values))
  
  # Calcular la precisión comparando los valores reales con las predicciones
  accuracy(predicted_values, real_values)
})

# Mostrar las métricas de precisión para cada contaminante
names(accuracy_list) <- names(forecasts)
accuracy_list
