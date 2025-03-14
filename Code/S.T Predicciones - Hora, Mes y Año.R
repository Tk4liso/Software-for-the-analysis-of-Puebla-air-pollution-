#Series de tiempo - Hora, Mes y Año


library(forecast)
library(ggplot2)
library(dplyr)
library(tseries)

# ==== Hora ====

library(dplyr)
library(ggplot2)
library(tidyr)
library(forecast)

data <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\Dataset\\NINFAS\\NINFAS_2020-2024_combinado.csv")
data$Datetime <- as.POSIXct(paste(data$FECHA, data$Horas), format="%d/%m/%Y %H:%M:%S", tz="America/Mexico_City")

#Agrupar por hora y calcular la concentración promedio por hora para cada contaminante
hourly_data <- data %>%
  mutate(Hour = format(Datetime, "%H")) %>%  
  group_by(Hour) %>%
  summarise(
    O3 = mean(O3, na.rm = TRUE),
    NO2 = mean(NO2, na.rm = TRUE),
    CO = mean(CO, na.rm = TRUE),
    SO2 = mean(SO2, na.rm = TRUE),
    PM10 = mean(PM.10, na.rm = TRUE),
    PM2.5 = mean(PM.2.5, na.rm = TRUE)
  )

#Checar que Hour sea num. y filtrar NA's
hourly_data <- hourly_data %>%
  mutate(Hour = as.numeric(Hour)) %>%
  filter(!is.na(Hour) & Hour >= 0 & Hour <= 23)

#Ajustar ARIMA y crear datos de predicción para las próximas 24 horas
create_arima_forecast <- function(series, h = 24) {
  ts_series <- ts(series, frequency = 24)
  arima_model <- auto.arima(ts_series)
  forecast_data <- forecast(arima_model, h = h)
  return(list(
    forecast = as.numeric(forecast_data$mean),
    model = arima_model
  ))
}

#Ajustar modelo ARIMA y obtener predicciones para las próx. 24 horas por contaminante
forecast_list <- list()
for (contaminant in c("O3", "NO2", "CO", "SO2", "PM10", "PM2.5")) {
  result <- create_arima_forecast(hourly_data[[contaminant]])
  forecast_list[[contaminant]] <- result$forecast
}

#Crear dataframe con predicciones
forecast_data <- bind_rows(lapply(names(forecast_list), function(contaminant) {
  data.frame(
    Hour = (max(hourly_data$Hour) + 1):(max(hourly_data$Hour) + 24),
    Concentracion = forecast_list[[contaminant]],
    Contaminante = contaminant,
    Tipo = "Predicción"
  )
}))

#Convertir el dataframe histórico a formato largo
hourly_data_long <- hourly_data %>%
  pivot_longer(cols = O3:PM2.5, names_to = "Contaminante", values_to = "Concentracion") %>%
  mutate(Tipo = "Histórico") %>%
  bind_rows(forecast_data)

#Graficar
ggplot(hourly_data_long, aes(x = Hour, y = Concentracion, color = Contaminante, linetype = Tipo)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Contaminante, scales = "free_y") +
  labs(title = "Concentración promedio de contaminantes por hora con predicción ARIMA",
       x = "Hora del día",
       y = "Concentración promedio") +
  theme_minimal()


# ==== Mes ====

library(dplyr)
library(ggplot2)
library(tidyr)
library(forecast)

# Leer los datos
data <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\Dataset\\NINFAS\\NINFAS_2020-2024_combinado.csv")
data$Datetime <- as.POSIXct(paste(data$FECHA, data$Horas), format="%d/%m/%Y %H:%M:%S", tz="America/Mexico_City")

# Agrupar por día del mes y calcular la concentración promedio
daily_data <- data %>%
  mutate(Day = as.integer(format(Datetime, "%d"))) %>%
  group_by(Day) %>%
  summarise(
    O3 = mean(O3, na.rm = TRUE),
    NO2 = mean(NO2, na.rm = TRUE),
    CO = mean(CO, na.rm = TRUE),
    SO2 = mean(SO2, na.rm = TRUE),
    PM10 = mean(PM.10, na.rm = TRUE),
    PM2.5 = mean(PM.2.5, na.rm = TRUE),
    .groups = 'drop'  # Asegura que los grupos se eliminen después de la summarización
  ) %>%
  filter(!is.na(Day))  # Elimina cualquier fila con NA en la columna 'Day'

# Verifica el rango de días y la existencia de NA en la columna 'Day'
print(range(daily_data$Day))
print(sum(is.na(daily_data$Day)))

# Función para ajustar ARIMA y predecir
create_arima_forecast <- function(series, h = 5) {
  ts_series <- ts(series, frequency = 30)  # Usamos frecuencia de 30 días por mes
  arima_model <- auto.arima(ts_series)
  forecast_data <- forecast(arima_model, h = h)
  return(list(
    forecast = as.numeric(forecast_data$mean),
    model = arima_model
  ))
}

# Ajustar modelo ARIMA y obtener predicciones
forecast_list <- list()
for (contaminant in c("O3", "NO2", "CO", "SO2", "PM10", "PM2.5")) {
  result <- create_arima_forecast(daily_data[[contaminant]])
  forecast_list[[contaminant]] <- result$forecast
}

# Crear dataframe con predicciones
forecast_data <- bind_rows(lapply(names(forecast_list), function(contaminant) {
  data.frame(
    Day = (max(daily_data$Day, na.rm = TRUE) + 1):(max(daily_data$Day, na.rm = TRUE) + 5),
    Concentracion = forecast_list[[contaminant]],
    Contaminante = contaminant,
    Tipo = "Predicción"
  )
}))

# Convertir el dataframe histórico a formato largo
daily_data_long <- daily_data %>%
  pivot_longer(cols = O3:PM2.5, names_to = "Contaminante", values_to = "Concentracion") %>%
  mutate(Tipo = "Histórico") %>%
  bind_rows(forecast_data)

# Graficar
ggplot(daily_data_long, aes(x = Day, y = Concentracion, color = Contaminante, linetype = Tipo)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Contaminante, scales = "free_y") +
  labs(title = "Concentración promedio de contaminantes por día del mes con predicción ARIMA",
       x = "Día del mes",
       y = "Concentración promedio") +
  theme_minimal()


# CALCULAR LA PRECICIÓN

# Divide los datos en entrenamiento (por ejemplo, los primeros 25 días) y prueba (últimos 5 días)
train_data <- daily_data %>% filter(Day <= 25)
test_data <- daily_data %>% filter(Day > 25)

# Función para ajustar ARIMA y predecir
create_arima_forecast <- function(series, h = 5) {
  ts_series <- ts(series, frequency = 30)  # Usamos frecuencia de 30 días por mes
  arima_model <- auto.arima(ts_series)
  forecast_data <- forecast(arima_model, h = h)
  return(list(
    forecast = as.numeric(forecast_data$mean),
    model = arima_model
  ))
}

# Ajustar el modelo ARIMA y obtener predicciones para el conjunto de prueba
forecast_list <- list()
accuracy_list <- list()

for (contaminant in c("O3", "NO2", "CO", "SO2", "PM10", "PM2.5")) {
  # Usamos los datos de entrenamiento
  result <- create_arima_forecast(train_data[[contaminant]])
  
  # Guardar las predicciones
  forecast_list[[contaminant]] <- result$forecast
  
  # Calcular la precisión en los datos de prueba
  real_values <- test_data[[contaminant]]
  predicted_values <- result$forecast[1:length(real_values)]
  
  # Calcular accuracy y guardar resultados
  accuracy_list[[contaminant]] <- accuracy(predicted_values, real_values)
}

# Ver los resultados de accuracy para cada contaminante
accuracy_list


# ==== Año ====

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

# Calcular el promedio de las concentraciones de O3
mean(daily_data$O3, na.rm = TRUE)


# ==== Estilo Jonny ====
ggplot(df_facet_pred, aes(x = Date, y = Concentration, color = Contaminante, linetype = Tipo)) + 
  geom_line(size = 0.8) +  # Ajustar el grosor de las líneas
  facet_wrap(~ Contaminante, scales = "free_y") + 
  labs(title = "Concentración promedio de contaminantes por día con predicción ARIMA",
       x = "Fecha",
       y = "Concentración promedio") +
  scale_color_manual(values = c("O3" = "blue", "NO2" = "green", "CO" = "purple", "SO2" = "orange", "PM10" = "red", "PM25" = "brown")) + 
  scale_linetype_manual(values = c("Observado" = "solid", "Predicción" = "dashed")) +  # Diferenciar observaciones y predicciones
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +  # Mostrar etiquetas cada 2 meses
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Rotar etiquetas y reducir tamaño de fuente
    plot.title = element_text(hjust = 0.5)  # Centrar el título
  )
