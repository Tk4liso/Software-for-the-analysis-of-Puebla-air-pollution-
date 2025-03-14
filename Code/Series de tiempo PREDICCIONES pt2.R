#Series de tiempo - PREDICCIÓN |NINFAS - 2023|

library(forecast)
library(ggplot2)
library(dplyr)
library(tseries)

# ==== Prueba 1 - O3 solito ====
data<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\Dataset\\NINFAS\\NINFAS_2020-2024_combinado.csv")
head(data)
str(data)
summary(data)

#Combinar columnas fecha y hora en un solo vector de tipo POSIXct para poder convertir la data a un objeto de serie de tiempo
data$DateTime <- as.POSIXct(paste(data$FECHA, data$Horas), format="%d/%m/%Y %H:%M:%S")
head(data)

#Convertir a serie de tiempo diaria (media diaria por día)
daily_data<-data %>% 
  group_by(Date = as.Date(DateTime)) %>%
  summarize(O3 = mean(O3))

#Crear serie de tiempo diaria
ts_daily <- ts(daily_data$O3, start = c(2020, 5), frequency = 365)
plot(ts_daily, main = "Serie de tiempo de O3", ylab = "O3", xlab= "Tiempo")

#Aplicar la Diferenciación para intentar hacerla estacionaria
original_ts <- ts_daily 
ts_daily<-diff(ts_daily)
adf.test(ts_daily)


model<-auto.arima(ts_daily)
summary(model)
forecast_values<-forecast(model,h=30) #h es el horizonte de predicción

#Graficar la serie diferenciada
autoplot(forecast(forecast_values,3)) + labs(x="Tiempo", y="Diferencia en la concentración de O3")

forecast(model, 3)
accuracy(forecast_values)

# ==== Todos de una en una función====

library(forecast)
library(ggplot2)
library(dplyr)
library(tseries)

data <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\Dataset\\NINFAS\\NINFAS_2020-2024_combinado.csv")

# Combinar columnas fecha y hora en un solo vector de tipo POSIXct para poder convertir la data a un objeto de serie de tiempo
data$DateTime <- as.POSIXct(paste(data$FECHA, data$Horas), format="%d/%m/%Y %H:%M:%S")

# Función para procesar cada contaminante
procesar_contaminante <- function(data, contaminante) {
  # Convertir a serie de tiempo diaria (media diaria por día)
  daily_data <- data %>% 
    group_by(Date = as.Date(DateTime)) %>%
    summarize(Concentracion = mean(get(contaminante), na.rm = TRUE))
  
  # Crear serie de tiempo diaria
  ts_daily <- ts(daily_data$Concentracion, start = c(2020, 5), frequency = 365)
  plot(ts_daily, main = paste("Serie de tiempo de", contaminante), ylab = contaminante, xlab = "Tiempo")
  
  # Aplicar la Diferenciación para intentar hacerla estacionaria
  original_ts <- ts_daily
  ts_daily <- diff(ts_daily)
  adf.test(ts_daily)
  
  # Crear el modelo ARIMA
  model <- auto.arima(ts_daily)
  summary(model)
  
  # Predecir valores futuros
  forecast_values <- forecast(model, h = 30) # h es el horizonte de predicción
  
  # Graficar la serie diferenciada y las predicciones
  autoplot(forecast(forecast_values, 3)) + labs(x = "Tiempo", y = paste("Diferencia en la concentración de", contaminante))
  
  # Mostrar las predicciones
  print(forecast(model, 3))
  
  # Mostrar la precisión del modelo
  print(accuracy(forecast_values))
}

# Procesar cada contaminante
contaminantes <- c("O3", "NO2", "CO", "SO2", "PM.10", "PM.2.5")
lapply(contaminantes, procesar_contaminante, data = data)

# ==== Todos por separado ====

library(forecast)
library(ggplot2)
library(dplyr)
library(tseries)

data <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\Dataset\\NINFAS\\NINFAS_2020-2024_combinado.csv")
data <- data %>% filter(complete.cases(.))

# Combinar columnas fecha y hora
data$DateTime <- as.POSIXct(paste(data$FECHA, data$Horas), format="%d/%m/%Y %H:%M:%S")

# Convertir a serie de tiempo diaria
daily_data <- data %>% 
  group_by(Date = as.Date(DateTime)) %>%
  summarize(O3 = mean(O3), NO2 = mean(NO2), CO = mean(CO), SO2 = mean(SO2), PM10 = mean(PM.10), PM25 = mean(PM.2.5))

any(is.na(daily_data$Date))
daily_data <- daily_data %>% filter(complete.cases(.))
any(is.na(daily_data$Date))

# Crear series de tiempo diarias
ts_data <- list(
  O3 = ts(daily_data$O3, start = c(2020, 5), frequency = 365),
  NO2 = ts(daily_data$NO2, start = c(2020, 5), frequency = 365),
  CO = ts(daily_data$CO, start = c(2020, 5), frequency = 365),
  SO2 = ts(daily_data$SO2, start = c(2020, 5), frequency = 365),
  PM10 = ts(daily_data$PM10, start = c(2020, 5), frequency = 365),
  PM25 = ts(daily_data$PM25, start = c(2020, 5), frequency = 365)
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
  theme_minimal()

# ==== Ya ni se ====

library(forecast)
library(ggplot2)
library(dplyr)
library(tseries)

data <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\Dataset\\NINFAS\\NINFAS_2020-2024_combinado.csv")

# Combinar columnas fecha y hora en un solo vector de tipo POSIXct para poder convertir la data a un objeto de serie de tiempo
data$DateTime <- as.POSIXct(paste(data$FECHA, data$Horas), format="%d/%m/%Y %H:%M:%S")

# Contaminante O3
daily_O3 <- data %>% 
  group_by(Date = as.Date(DateTime)) %>%
  summarize(O3 = mean(O3, na.rm = TRUE))
ts_O3 <- ts(daily_O3$O3, start = c(2020, 5), frequency = 365)
ts_O3_diff <- diff(ts_O3)
adf.test(ts_O3_diff)
model_O3 <- auto.arima(ts_O3_diff)
forecast_O3 <- forecast(model_O3, h = 30)
autoplot(forecast(forecast_O3, 3)) + labs(x = "Tiempo", y = "Diferencia en la concentración de O3")

# Contaminante NO2
daily_NO2 <- data %>% 
  group_by(Date = as.Date(DateTime)) %>%
  summarize(NO2 = mean(NO2, na.rm = TRUE))
ts_NO2 <- ts(daily_NO2$NO2, start = c(2020, 5), frequency = 365)
ts_NO2_diff <- diff(ts_NO2)
adf.test(ts_NO2_diff)
model_NO2 <- auto.arima(ts_NO2_diff)
forecast_NO2 <- forecast(model_NO2, h = 30)
autoplot(forecast(forecast_NO2, 3)) + labs(x = "Tiempo", y = "Diferencia en la concentración de NO2")

# Contaminante CO
daily_CO <- data %>% 
  group_by(Date = as.Date(DateTime)) %>%
  summarize(CO = mean(CO, na.rm = TRUE))
ts_CO <- ts(daily_CO$CO, start = c(2020, 5), frequency = 365)
ts_CO_diff <- diff(ts_CO)
adf.test(ts_CO_diff)
model_CO <- auto.arima(ts_CO_diff)
forecast_CO <- forecast(model_CO, h = 30)
autoplot(forecast(forecast_CO, 3)) + labs(x = "Tiempo", y = "Diferencia en la concentración de CO")

# Contaminante SO2
daily_SO2 <- data %>% 
  group_by(Date = as.Date(DateTime)) %>%
  summarize(SO2 = mean(SO2, na.rm = TRUE))
ts_SO2 <- ts(daily_SO2$SO2, start = c(2020, 5), frequency = 365)
ts_SO2_diff <- diff(ts_SO2)
adf.test(ts_SO2_diff)
model_SO2 <- auto.arima(ts_SO2_diff)
forecast_SO2 <- forecast(model_SO2, h = 30)
autoplot(forecast(forecast_SO2, 3)) + labs(x = "Tiempo", y = "Diferencia en la concentración de SO2")

# Contaminante PM10
daily_PM10 <- data %>% 
  group_by(Date = as.Date(DateTime)) %>%
  summarize(PM10 = mean(PM.10, na.rm = TRUE))
ts_PM10 <- ts(daily_PM10$PM10, start = c(2020, 5), frequency = 365)
ts_PM10_diff <- diff(ts_PM10)
adf.test(ts_PM10_diff)
model_PM10 <- auto.arima(ts_PM10_diff)
forecast_PM10 <- forecast(model_PM10, h = 30)
autoplot(forecast(forecast_PM10, 3)) + labs(x = "Tiempo", y = "Diferencia en la concentración de PM10")

# Contaminante PM2.5
daily_PM25 <- data %>% 
  group_by(Date = as.Date(DateTime)) %>%
  summarize(PM25 = mean(PM.2.5, na.rm = TRUE))
ts_PM25 <- ts(daily_PM25$PM25, start = c(2020, 5), frequency = 365)
ts_PM25_diff <- diff(ts_PM25)
adf.test(ts_PM25_diff)
model_PM25 <- auto.arima(ts_PM25_diff)
forecast_PM25 <- forecast(model_PM25, h = 30)
autoplot(forecast(forecast_PM25, 3)) + labs(x = "Tiempo", y = "Diferencia en la concentración de PM2.5")



# Graficar todas las series juntas usando facetado
df_facet <- data.frame(
  Date = rep(daily_O3$Date, 6),
  Concentration = c(daily_O3$O3, daily_NO2$NO2, daily_CO$CO, daily_SO2$SO2, daily_PM10$PM10, daily_PM25$PM25),
  Contaminante = rep(c("O3", "NO2", "CO", "SO2", "PM10", "PM2.5"), each = nrow(daily_O3))
)

# Verificar si hay valores NA en el dataframe df_facet
sum(is.na(df_facet))

# Eliminar filas con NA
df_facet <- df_facet %>% na.omit()

ggplot(df_facet, aes(x = Date, y = Concentration)) + 
  geom_line() + 
  facet_wrap(~ Contaminante, scales = "free_y") + 
  labs(x = "Fecha", y = "Concentración", title = "Series de tiempo de contaminantes") +
  theme_minimal()

# ==== mmm ya ====

library(forecast)
library(ggplot2)
library(dplyr)
library(tseries)

# Leer los datos
data <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\Dataset\\NINFAS\\NINFAS_2020-2024_combinado.csv")

# Combinar columnas fecha y hora en un solo vector de tipo POSIXct para poder convertir la data a un objeto de serie de tiempo
data$DateTime <- as.POSIXct(paste(data$FECHA, data$Horas), format="%d/%m/%Y %H:%M:%S")

# Contaminantes y sus predicciones
contaminantes <- c("O3", "NO2", "CO", "SO2", "PM.10", "PM.2.5")
forecast_dfs <- list()

for (contaminante in contaminantes) {
  # Preparar la data diaria
  daily_data <- data %>%
    group_by(Date = as.Date(DateTime)) %>%
    summarize(Concentration = mean(get(contaminante), na.rm = TRUE))
  
  # Eliminar valores NA en Date antes de calcular max(Date)
  daily_data <- daily_data %>% na.omit()
  
  # Crear serie de tiempo y aplicar diferencias
  ts_data <- ts(daily_data$Concentration, start = c(2020, 5), frequency = 365)
  ts_diff <- diff(ts_data)
  
  # Modelado y pronóstico
  model <- auto.arima(ts_diff)
  forecast_data <- forecast(model, h = 30)
  
  # Crear dataframe para cada contaminante con bandas de confianza
  max_date <- max(daily_data$Date, na.rm = TRUE)
  forecast_df <- data.frame(
    Date = c(daily_data$Date, seq(max(daily_data$Date) + 1, length.out = 30, by = "days")),
    Concentration = c(daily_data$Concentration, as.numeric(forecast_data$mean)),
    Lower = c(rep(NA, nrow(daily_data)), as.numeric(forecast_data$lower[, 2])),
    Upper = c(rep(NA, nrow(daily_data)), as.numeric(forecast_data$upper[, 2])),
    Contaminante = contaminante,
    Tipo = rep(c("Observado", "Predicción"), c(nrow(daily_data), 30))
  )
  
  forecast_dfs[[contaminante]] <- forecast_df
}

# Combinar todos los dataframes en uno solo
df_facet <- bind_rows(forecast_dfs)

# Eliminar filas con NA
df_facet <- df_facet %>% na.omit()

# Graficar con ggplot, incluyendo bandas de confianza y diferenciando observados de predicción
ggplot(df_facet, aes(x = Date, y = Concentration, color = Tipo)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  facet_wrap(~ Contaminante, scales = "free_y") + 
  labs(x = "Fecha", y = "Concentración", title = "Series de tiempo de contaminantes") +
  theme_minimal()
