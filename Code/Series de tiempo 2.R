#Series de tiempo v2 | NINFAS 2023

library(ggplot2)
library(lubridate)

library(forecast)

library(dplyr)
library(tidyr)

data <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\CL-NINFAS 2023 (J).csv")
head(data)
str(data)

# Convertir columnas fecha y hora
data$FECHA <- as.Date(data$FECHA, format="%d/%m/%Y")
data$Horas <- hms::as_hms(data$Horas)
data$timeObj <- as.POSIXct(data$FECHA) + as.numeric(data$Horas)
head(data)

#Extraer la hora de timeObj
data$hour_of_day <- hour(data$timeObj)

# ===== O3 ((Ozono)) =====

# Gráfico de O3 a lo largo del tiempo
ggplot(data, aes(x = timeObj, y = O3)) +
  geom_line(color = "orange") +
  labs(title = "Evolución de O3 a lo largo del tiempo",
       x = "Fecha y Hora",
       y = "Concentración de O3 (ug/m3)")

# Calcular la media de O3 por hora del día
o3_hourly <- data %>%
  group_by(hour_of_day) %>%
  summarise(mean_o3 = mean(O3, na.rm = TRUE))

# Graficar la media de O3 por hora del día
ggplot(o3_hourly, aes(x = hour_of_day, y = mean_o3)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 0:23) +  # Asegura que se muestren todas las horas
  labs(title = "Concentración media de O3 por hora del día",
       x = "Hora del día",
       y = "Concentración media de O3") +
  theme_minimal()


# ===== NO2 (Dióxido de Nitrógeno) =====

# Gráfico de NO2 a lo largo del tiempo
ggplot(data, aes(x = timeObj, y = NO2)) +
  geom_line(color = "red") +
  labs(title = "Evolución de NO2 a lo largo del tiempo",
       x = "Fecha y Hora",
       y = "Concentración de NO2 (ppb)")

# Calcular la media de NO2 por hora del día
no2_hourly <- data %>%
  group_by(hour_of_day) %>%
  summarise(mean_no2 = mean(NO2, na.rm = TRUE))

# Graficar la media de NO2 por hora del día
ggplot(no2_hourly, aes(x = hour_of_day, y = mean_no2)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 0:23) +  # Asegura que se muestren todas las horas
  labs(title = "Concentración media de NO2 por hora del día",
       x = "Hora del día",
       y = "Concentración media de NO2") +
  theme_minimal()


# ===== CO (Monóxido de Carbono) =====

# Gráfico de CO a lo largo del tiempo
ggplot(data, aes(x = timeObj, y = CO)) +
  geom_line(color = "purple") +
  labs(title = "Evolución de CO a lo largo del tiempo",
       x = "Fecha y Hora",
       y = "Concentración de CO (ug/m3)")

# Calcular la media de CO por hora del día
co_hourly <- data %>%
  group_by(hour_of_day) %>%
  summarise(mean_co = mean(CO, na.rm = TRUE))

# Graficar la media de CO por hora del día
ggplot(co_hourly, aes(x = hour_of_day, y = mean_co)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Concentración media de CO por hora del día",
       x = "Hora del día",
       y = "Concentración media de CO") +
  theme_minimal()


# ===== SO2 (Dióxido de Azufre =====

# Gráfico de SO2 a lo largo del tiempo
ggplot(data, aes(x = timeObj, y = SO2)) +
  geom_line(color = "brown") +
  labs(title = "Evolución de SO2 a lo largo del tiempo",
       x = "Fecha y Hora",
       y = "Concentración de SO2 (ug/m3)")

# Calcular la media de SO2 por hora del día
so2_hourly <- data %>%
  group_by(hour_of_day) %>%
  summarise(mean_so2 = mean(SO2, na.rm = TRUE))

# Graficar la media de SO2 por hora del día
ggplot(so2_hourly, aes(x = hour_of_day, y = mean_so2)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Concentración media de SO2 por hora del día",
       x = "Hora del día",
       y = "Concentración media de SO2") +
  theme_minimal()


# ===== PM.10 =====

# Gráfico de PM10 a lo largo del tiempo
ggplot(data, aes(x = timeObj, y = PM.10)) +
  geom_line(color = "green") +
  labs(title = "Evolución de PM10 a lo largo del tiempo",
       x = "Fecha y Hora",
       y = "Concentración de PM10 (ug/m3)")

# Calcular la media de PM10 por hora del día
pm10_hourly <- data %>%
  group_by(hour_of_day) %>%
  summarise(mean_pm10 = mean(PM.10, na.rm = TRUE))

# Graficar la media de PM10 por hora del día
ggplot(pm10_hourly, aes(x = hour_of_day, y = mean_pm10)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 0:23) +  # Asegura que se muestren todas las horas
  labs(title = "Concentración media de PM10 por hora del día",
       x = "Hora del día",
       y = "Concentración media de PM10") +
  theme_minimal()


# ===== PM.2.5 =====

#Gráfico de PM2.5 a lo largo del tiempo
ggplot(data, aes(x = timeObj, y = PM.2.5)) +
  geom_line(color = "blue") +
  labs(title = "Evolución de PM2.5 a lo largo del tiempo",
       x = "Fecha y Hora",
       y = "Concentración de PM2.5 (ug/m3)")


#Calcular la media de PM2.5 por hora del día
pm25_hourly <- data %>%
  group_by(hour_of_day) %>%
  summarise(mean_pm25 = mean(PM.2.5, na.rm = TRUE))

# Graficar la media de PM2.5 por hora del día
ggplot(pm25_hourly, aes(x = hour_of_day, y = mean_pm25)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 0:23) +  # Asegura que se muestren todas las horas
  labs(title = "Concentración media de PM2.5 por hora del día",
       x = "Hora del día",
       y = "Concentración media de PM2.5") +
  theme_minimal()



# ===== Todos a la vez =====

#Grafico 1: Mostrar todos los contaminantes a lo largo del año

data_long <- data %>%
  pivot_longer(cols = c(O3, NO2, CO, SO2, PM.10, PM.2.5), names_to = "contaminant", values_to = "concentration")

ggplot(data_long, aes(x = timeObj, y = concentration, color = contaminant)) +
  geom_line() +
  labs(title = "Evolución de contaminantes a lo largo del tiempo",
       x = "Fecha y Hora",
       y = "Concentración (ug/m3)") +
  theme_minimal()


#Grafico 2: Calcular la media de cada contaminante por hora del día

data$hour_of_day <- hour(data$timeObj)

contaminant_hourly <- data %>%
  pivot_longer(cols = c(O3, NO2, CO, SO2, PM.10, PM.2.5), names_to = "contaminant", values_to = "concentration") %>%
  group_by(hour_of_day, contaminant) %>%
  summarise(mean_concentration = mean(concentration, na.rm = TRUE))

ggplot(contaminant_hourly, aes(x = hour_of_day, y = mean_concentration, color = contaminant)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 0:23) +  
  labs(title = "Concentración media de contaminantes por hora del día",
       x = "Hora del día",
       y = "Concentración media (ug/m3)") +
  theme_minimal()

#Media de contaminante por hora | FACETADO
ggplot(contaminant_hourly, aes(x = hour_of_day, y = mean_concentration)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 0:23) +  
  labs(title = "Concentración media de contaminantes por hora del día",
       x = "Hora del día",
       y = "Concentración media (ug/m3)") +
  theme_minimal() +
  facet_wrap(~ contaminant, scales = "free_y")

# ===== ARIMA =====

# Crear un objetos de serie temporal para una variable (sólo ejecutar el que se quiera calcular)
ts_data <- ts(data$O3, start=c(2023,1), frequency=24)     # frecuencia 24 para datos horarios
ts_data <- ts(data$NO2, start=c(2023,1), frequency=24)
ts_data <- ts(data$CO, start=c(2023,1), frequency=24)     # ---
ts_data <- ts(data$SO2, start=c(2023,1), frequency=24)
ts_data <- ts(data$PM.10, start=c(2023,1), frequency=24)
ts_data <- ts(data$PM.2.5, start=c(2023,1), frequency=24)

# Encontrar el mejor modelo ARIMA
fit <- auto.arima(ts_data)
fit

# Realizar un pronóstico a futuro (por ejemplo, para las próximas 48 horas)
forecasted_values <- forecast(fit, h=48)
accuracy(fit)

# Graficas (sólo ejecutar el respectivo al objeto de serie temporal)

#O3
autoplot(forecasted_values) + 
  labs(x="Fecha y Hora", y="O3") + 
  ggtitle("Pronóstico de O3 para las próximas 48 horas")

#NO2
autoplot(forecasted_values) + 
  labs(x="Fecha y Hora", y="NO2") + 
  ggtitle("Pronóstico de NO2 para las próximas 48 horas")

#CO
autoplot(forecasted_values) + 
  labs(x="Fecha y Hora", y="CO") + 
  ggtitle("Pronóstico de CO para las próximas 48 horas")

#SO2
autoplot(forecasted_values) + 
  labs(x="Fecha y Hora", y="SO2") + 
  ggtitle("Pronóstico de SO2 para las próximas 48 horas")

#PM.10
autoplot(forecasted_values) + 
  labs(x="Fecha y Hora", y="PM.10") + 
  ggtitle("Pronóstico de PM.10 para las próximas 48 horas")

#PM.2.5
autoplot(forecasted_values) + 
  labs(x="Fecha y Hora", y="PM.2.5") + 
  ggtitle("Pronóstico de PM2.5 para las próximas 48 horas")



# Verificar los residuos del modelo
checkresiduals(fit)
