#Series de tiempo | NINFAS 2020-2024

library(ggplot2)
library(lubridate)

library(forecast)

library(dplyr)
library(tidyr)

data<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\CL-NINFAS 2020-2024.csv")
head(data)

# Agregar ':00' a las cadenas de tiempo (para los segundos)
data$Horas <- paste0(data$Horas, ":00")

str(data)

#Convertir columnas fecha y hora a objeto tipo DATE
data$Fecha<-ymd(data$Fecha)
#data$Horas<-hms(data$Horas)
data$Horas <- hms::parse_hms(data$Horas)
data$timeObj <- as.POSIXct(data$Fecha) + as.numeric(data$Horas)

#Extraer la hora de timeObj
data$hour_of_day <- hour(data$timeObj)


# ===== Gráficas =====

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
#Contaminantes a lo largo del año

data_long <- data %>%
  pivot_longer(cols = c(O3, NO2, CO, SO2, PM.10, PM.2.5), names_to = "contaminant", values_to = "concentration")

ggplot(data_long, aes(x = timeObj, y = concentration, color = contaminant)) +
  geom_line() +
  labs(title = "Evolución de contaminantes a lo largo del tiempo",
       x = "Fecha y Hora",
       y = "Concentración (ug/m3)") +
  theme_minimal()


#Media de contaminante por hora | FACETADO

data$hour_of_day <- hour(data$timeObj)

contaminant_hourly <- data %>%
  pivot_longer(cols = c(O3, NO2, CO, SO2, PM.10, PM.2.5), names_to = "contaminant", values_to = "concentration") %>%
  group_by(hour_of_day, contaminant) %>%
  summarise(mean_concentration = mean(concentration, na.rm = TRUE))

ggplot(contaminant_hourly, aes(x = hour_of_day, y = mean_concentration)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 0:23) +  
  labs(title = "Concentración media de contaminantes por hora del día",
       x = "Hora del día",
       y = "Concentración media (ug/m3)") +
  theme_minimal() +
  facet_wrap(~ contaminant, scales = "free_y")
