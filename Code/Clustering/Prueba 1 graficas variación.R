#Clustering de NINFAS 2023

library(dplyr)
library(ggplot2)
library(lubridate)

data<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\CL-VELO 2023.csv")
head(data)
summary(data)

data <- data %>%
  mutate(Datetime = as.POSIXct(paste(FECHA, Horas), format="%d/%m/%Y %H:%M"))

# Extraer componentes temporales
data <- data %>%
  mutate(Hour = hour(Datetime),
         Day = wday(Datetime, label = TRUE, abbr = FALSE),
         Month = month(Datetime, label = TRUE, abbr = FALSE))


# Crear gráficos para analizar patrones temporales

# Variación horaria
ggplot(data, aes(x = Hour)) +
  geom_line(aes(y = O3, color = "O3"), size = 1) +
  geom_line(aes(y = NO2, color = "NO2"), size = 1, linetype = "dotted") +
  geom_line(aes(y = CO, color = "CO"), size = 1) +
  geom_line(aes(y = SO2, color = "SO2"), size = 1, linetype = "dotted") +
  geom_line(aes(y = PM.10, color = "PM10"), size = 1, linetype = "dotted") +
  geom_line(aes(y = PM.2.5, color = "PM2.5"), size = 1) +
  labs(title = "Variación horaria de los contaminantes",
       x = "Hora del día", y = "Concentración (ug/m3)") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")

# Variación diaria
ggplot(data, aes(x = Day)) +
  geom_line(aes(y = O3, color = "O3"), size = 2) +
  geom_line(aes(y = NO2, color = "NO2"), size = 2) +
  geom_line(aes(y = CO, color = "CO"), size = 2) +
  geom_line(aes(y = SO2, color = "SO2"), size = 2) +
  geom_line(aes(y = PM.10, color = "PM10"), size = 2) +
  geom_line(aes(y = PM.2.5, color = "PM2.5"), size = 2) +
  labs(title = "Variación diaria de los contaminantes",
       x = "Día de la semana", y = "Concentración (ug/m3)") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")

# Variación mensual
ggplot(data, aes(x = Month)) +
  geom_line(aes(y = O3, color = "O3"), size = 2) +
  geom_line(aes(y = NO2, color = "NO2"), size = 2) +
  geom_line(aes(y = CO, color = "CO"), size = 2) +
  geom_line(aes(y = SO2, color = "SO2"), size = 2) +
  geom_line(aes(y = PM.10, color = "PM10"), size = 2) +
  geom_line(aes(y = PM.2.5, color = "PM2.5"), size = 2) +
  labs(title = "Variación mensual de los contaminantes",
       x = "Mes", y = "Concentración (ug/m3)") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")

# Agrupación y resumen de datos por componentes temporales
summary_by_hour <- data %>%
  group_by(Hour) %>%
  summarise(across(c(O3, NO2, CO, SO2, PM.10, PM.2.5), mean))

summary_by_day <- data %>%
  group_by(Day) %>%
  summarise(across(c(O3, NO2, CO, SO2, PM.10, PM.2.5), mean))

summary_by_month <- data %>%
  group_by(Month) %>%
  summarise(across(c(O3, NO2, CO, SO2, PM.10, PM.2.5), mean))

# Mostrar resúmenes
print(summary_by_hour)
print(summary_by_day)
print(summary_by_month)