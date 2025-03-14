#Series de tiempo 1

data<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\NINFAS-2023-limpio.csv")
head(data)
summary(data)

data$DateTime <- as.POSIXct(paste(data$FECHA, data$Horas), format="%d/%m/%Y %H:%M:%S")
str(data)

library(xts)

# Crear un objeto xts para cada contaminante
o3_xts <- xts(data$O3, order.by = data$DateTime)
no2_xts <- xts(data$NO2, order.by = data$DateTime)
co_xts <- xts(data$CO, order.by = data$DateTime)
so2_xts <- xts(data$SO2, order.by = data$DateTime)
pm10_xts <- xts(data$PM.10, order.by = data$DateTime)
pm2.5_xts <- xts(data$PM.2.5, order.by = data$DateTime)

# Mostrar los primeros datos del objeto xts de O3 para verificar
head(o3_xts)

# ==== Gráficas ====

library(ggplot2)
library(scales)

# Grafica para O3

# Convertir el objeto xts a un dataframe para ggplot
o3_df <- data.frame(DateTime = index(o3_xts), O3 = coredata(o3_xts))
head(o3_df)

ggplot(o3_df, aes(x = DateTime, y = O3)) +
  geom_line() +
  labs(title = "Concentración de O3 en la Estación Ninfas (2023)",
       x = "Fecha y hora",
       y = "Concentración de O3 (ppm)") +
  theme_minimal() +
  scale_x_datetime(labels = date_format("%d-%m-%Y %H:%M"), date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Grafica para NO2

no2_df <- data.frame(DateTime = index(no2_xts), NO2 = coredata(no2_xts))
ggplot(no2_df, aes(x = DateTime, y = NO2)) +
  geom_line() +
  labs(title = "Concentración de NO2 en la Estación Ninfas (2023)",
       x = "Fecha y hora",
       y = "Concentración de NO2 (ppb)") +
  theme_minimal() +
  scale_x_datetime(labels = date_format("%d-%m-%Y %H:%M"), date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
