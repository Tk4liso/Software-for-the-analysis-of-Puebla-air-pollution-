# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(lubridate)
library(cluster)
library(factoextra)

# Leer los datos
data <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\CL-VELO 2023.csv")

# Convertir columnas de fecha y hora en formato datetime
data <- data %>%
  mutate(Datetime = as.POSIXct(paste(FECHA, Horas), format="%d/%m/%Y %H:%M"))

# Seleccionar solo las columnas numéricas para clustering
data_clustering <- data %>%
  select(O3, NO2, CO, SO2, PM.10, PM.2.5)

# Estandarizar los datos
data_scaled <- scale(data_clustering)

# Determinar el número óptimo de clusters usando el método del codo
fviz_nbclust(data_scaled, kmeans, method = "wss") +
  labs(title = "Número óptimo de clusters usando el método del codo")

# Aplicar k-means con el número óptimo de clusters (por ejemplo, 3)
set.seed(123)  # Para reproducibilidad
kmeans_result <- kmeans(data_scaled, centers = 3, nstart = 25)

# Añadir la asignación de clusters a los datos originales
data <- data %>%
  mutate(Cluster = as.factor(kmeans_result$cluster))



# Visualizar los clusters en 2D usando dos de las variables (por ejemplo, O3 y NO2)
ggplot(data, aes(x = O3, y = NO2, color = Cluster)) +
  geom_point() +
  labs(title = "Clustering de contaminantes del aire (O3 vs NO2)",
       x = "O3", y = "NO2") +
  theme_minimal()

# Visualizar los clusters en 2D usando un par de variables (por ejemplo, PM.10 y PM.2.5)
ggplot(data, aes(x = PM.10, y = PM.2.5, color = Cluster)) +
  geom_point() +
  labs(title = "Clustering de contaminantes del aire (PM.10 vs PM.2.5)",
       x = "PM.10", y = "PM.2.5") +
  theme_minimal()


