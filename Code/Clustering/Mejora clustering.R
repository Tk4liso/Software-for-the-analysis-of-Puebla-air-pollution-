#Actualización y mejora del clustering

# ==== Clustering K-Means sobre promedios mensuales ====
library(tidyverse)
library(lubridate)
library(cluster)
library(factoextra)
library(ggfortify)
library(scales)

rutas <- list(
  AGUASANTA = "C:/Users/Tacos/OneDrive/Documentos/Universidad/Servicio Social/Datos/Dataset/AGUASANTA/AGUASANTA_2020-2024_combinado.csv",
  BINE = "C:/Users/Tacos/OneDrive/Documentos/Universidad/Servicio Social/Datos/Dataset/BINE/BINE_2021-2024_combinado.csv",
  NINFAS = "C:/Users/Tacos/OneDrive/Documentos/Universidad/Servicio Social/Datos/Dataset/NINFAS/NINFAS_2020-2024_combinado.csv",
  UTP = "C:/Users/Tacos/OneDrive/Documentos/Universidad/Servicio Social/Datos/Dataset/UTP/UTP_2020-2024_combinado.csv")

# FUNCIONES DE APOYO
leer_y_preparar <- function(ruta, estacion) {
  read_csv(ruta, show_col_types = FALSE) %>%
    rename(
      PM10 = `PM-10`,
      `PM2.5` = `PM-2.5`
    ) %>%
    mutate(
      FECHA = dmy(FECHA),
      Mes = floor_date(FECHA, "month"),
      Estacion = estacion
    ) %>%
    group_by(Estacion, Mes) %>%
    summarise(across(c(O3, NO2, CO, SO2, PM10, `PM2.5`), mean, na.rm = TRUE), .groups = "drop")
}


# CARGAR Y UNIR DATOS
datos_estaciones <- map2_dfr(rutas, names(rutas), leer_y_preparar)


# AGREGAR VARIABLES TEMPORALES NUMÉRICAS
datos_clustering <- datos_estaciones %>%
  mutate(
    Mes_num = as.numeric(Mes - min(Mes)))

# ESCALAR VARIABLES PARA CLUSTERING
datos_scaled <- datos_clustering %>%
  select(O3, NO2, CO, SO2, PM10, `PM2.5`, Mes_num) %>%
  scale()


# - Aplicar clustering -
# DETERMINAR NÚMERO ÓPTIMO DE CLÚSTERS
fviz_nbclust(datos_scaled, kmeans, method = "wss") +
  labs(title = "Elbow method to find optimal k")

fviz_nbclust(datos_scaled, kmeans, method = "silhouette") +
  labs(title = "Silhouette method to find optimal k")

# Elegimos k = 2 como el k más consistente según los dos métodos para descubrir el k óptimo
set.seed(123)
modelo_kmeans <- kmeans(datos_scaled, centers = 2, nstart = 25)

# AÑADIR RESULTADOS A LOS DATOS ORIGINALES
datos_clustering <- datos_clustering %>%
  mutate(Cluster = as.factor(modelo_kmeans$cluster))



# - VISUALIZACIÓN CON PCA -
datos_viz <- datos_clustering %>%
  rename(Station = Estacion)

# Visualizar usando 'Station' como forma (shape)
autoplot(prcomp(datos_scaled), data = datos_viz, colour = 'Cluster', shape = 'Station') +
  labs(title = "PCA of pollutant clusters by station and month") +
  theme_minimal()

# VISUALIZACIÓN TEMPORAL
ggplot(datos_viz, aes(x = Mes, y = Cluster, color = Station)) +
  geom_point(size = 2) +
  facet_wrap(~ Station) +
  labs(title = "Temporal evolution of clusters per station") +
  theme_minimal()


#Analizar las medias de los contaminantes dentro de cada clúster
datos_clustering %>%
  group_by(Cluster) %>%
  summarise(across(c(O3, NO2, CO, SO2, PM10, `PM2.5`), mean, na.rm = TRUE))

#Cluster 1: baja contaminación | CLuster 2: alta contaminación


# ==== EDA ====
#DISTRIBUCIONES PROMEDIO POR ESTACIÓN 
library(ggplot2)
library(dplyr)

# Comparar promedios generales por estación para cada contaminante
datos_clustering %>%
  pivot_longer(cols = c(O3, NO2, CO, SO2, PM10, `PM2.5`), names_to = "Contaminante", values_to = "Valor") %>%
  ggplot(aes(x = Estacion, y = Valor, fill = Estacion)) +
  geom_boxplot() +
  facet_wrap(~ Contaminante, scales = "free_y") +
  labs(title = "Monthly distribution of pollutants by station (2020-2024)",
       x = "Station", y = "Concentration") +
  theme_minimal() +
  theme(legend.position = "none")


# ==== Extra ====
#Gráfico de radar
library(fmsb)

#Datos promedio por clúster 
clust_data <- data.frame(
  row.names = c("Clúster 1", "Clúster 2"),
  O3 = c(0.0248, 18.7),
  NO2 = c(0.0140, 8.74),
  CO = c(0.929, 7.06),
  SO2 = c(0.196, 3.75),
  PM10 = c(40.7, 65.2),
  PM2.5 = c(16.9, 69.3)
)

#Normalizar por columna (cada contaminante)
clust_norm <- as.data.frame(lapply(clust_data, function(x) x / max(x)))

#Agregar filas de máximo y mínimo (requerido por fmsb) 
clust_norm <- rbind(rep(1, 6), rep(0, 6), clust_norm)
rownames(clust_norm) <- c("max", "min", "Clúster 1", "Clúster 2")

#Graficar radar
radarchart(clust_norm,
           axistype = 1,
           pcol = c("darkorange", "deepskyblue"),
           pfcol = c(rgb(1, 0.6, 0, 0.4), rgb(0, 0.6, 1, 0.4)),
           plwd = 2,
           plty = 1,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "black",
           caxislabels = c("0", "0.25", "0.5", "0.75", "1"),
           vlcex = 1.2,
           title = "Perfil relativo de contaminantes por clúster")

legend("bottom", 
       legend = c("Clúster 1 (baja contaminación)", "Clúster 2 (alta contaminación)"),
       col = c("darkorange", "deepskyblue"), 
       lty = 1, 
       lwd = 2, 
       cex = 1,
       bty = "n")








#.