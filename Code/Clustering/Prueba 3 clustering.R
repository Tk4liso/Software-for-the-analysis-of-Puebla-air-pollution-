#Clustering

library(cluster)
library(factoextra)

data <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\CL-NINFAS 2023 (J).csv")
head(data)

# ===== Matrices =====

#Escalando los datos
data.scaled<-scale(data[, c("O3", "NO2", "CO", "SO2",  "PM.10", "PM.2.5")])
data.scaled<-as.data.frame(data.scaled)
data.scaled$Fecha<-data$FECHA
data.scaled$Hora<-data$Horas
head(data.scaled)

#Cálculo de matriz de distancia
data.eucl = dist( x = data.scaled, method = "euclidean")
round(as.matrix(data.eucl)[1:6, 1:6],1)

#Cálculo de distancia basada en correlación
data.cor<-get_dist(x = data.scaled[1:6], method = "pearson")
round(as.matrix(data.cor)[1:6, 1:6],1)

#Cálculo de distancias para datos mixtos
str(data)
data.mix<-data
data.mix$FECHA <- as.Date(data.mix$FECHA, format = "%d/%m/%Y")
str(data.mix)

#Convertir la fecha a número de días desde un origen (mínima fecha en el dataset)
data.mix$FECHA_num <- as.numeric(data.mix$FECHA - min(data.mix$FECHA))

#Convertir la hora a una variable numérica (fracción del día)
convert_to_fraction <- function(time_str) {
  time_parts <- as.numeric(unlist(strsplit(time_str, ":")))
  return(time_parts[1] / 24 + time_parts[2] / 1440 + time_parts[3] / 86400)
}

data.mix$Hora_num <- sapply(data.mix$Horas, convert_to_fraction)
str(data.mix)

data.mix <- data.mix[, c("FECHA_num", "Hora_num", "O3", "NO2", "CO", "SO2", "PM.10", "PM.2.5")]
data.daisy<-daisy(data.mix)
round(as.matrix(data.daisy)[1:6, 1:6],2)

data.daisy <- daisy(data.mix, metric = "gower")
round(as.matrix(data.daisy)[1:6, 1:6],2)


#Graficar matrices de distancia
fviz_dist(dist.obj = data.eucl, order = TRUE, show_labels = TRUE)


# ===== K-Means =====

library(mlr)
library(tidyverse)

library(ggplot2)
library(dplyr)

data.tib<-as_tibble(data)
data.tib_numeric<-data.tib %>%
  select(-FECHA, -Horas)

data.tib_scaled<-scale(data.tib_numeric)

#k-means
set.seed(123)
model<-kmeans(data.tib_scaled, centers = 3)
model

#Gráfica
library(GGally)

ggpairs(data.tib_scaled,
        upper = list(continuous="density"),
        lower = list(continuous=wrap("points", size=0.5)),
        diag=list(continuous="densityDiag"))+
  theme_bw()
