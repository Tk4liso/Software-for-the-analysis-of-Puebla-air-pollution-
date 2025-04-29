#Artículo - Corrientes del viento
#22/04/2025
#Taisen Romero Bañuelos

# ==== Aplicar OCR para detectar la dirección y velocidad del viento ====
library(magick)
library(tesseract)

img <- image_read("C:/Users/Tacos/OneDrive/Documentos/Universidad/Servicio Social/Imágenes/Corrientes de aire/2023/1. Enero/01-01-2023.png")
print(img)

# Convertir a escala de grises
img_gray <- image_convert(img, colorspace = "gray")

# Ajustar contraste (aplicar más si es necesario)
img_contrast <- image_contrast(img_gray, sharpen = 1)
print(img_contrast)

# Recortar la zona del texto del viento | Para la esquina donde aparece "Wind: 265° @ 3 km/h"
img_crop <- image_crop(img_contrast, geometry_area(width = 300, height = 270, x = 20, y = 35))
print(img_crop)

# Aplicar OCR
texto <- ocr(img_crop)
cat("Texto detectado:\n", texto)

# ==== Automatización para los 365 días del año (2023) ====
library(magick)
library(tesseract)
library(dplyr)
library(stringr)

ruta_imagenes <- "C:/Users/Tacos/OneDrive/Documentos/Universidad/Servicio Social/Imágenes/Corrientes de aire/2023"
imagenes <- list.files(path = ruta_imagenes, pattern = "\\.png$", full.names = TRUE, recursive = TRUE)

resultados <- data.frame(
  archivo = character(),
  fecha = as.Date(character()),
  direccion = numeric(),
  velocidad = numeric(),
  stringsAsFactors = FALSE
)

extraer_direccion_y_velocidad <- function(archivo) {
  img <- image_read(archivo)
  img_gray <- image_convert(img, colorspace = "gray")
  img_contrast <- image_contrast(img_gray, sharpen = 1)
  
  #Recorte de imagen basado en el encuadre de las capturas
  img_crop <- image_crop(img_contrast, geometry_area(width = 300, height = 270, x = 20, y = 35))
  
  texto <- ocr(img_crop)
  
  # Extraer dirección y velocidad
  patron <- str_extract(texto, "\\d{2,3}°\\s*@\\s*\\d{1,2}")
  if (!is.na(patron)) {
    partes <- str_match(patron, "(\\d{2,3})°\\s*@\\s*(\\d{1,2})")
    direccion <- as.numeric(partes[2])
    velocidad <- as.numeric(partes[3])
    return(c(direccion, velocidad))
  } else {
    return(c(NA, NA))
  }
}

for (archivo in imagenes) {
  valores <- extraer_direccion_y_velocidad(archivo)
  nombre_archivo <- basename(archivo)
  fecha_extraida <- str_extract(nombre_archivo, "\\d{2}-\\d{2}-\\d{4}")
  fecha_formateada <- as.Date(fecha_extraida, format = "%d-%m-%Y")
  
  resultados <- resultados %>%
    add_row(archivo = archivo, fecha = fecha_formateada, direccion = valores[1], velocidad = valores[2])
}

#Guardar datos | No identificó bien algunos casos pero lo corregí manualmente, fueron pocas correcciones
write.csv(resultados, "viento_direcciones_2023.csv", row.names = FALSE)



# ==== Análisis de la dirección y velocidad del viento ====

#Objetivo: Comprobar si los vientos del año 2023 favorecen el transporte de contaminantes
#desde Izúcar de Matamoros hacia la ciudad de Puebla, específicamente a la estación de Agua Santa.

#Izúcar de Matamoros está al suroeste de la capital | Agua Santa (Puebla) está al noreste desde Izúcar.
#Por tanto, vientos favorables para arrastrar contaminantes serían los vientos 
#que soplan desde el suroeste hacia el noreste, es decir, con una dirección de origen entre:
#210° y 250° aproximadamente.
#Esto significa: el viento viene de entre sur-suroeste y oeste-suroeste (±20° alrededor de 230°).

library(dplyr)
library(ggplot2)

data <- read.csv("C:/Users/Tacos/OneDrive/Documentos/Universidad/Servicio Social/Imágenes/Corrientes de aire/2023/viento_direcciones_2023.csv")
head(data)
data<-data[-1]
head(data)
str(data)

#Media, promedio y cuartiles de todo el dataset
summary(data)

#Filtrar días con dirección favorable (210° a 250°)
datos_favorables <- data %>%
  filter(!is.na(direccion_grados)) %>%
  filter(direccion_grados >= 210 & direccion_grados <= 250)

# Cálculos
total_dias <- nrow(data)
dias_favorables <- nrow(datos_favorables)
porcentaje_favorables <- round((dias_favorables / total_dias) * 100, 1)

direccion_promedio <- mean(data$direccion_grados, na.rm = TRUE)
velocidad_promedio <- mean(data$velocidad_kmxh, na.rm = TRUE)

cat("Total de días:", total_dias, "\n")
cat("Días con viento favorable para la hipótesis:", dias_favorables, "\n")
cat("Porcentaje del año:", porcentaje_favorables, "%\n")
cat("Dirección promedio del viento:", round(direccion_promedio, 1), "°\n")
cat("Velocidad promedio del viento:", round(velocidad_promedio, 1), "km/h\n")

#Visualización 1: Histograma de direcciones
ggplot(data, aes(x = direccion_grados)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  geom_vline(xintercept = c(210, 250), color = "red", linetype = "dashed") +
  labs(title = "Distribución de direcciones del viento (2023)",
       x = "Dirección del viento (°)",
       y = "Frecuencia") +
  theme_minimal()

#Visualización 2: Dirección a lo largo del tiempo
library(scales)  # Para etiquetas de fechas
data_plot <- data #Variable temporal
data_plot$fecha <- as.Date(data_plot$fecha, format = "%d/%m/%Y")
#head(data_plot)

ggplot(data_plot, aes(x = fecha, y = direccion_grados)) +
  geom_line(color = "darkblue", size = 0.8) +
  geom_hline(yintercept = c(210, 250), linetype = "dashed", color = "red", size = 0.8) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b",
    limits = as.Date(c("2023-01-01", "2023-12-31")),
    expand = c(0, 0)) + #Mostrar meses por abreviaturas
  labs(
    title = "Dirección del viento a lo largo del año",
    x = "Mes",
    y = "Dirección (°)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# ----> Analizar las fechas que coinciden con la quema de caña de azúcar (Temporada crítica: diciembre a abril)
library(lubridate)

data$fecha <- as.Date(data$fecha)

#Extraer mes
data$mes <- month(data$fecha)
data$anio <- year(data$fecha)

#Filtrar temporada de quema: diciembre a abril
temporada_quema <- data %>%
  filter((mes %in% c(12, 1, 2, 3, 4)))

#De esa temporada, seleccionar los días con dirección favorable
quema_favorables <- temporada_quema %>%
  filter(direccion_grados >= 210 & direccion_grados <= 250)

#Cálculos específicos
total_quema <- nrow(temporada_quema)
favorables_quema <- nrow(quema_favorables)
porcentaje_quema <- round((favorables_quema / total_quema) * 100, 1)

cat("Total de días en temporada de quema:", total_quema, "\n")
cat("Días con viento favorable en temporada:", favorables_quema, "\n")
cat("Porcentaje favorable en temporada:", porcentaje_quema, "%\n")

#Durante la temporada de quema de caña de azúcar (dic-abr), el 45.7% de los días 
#presentaron vientos desde el suroeste (210°–250°), lo que favorece el arrastre 
#de contaminantes hacia la estación de Agua Santa.



# ====  Comparar días con viento favorable dentro vs fuera de la temporada de quema ====
library(dplyr)

data_plot$fecha <- as.Date(data_plot$fecha, format = "%d/%m/%Y")
data_plot$mes <- format(data_plot$fecha, "%m") %>% as.numeric()

#Clasificamos en temporada de quema
data_plot <- data_plot %>%
  mutate(
    temporada_quema = ifelse(mes %in% c(12, 1, 2, 3, 4), "Quema", "Fuera de quema"),
    favorable = direccion_grados >= 210 & direccion_grados <= 250)

#Conteo de días favorables dentro y fuera de la temporada
tabla_comparativa <- data_plot %>%
  mutate(temporada_legible = ifelse(
    temporada_quema == "Quema",
    "Temporada de quema (dic-abr)",
    "Resto del año (may-nov)")) %>%
  group_by(temporada_legible) %>%
  summarise(
    total_dias = n(),
    dias_favorables = sum(favorable, na.rm = TRUE),
    porcentaje_favorables = round(dias_favorables / total_dias * 100, 1))

print(tabla_comparativa)


#Gráfico de barras comparativo
ggplot(tabla_comparativa, aes(x = temporada_legible, y = porcentaje_favorables, fill = temporada_legible)) +
  geom_col(width = 0.6, color = "black") +
  geom_text(aes(label = paste0(porcentaje_favorables, "%")), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Temporada de quema (dic-abr)" = "#f4a261", "Resto del año (may-nov)" = "#a8dadc")) +
  labs(
    title = "Temporada de quema de caña vs el resto del año",
    x = "",
    y = "Días con viento desde Izúcar hacia Puebla (%)") +
  theme_minimal() +
  theme(legend.position = "none",text = element_text(size = 13))




#.