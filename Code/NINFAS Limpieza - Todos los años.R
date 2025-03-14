#Limpieza de datos uniendo todos los años | NINFAS 2020-2024

library(dplyr)

# ===== Unir los datos =====

data2020<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\DatosPueblaAIRE 2020-2022\\Datos_Ninfas_2020.csv")
data2021<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\DatosPueblaAIRE 2020-2022\\Datos_Ninfas_2021.csv")
data2022<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\DatosPueblaAIRE 2020-2022\\Datos_Ninfas_2022.csv")
data2023 <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\CL-NINFAS 2023 (J).csv")
data2024 <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\CL-NINFAS 2024.csv")

head(data2020)
head(data2021)
head(data2022)
head(data2023)
head(data2024)


# Eliminar columnas de categoría en data2024
data2024 <- data2024 %>% select(-starts_with("Categoria"))

# Renombrar columnas para asegurar consistencia
colnames(data2020) <- c("Fecha", "Horas", "O3", "O3.8.hrs", "NO2", "CO", "SO2", "PM.10", "PM.2.5")
colnames(data2021) <- c("Fecha", "Horas", "O3", "O3.8.hrs", "NO2", "CO", "SO2", "PM.10", "PM.2.5")
colnames(data2022) <- c("Fecha", "Horas", "O3", "O3.8.hrs", "NO2", "CO", "SO2", "PM.10", "PM.2.5")
colnames(data2023) <- c("Fecha", "Horas", "O3", "NO2", "CO", "SO2", "PM.10", "PM.2.5")
colnames(data2024) <- c("Fecha", "Horas", "O3", "NO2", "CO", "SO2", "PM.10", "PM.2.5")

# Eliminar la columna O3.8.hrs de data2020, data2021 y data2022
data2020 <- data2020 %>% select(-O3.8.hrs)
data2021 <- data2021 %>% select(-O3.8.hrs)
data2022 <- data2022 %>% select(-O3.8.hrs)

# Asegurarnos de que todas las columnas tengan los mismos tipos de datos
data2020$Fecha <- as.Date(data2020$Fecha, format="%Y-%m-%d")
data2021$Fecha <- as.Date(data2021$Fecha, format="%Y-%m-%d")
data2022$Fecha <- as.Date(data2022$Fecha, format="%Y-%m-%d")
data2023$Fecha <- as.Date(data2023$Fecha, format="%d/%m/%Y")
data2024$Fecha <- as.Date(data2024$Fecha, format="%d/%m/%Y")

# Convertir todas las columnas numéricas a tipo numérico
convert_to_numeric <- function(df) {
  df %>% mutate(
    O3 = as.numeric(O3),
    NO2 = as.numeric(NO2),
    CO = as.numeric(CO),
    SO2 = as.numeric(SO2),
    PM.10 = as.numeric(PM.10),
    PM.2.5 = as.numeric(PM.2.5)
  )
}

data2020 <- convert_to_numeric(data2020)
data2021 <- convert_to_numeric(data2021)
data2022 <- convert_to_numeric(data2022)
data2023 <- convert_to_numeric(data2023)
data2024 <- convert_to_numeric(data2024)

# Combinar todos los data frames
data_total <- bind_rows(data2020, data2021, data2022, data2023, data2024)

# Verificar el resultado
head(data_total)

# ===== Limpiar el conjunto total =====

#Contar NA's
colSums(is.na(data_total))

#Encontrar la fila con NA en fecha (en el data frame se puede observar que es un dato que solito se indexó, por lo que no pasa nada si lo borramos)
which(is.na(data_total$Fecha))
data_total <- data_total[!is.na(data_total$Fecha), ]
colSums(is.na(data_total))

#Reemplazar los NA por el promedio de cada columna
columns_to_convert <- c("O3", "NO2", "CO", "SO2", "PM.10", "PM.2.5")
for (col in columns_to_convert) {
  mean_value <- mean(data_total[[col]], na.rm = TRUE)
  data_total[[col]][is.na(data_total[[col]])] <- mean_value
}

#Sin decimales -> Entero | Con decimales -> se redondeen a tres decimales
data_total <- data_total %>%
  mutate(across(all_of(columns_to_convert), ~ ifelse(round(.) == ., as.integer(round(.)), round(., 3))))


# Verificar los resultados
colSums(is.na(data_total))
head(data_total)

#Guardar el nuevo data frame
write.csv(data_total, "C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\CL-NINFAS 2020-2024.csv", row.names = FALSE)
