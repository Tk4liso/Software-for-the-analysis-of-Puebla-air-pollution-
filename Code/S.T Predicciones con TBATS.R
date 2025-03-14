#Series de tiempo - PREDICCIÓN CON TRANSFORMADAS DE FOURIER (TBATS) |NINFAS - 2023|

# ==== Pruebas ====
library(forecast)
library(ggplot2)
library(tseries)

data<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\Dataset\\NINFAS\\NINFAS_2020-2024_combinado.csv")
head(data)
str(data)
summary(data)

#Combinar columnas fecha y hora en un solo vector de tipo POSIXct para poder convertir la data a un objeto de serie de tiempo
data$DateTime <- as.POSIXct(paste(data$FECHA, data$Horas), format="%d/%m/%Y %H:%M:%S")
data <- na.omit(data)
head(data)
any(is.na(data))


#Pronóstico con TBATS

library(dplyr)
library(lubridate)
library(forecast)

# Agrupar por día y calcular la media de cada contaminante
data_daily <- data %>%
  group_by(Date = as.Date(DateTime)) %>%
  summarise(
    O3_daily_mean = mean(O3, na.rm = TRUE),
    NO2_daily_mean = mean(NO2, na.rm = TRUE),
    CO_daily_mean = mean(CO, na.rm = TRUE),
    SO2_daily_mean = mean(SO2, na.rm = TRUE),
    PM10_daily_mean = mean(PM.10, na.rm = TRUE),
    PM2.5_daily_mean = mean(PM.2.5, na.rm = TRUE)
  )

# Convertir cada contaminante a una serie de tiempo
o3_ts <- ts(data_daily$O3_daily_mean, frequency = 365, start = c(year(min(data_daily$Date)), yday(min(data_daily$Date))))
tbats_model <- tbats(o3_ts)

# Realizar el pronóstico para los siguientes 30 días
forecast_tbats <- forecast(tbats_model, h = 30)

plot(forecast_tbats, main = "Pronóstico Diario de O3 usando TBATS")


PM10_ts <- ts(data_daily$PM10_daily_mean, frequency = 365, start = c(year(min(data_daily$Date)), yday(min(data_daily$Date))))
tbats_model <- tbats(PM10_ts)
forecast_tbats <- forecast(tbats_model, h = 30)
plot(forecast_tbats, main = "Pronóstico Diario de PM.10 usando TBATS")

accuracy(forecast_tbats)
checkresiduals(forecast_tbats)



# ----> Verificación de estacionalidad y pronóstico <----

# Descomposición de la serie de tiempo para PM10
pm10_decomp <- stl(PM10_ts, s.window = "periodic")
plot(pm10_decomp, main = "Descomposición de la serie de tiempo de PM.10")
adf.test(PM10_ts)

#Aplicar la Diferenciación para intentar hacerla estacionaria
original_ts <- data_daily #Guardamos la ts original para que después nos ayude a revertit la diferenciación
diff_PM10<-diff(data_daily$PM10_daily_mean)
adf.test(diff_PM10)


PM10_ts <- ts(diff_PM10, frequency = 365, start = c(year(min(data_daily$Date)), yday(min(data_daily$Date))))
tbats_model <- tbats(PM10_ts)
forecast_tbats <- forecast(tbats_model, h = 30)
plot(forecast_tbats, main = "Pronóstico Diario de PM.10 usando TBATS")

accuracy(forecast_tbats)
checkresiduals(forecast_tbats)





#Descomposición de la serie de tiempo para O3
o3_decomp <- stl(o3_ts, s.window = "periodic")
plot(o3_decomp, main = "Descomposición de la serie de tiempo de O3")
adf.test(o3_ts)

original_ts <- data_daily
diff_o3<-diff(data_daily$O3_daily_mean)
adf.test(diff_o3)

o3_ts <- ts(diff_o3, frequency = 365, start = c(year(min(data_daily$Date)), yday(min(data_daily$Date))))
tbats_model <- tbats(o3_ts)
forecast_tbats <- forecast(tbats_model, h = 30)
plot(forecast_tbats, main = "Pronóstico Diario de O3 usando TBATS")

accuracy(forecast_tbats)
checkresiduals(forecast_tbats)

#De cara a los pronósticos de series de tiempo las métricas RMSE y MAE son las más relevantes.

# ==== TLAXCALA ====

# Dividir los datos por años
data$Year <- year(data$DateTime)

# Lista para almacenar los modelos TBATS por año
tbats_models <- list()

# Aplicar TBATS por año
for (yr in unique(data$Year)) {
  # Filtrar datos por año
  data_year <- data %>% filter(Year == yr)
  
  # Agrupar por día para reducir las observaciones (puedes mantener las observaciones horarias si lo prefieres)
  data_daily <- data_year %>%
    group_by(Date = as.Date(DateTime)) %>%
    summarise(O3_daily_mean = mean(O3))
  
  # Convertir a serie de tiempo
  o3_ts <- ts(data_daily$O3_daily_mean, frequency = 365, start = c(year(min(data_daily$Date)), yday(min(data_daily$Date))))
  
  # Ajustar el modelo TBATS
  tbats_models[[as.character(yr)]] <- tbats(o3_ts)
  
  # Puedes generar pronósticos y visualizarlos para cada año
  forecast_tbats <- forecast(tbats_models[[as.character(yr)]], h = 30)
  plot(forecast_tbats, main = paste("Pronóstico de O3 para el año", yr, "usando TBATS"))
}

accuracy(forecast_tbats)
checkresiduals(forecast_tbats)



# ==== Conclusión =====

library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)
library(tseries)

#Limpieza y tratamiento de datos
data<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\Dataset\\NINFAS\\NINFAS_2020-2024_combinado.csv")
head(data)
str(data)
summary(data)

data$DateTime <- as.POSIXct(paste(data$FECHA, data$Horas), format="%d/%m/%Y %H:%M:%S")
data <- na.omit(data)
head(data)
any(is.na(data))

# Agrupar por día y calcular la media de cada contaminante
data_daily <- data %>%
  group_by(Date = as.Date(DateTime)) %>%
  summarise(
    O3_daily_mean = mean(O3, na.rm = TRUE),
    NO2_daily_mean = mean(NO2, na.rm = TRUE),
    CO_daily_mean = mean(CO, na.rm = TRUE),
    SO2_daily_mean = mean(SO2, na.rm = TRUE),
    PM10_daily_mean = mean(PM.10, na.rm = TRUE),
    PM2.5_daily_mean = mean(PM.2.5, na.rm = TRUE)
  )

#Iniciando proceso de series de tiempo

#O3
diff_o3<-diff(data_daily$O3_daily_mean)
o3_ts <- ts(diff_o3, frequency = 365, start = c(year(min(data_daily$Date)), yday(min(data_daily$Date))))
tbats_model <- tbats(o3_ts)
forecast_tbats <- forecast(tbats_model, h = 30)
plot(forecast_tbats, main = "Pronóstico Diario de O3 usando TBATS")

#PM.10
diff_PM10<-diff(data_daily$PM10_daily_mean)
PM10_ts <- ts(diff_PM10, frequency = 365, start = c(year(min(data_daily$Date)), yday(min(data_daily$Date))))
tbats_model <- tbats(PM10_ts)
forecast_tbats <- forecast(tbats_model, h = 30)
plot(forecast_tbats, main = "Pronóstico Diario de PM.10 usando TBATS")

# ==== Por año ====
library(dplyr)
library(ggplot2)
library(tidyr)
library(forecast)
library(lubridate)
library(readr)

data <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\Dataset\\NINFAS\\NINFAS_2020-2024_combinado.csv")
data$DateTime <- as.POSIXct(paste(data$FECHA, data$Horas), format="%d/%m/%Y %H:%M:%S")
data <- na.omit(data)

# Agrupar por día y calcular la media de cada contaminante
data_daily <- data %>%
  group_by(Date = as.Date(DateTime)) %>%
  summarise(
    O3_daily_mean = mean(O3, na.rm = TRUE),
    NO2_daily_mean = mean(NO2, na.rm = TRUE),
    CO_daily_mean = mean(CO, na.rm = TRUE),
    SO2_daily_mean = mean(SO2, na.rm = TRUE),
    PM10_daily_mean = mean(PM.10, na.rm = TRUE),
    PM2.5_daily_mean = mean(PM.2.5, na.rm = TRUE)
  )

#Ajustar modelos TBATS y generar pronósticos para cada contaminante
tbats_forecasts <- list()

for (contaminante in c("O3_daily_mean", "NO2_daily_mean", "CO_daily_mean", "SO2_daily_mean", "PM10_daily_mean", "PM2.5_daily_mean")) {
  #Convertir a TS (asumiendo frecuencia diaria)
  ts_data <- ts(data_daily[[contaminante]], frequency = 365)
  
  fit_tbats <- tbats(ts_data)
  
  #Generar pronóstico para los próximos 30 días
  forecast_tbats <- forecast(fit_tbats, h = 30)
  
  tbats_forecasts[[contaminante]] <- forecast_tbats
}

#Convertir el dataset a formato largo para graficar con ggplot
data_long <- data_daily %>%
  pivot_longer(cols = c("O3_daily_mean", "NO2_daily_mean", "CO_daily_mean", "SO2_daily_mean", "PM10_daily_mean", "PM2.5_daily_mean"),
               names_to = "Contaminante",
               values_to = "Concentracion") %>%
  mutate(Tipo = "Histórico")

#Agregar los pronósticos al dataset para graficar
data_with_forecast <- data_long

for (contaminante in names(tbats_forecasts)) {
  forecast_dates <- seq(max(data_daily$Date) + 1, by = "day", length.out = 30)
  
  forecast_data <- data.frame(
    Date = forecast_dates,
    Concentracion = tbats_forecasts[[contaminante]]$mean,
    Contaminante = contaminante,
    Tipo = "Pronóstico"  #Campo para distinguir los pronósticos
  )
  
  # Añadir las predicciones al dataset original
  data_with_forecast <- rbind(data_with_forecast, forecast_data)
}

data_with_forecast$Tipo <- ifelse(is.na(data_with_forecast$Tipo), "Histórico", data_with_forecast$Tipo)
ggplot(data_with_forecast, aes(x = Date, y = Concentracion, color = Contaminante)) +
  geom_line(aes(linetype = Tipo, color = ifelse(Tipo == "Pronóstico", "Pronóstico", Contaminante)), linewidth = 1)+  
  scale_color_manual(values = c("Pronóstico" = "red",  # Color rojo para los pronósticos
                                "CO_daily_mean" = "salmon", 
                                "NO2_daily_mean" = "goldenrod",
                                "O3_daily_mean" = "green",
                                "PM10_daily_mean" = "cyan",
                                "PM2.5_daily_mean" = "lightblue",
                                "SO2_daily_mean" = "magenta")) +
  scale_linetype_manual(values = c("Histórico" = "solid", "Pronóstico" = "solid")) +  # Líneas punteadas para pronósticos
  facet_wrap(~Contaminante, scales = "free_y") +
  labs(title = "Concentración Diaria Promedio de Contaminantes con Pronóstico TBATS",
       x = "Fecha", y = "Concentración") +
  theme_minimal()


#Graficas por separado
plots <- list()
contaminantes <- c("CO_daily_mean", "NO2_daily_mean", "O3_daily_mean", 
                   "PM10_daily_mean", "PM2.5_daily_mean", "SO2_daily_mean")

for (contaminante in contaminantes) {
  plot <- ggplot(subset(data_with_forecast, Contaminante == contaminante), 
                 aes(x = Date, y = Concentracion, color = Contaminante)) +
    geom_line(aes(linetype = Tipo, color = ifelse(Tipo == "Pronóstico", "Pronóstico", Contaminante)), size = 1) +
    scale_color_manual(values = c("Pronóstico" = "red",  # Color rojo para pronósticos
                                  contaminante = scales::hue_pal()(1))) +  # Mantener el color único para cada contaminante
    scale_linetype_manual(values = c("Histórico" = "solid", "Pronóstico" = "solid")) +  
    labs(title = paste("Concentración Diaria Promedio de", contaminante),
         x = "Fecha", y = "Concentración") +
    theme_minimal()
  
  # Almacenar la gráfica en la lista
  plots[[contaminante]] <- plot
}

print(plots$CO_daily_mean)
print(plots$NO2_daily_mean)
print(plots$O3_daily_mean)
print(plots$SO2_daily_mean)
print(plots$PM10_daily_mean)
print(plots$PM2.5_daily_mean)

accuracy(tbats_forecasts$CO_daily_mean)
accuracy(tbats_forecasts$NO2_daily_mean)
accuracy(tbats_forecasts$O3_daily_mean)
accuracy(tbats_forecasts$SO2_daily_mean)
accuracy(tbats_forecasts$PM10_daily_mean)
accuracy(tbats_forecasts$PM2.5_daily_mean)


#OTRA FORMA DE CALCULAR LA PRECISION

# Calcular la precisión de las predicciones para cada contaminante
accuracy_tbats_list <- list()

for (contaminante in c("O3_daily_mean", "NO2_daily_mean", "CO_daily_mean", "SO2_daily_mean", "PM10_daily_mean", "PM2.5_daily_mean")) {
  # Convertir a serie de tiempo (TS) asumiendo frecuencia diaria
  ts_data <- ts(data_daily[[contaminante]], frequency = 365)
  
  # Ajustar el modelo TBATS
  fit_tbats <- tbats(ts_data)
  
  # Generar pronóstico para los próximos 30 días
  forecast_tbats <- forecast(fit_tbats, h = 30)
  
  # Comparar las predicciones con los últimos 30 días de datos observados (si existen)
  if (nrow(data_daily) >= 30) {
    # Extraer los últimos 30 días observados
    real_values <- tail(ts_data, 30)
    
    # Tomar las primeras 30 predicciones
    predicted_values <- forecast_tbats$mean[1:30]
    
    # Calcular precisión y almacenarla en la lista
    accuracy_tbats <- accuracy(predicted_values, real_values)
    accuracy_tbats_list[[contaminante]] <- accuracy_tbats
  }
}

# Mostrar las métricas de precisión para cada contaminante
accuracy_tbats_list


# ==== Por mes ====

library(dplyr)
library(ggplot2)
library(forecast)
library(tidyr)
library(lubridate)
library(readr)

data <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\Dataset\\NINFAS\\NINFAS_2020-2024_combinado.csv")
data$Datetime <- as.POSIXct(paste(data$FECHA, data$Horas), format="%d/%m/%Y %H:%M:%S", tz="America/Mexico_City")
data <- na.omit(data)

data_daily <- data %>%
  mutate(Day = as.integer(format(Datetime, "%d"))) %>%
  group_by(Day) %>%
  summarise(
    O3_daily_mean = mean(O3, na.rm = TRUE),
    NO2_daily_mean = mean(NO2, na.rm = TRUE),
    CO_daily_mean = mean(CO, na.rm = TRUE),
    SO2_daily_mean = mean(SO2, na.rm = TRUE),
    PM10_daily_mean = mean(PM.10, na.rm = TRUE),
    PM2.5_daily_mean = mean(PM.2.5, na.rm = TRUE)
  )

# Aplicar modelos TBATS para cada contaminante y hacer predicciones de 5 días
tbats_forecasts_month <- list()
for (contaminante in c("O3_daily_mean", "NO2_daily_mean", "CO_daily_mean", "SO2_daily_mean", "PM10_daily_mean", "PM2.5_daily_mean")) {
  # Convertir a serie temporal (frecuencia de 30 días)
  ts_data <- ts(data_daily[[contaminante]], frequency = 30)
  
  # Ajustar el modelo TBATS
  fit_tbats <- tbats(ts_data)
  
  # Generar pronóstico para los próximos 5 días
  forecast_tbats <- forecast(fit_tbats, h = 5)
  
  tbats_forecasts_month[[contaminante]] <- forecast_tbats
}

# Convertir los datos a formato largo para graficar con ggplot
data_long_month <- data_daily %>%
  pivot_longer(cols = c("O3_daily_mean", "NO2_daily_mean", "CO_daily_mean", "SO2_daily_mean", "PM10_daily_mean", "PM2.5_daily_mean"),
               names_to = "Contaminante", values_to = "Concentracion") %>%
  mutate(Tipo = "Histórico")

# Agregar los pronósticos al dataset
data_with_forecast_month <- data_long_month
for (contaminante in names(tbats_forecasts_month)) {
  # Obtener el último día del mes en `data_daily`
  last_day <- max(data_daily$Day, na.rm = TRUE)
  
  # Generar las fechas de pronóstico
  forecast_dates <- seq(from = last_day + 1, by = 1, length.out = 5)  # Usar by=1 para días
  
  forecast_data <- data.frame(
    Day = forecast_dates,
    Concentracion = tbats_forecasts_month[[contaminante]]$mean,
    Contaminante = contaminante,
    Tipo = "Pronóstico"
  )
  
  # Añadir las predicciones al dataset original
  data_with_forecast_month <- rbind(data_with_forecast_month, forecast_data)
}


# Gráfica para análisis por mes
ggplot(data_with_forecast_month, aes(x = Day, y = Concentracion, color = Contaminante)) +
  geom_line(aes(linetype = Tipo, color = ifelse(Tipo == "Pronóstico", "Pronóstico", Contaminante)), linewidth = 1) +  
  scale_color_manual(values = c("Pronóstico" = "red",
                                "CO_daily_mean" = "salmon", 
                                "NO2_daily_mean" = "goldenrod",
                                "O3_daily_mean" = "green",
                                "PM10_daily_mean" = "cyan",
                                "PM2.5_daily_mean" = "lightblue",
                                "SO2_daily_mean" = "magenta")) +
  facet_wrap(~Contaminante, scales = "free_y") +
  labs(title = "Concentración Promedio Diaria por Mes con Pronóstico TBATS",
       x = "Día del mes", y = "Concentración") +
  theme_minimal()
