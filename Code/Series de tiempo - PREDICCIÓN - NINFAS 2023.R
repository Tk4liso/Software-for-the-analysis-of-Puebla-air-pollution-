#Series de tiempo - PREDICCIÓN |NINFAS - 2023|

library(forecast)

data<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\Servicio Social\\Datos\\Dataset\\NINFAS\\NINFAS_2020-2024_combinado.csv")
head(data)
str(data)
summary(data)

#Combinar columnas fecha y hora en un solo vector de tipo POSIXct para poder convertir la data a un objeto de serie de tiempo
data$DateTime <- as.POSIXct(paste(data$FECHA, data$Horas), format="%d/%m/%Y %H:%M:%S")
head(data)

# ==== ARIMA ====

library(dplyr)
library(tseries)
library(ggplot2)

#Convertir a serie de tiempo diaria (media diaria por día)
daily_data<-data %>% 
  group_by(Date = as.Date(DateTime)) %>%
  summarize(O3 = mean(O3))

#Crear serie de tiempo diaria
ts_O3_daily <- ts(daily_data$O3, start = c(2020, 5), frequency = 365)

#Graficar ts para observar patrones
plot(ts_O3_daily, main = "Serie de tiempo de O3", ylab = "O3", xlab= "Tiempo")

#==== Verificación de estacionalidad====

#Aplicar prueba ADF para saber si la ts es estacionaria
adf.test(ts_O3_daily)

#El resultado de la prueba sugiere que la serie de tiempo podría no ser estacionaria, 
#ya que el p-valor es 0.08009, lo cual es mayor que el nivel de significancia comúnmente utilizado (0.05)

#Aplicar la Diferenciación para intentar hacerla estacionaria
original_ts <- ts_O3_daily #Guardamos la ts original para que después nos ayude a revertit la diferenciación
ts_O3_daily<-diff(ts_O3_daily)
adf.test(ts_O3_daily)

#====

#Construcción del modelo ARIMA para la identificación de los parámetros óptimos (p,d,q)
model<-auto.arima(ts_O3_daily)
summary(model)
#ARIMA(1,0,2) parece ser un ajuste razonable

# ==== Ajuste manual del modelo ====
acf(ts_O3_daily)
pacf(ts_O3_daily)

model_manual<-arima(ts_O3_daily, order = c(1,0,2))
summary(model_manual)
#Las medidas y coeficientes del auto.arima y el arima manual son muy similares pero con
#ligeras diferencias. Como tal se podría decir que el auto.arima es un poco mejor

#Revisión de los residuos
checkresiduals(model)

# ==== Predicción ====

forecast_values<-forecast(model,h=30) #h es el horizonte de predicción

# Graficar la serie diferenciada
#plot(forecast_values)
autoplot(forecast(forecast_values,3)) + labs(x="Tiempo", y="Diferencia en la concentración de O3")

forecast(model, 3)
accuracy(forecast_values)
autoplot(model)



#Revertir la diferenciación
reverted_ts<-cumsum(c(original_ts[1], ts_O3_daily))

# Graficar la serie revertida
plot.ts(reverted_ts, main = "Serie Temporal Revertida", ylab = "Concentración de O3", xlab = "Tiempo")

# Comparar con la serie original
plot.ts(original_ts, main = "Serie Temporal Original", ylab = "Concentración de O3", xlab = "Tiempo")




# Revertir diferenciación en las predicciones
forecast_cumsum <- cumsum(c(original_ts[length(original_ts)], forecast_values$mean))[-1]

# Preparar el eje X para las predicciones
last_date <- tail(daily_data$Date, 1)
daily_data <- daily_data %>% filter(!is.na(Date))
last_date <- tail(daily_data$Date, 1)
forecast_dates <- seq(from = as.Date(last_date) + 1, by = "day", length.out = length(forecast_values$mean))

# Crear un vector de fechas para la serie temporal original
original_dates <- seq(from = min(daily_data$Date), by = "day", length.out = length(original_ts))

# Graficar la serie temporal original y las predicciones revertidas
plot(original_dates, original_ts, type = "l", main = "Serie Temporal Original con Predicciones", ylab = "Concentración de O3", col = "blue", lwd = 2, xlim = c(min(original_dates), max(forecast_dates)))
lines(forecast_dates, forecast_cumsum, col = "red", lwd = 2, lty = 2)




#Gráfica 2 (un poco más gruesas las líneas y la predicción es un trazo continuo, no punteado)

plot_data <- data.frame(
  Date = c(original_dates, forecast_dates),
  O3 = c(original_ts, forecast_cumsum),
  Type = c(rep("Original", length(original_ts)), rep("Predicción", length(forecast_cumsum)))
)

ggplot(plot_data, aes(x = Date, y = O3, color = Type)) +
  geom_line(size = 1) +
  labs(title = "Serie Temporal de O3 con Predicciones", x = "Fecha", y = "Concentración de O3") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal() +
  theme(legend.position = "top")
