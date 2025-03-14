library(shiny)      # Para construir la aplicación Shiny
library(ggplot2)    # Para la visualización de datos
library(dplyr)      # Para la manipulación de datos
library(lubridate)  # Para el manejo de fechas y horas
library(tidyr)      # Para la transformación de datos
library(readr)      # Para la lectura de archivos CSV
library(forecast)   # Para las predicciones con ARIMA

# Define la función para cargar y procesar datos
load_and_process_data <- function(station, year, type) {
  # Reemplaza "AGUA SANTA" por "AGUASANTA" en el nombre de la estación
  station <- gsub("AGUA SANTA", "AGUASANTA", station)
  
  if (type == "year") {
    # Para el gráfico anual, leer datos de todos los años
    years <- c("2020", "2021", "2022", "2023", "2024")
    data_list <- lapply(years, function(y) {
      file_path <- paste0("C:/Users/Tacos/OneDrive/Documentos/Universidad/Servicio Social/Datos/Dataset/", station, "/", station, "-", y, "-limpiado.csv")
      
      data <- tryCatch({
        read_csv(file_path, col_types = cols())
      }, error = function(e) {
        return(NULL)  # Retorna NULL si hay un error al leer el archivo
      })
      
      if (!is.null(data)) {
        data <- data %>%
          mutate(Datetime = dmy_hms(paste(FECHA, Horas))) %>%  # Crear columna Datetime combinando FECHA y Horas
          #select(-FECHA, -Horas) %>%  # Eliminar las columnas FECHA y Horas
          arrange(Datetime) %>%  # Ordenar los datos por Datetime
          pivot_longer(cols = c(O3, NO2, CO, SO2, `PM-10`, `PM-2.5`), names_to = "Contaminante", values_to = "Concentracion") %>%  # Transformar los datos a formato largo
          mutate(Year = year(Datetime)) %>%  # Extraer el año de la columna Datetime
          #select(-Datetime)  # Eliminar la columna Datetime
        return(data)
      } else {
        return(NULL)
      }
    })
    # Combinar todos los datos en un solo data frame y calcular el promedio de concentración por año y contaminante
    data <- bind_rows(data_list) %>%
      group_by(Year, Contaminante) %>%
      summarise(Promedio_Concentracion = mean(Concentracion, na.rm = TRUE), .groups = 'drop')
  } else {
    # Para los gráficos por hora y por mes, leer datos de un solo año
    file_path <- paste0("C:/Users/Tacos/OneDrive/Documentos/Universidad/Servicio Social/Datos/Dataset/", station, "/", station, "-", year, "-limpiado.csv")
    
    data <- tryCatch({
      read_csv(file_path, col_types = cols())
    }, error = function(e) {
      return(NULL)  # Retorna NULL si hay un error al leer el archivo
    })
    
    if (is.null(data)) {
      return(NULL)
    }
    
    data <- data %>%
      mutate(Datetime = dmy_hms(paste(FECHA, Horas))) %>%  # Crear columna Datetime combinando FECHA y Horas
      select(-FECHA, -Horas) %>%  # Eliminar las columnas FECHA y Horas
      arrange(Datetime) %>%  # Ordenar los datos por Datetime
      pivot_longer(cols = c(O3, NO2, CO, SO2, `PM-10`, `PM-2.5`), names_to = "Contaminante", values_to = "Concentracion")
    
    if (type == "hour") {
      # Para gráficos por hora: calcular el promedio de concentración por hora y contaminante
      data <- data %>%
        mutate(Hora = hour(Datetime)) %>%  # Extraer la hora de la columna Datetime
        group_by(Hora, Contaminante) %>%
        summarise(Promedio_Concentracion = mean(Concentracion, na.rm = TRUE), .groups = 'drop')
    } else if (type == "month") {
      # Para gráficos por mes: calcular el promedio de concentración por mes y contaminante
      data <- data %>%
        mutate(Mes = month(Datetime, label = TRUE, abbr = TRUE)) %>%  # Extraer el mes de la columna Datetime
        group_by(Mes, Contaminante) %>%
        summarise(Promedio_Concentracion = mean(Concentracion, na.rm = TRUE), .groups = 'drop')
    }
  }
  
  return(data)

}


# Define la interfaz de usuario
ui <- fluidPage(
  titlePanel("Promedio de Concentración de Contaminantes en Puebla"),
  sidebarLayout(
    sidebarPanel(
      selectInput("graphType", "Seleccionar Tipo de Gráfico:",
                  choices = c("Por Hora" = "hour", "Por Año" = "year", "Por Mes" = "month")),
      
      uiOutput("station_ui"),  # Salida dinámica para seleccionar la estación
      uiOutput("time_ui"),     # Salida dinámica para seleccionar el año (solo para gráficos que no son por año)
      uiOutput("contaminants_ui"),  # Salida dinámica para seleccionar los contaminantes
      
      # Condicional para mostrar la opción de predicción ARIMA solo si se selecciona "Por Hora"
      conditionalPanel(
        condition = "input.graphType == 'hour'",
        checkboxInput("showPrediction", "Mostrar Predicción ARIMA", value = FALSE)
      )
      
    ),
    mainPanel(
      plotOutput("contaminantPlot")  # Salida dinámica para el gráfico
    )
  )
)


# Define la función del servidor
server <- function(input, output, session) {
  
  # Reacciona a los cambios en el tipo de gráfico para actualizar los controles de entrada
  observe({
    if (input$graphType == "year") {
      output$station_ui <- renderUI({
        selectInput("station", "Seleccionar Estación:",
                    choices = c("NINFAS", "UTP", "AGUA SANTA", "BINE"))
      })
      output$time_ui <- renderUI({
        NULL  # Elimina la selección de año para gráficos por año
      })
      output$contaminants_ui <- renderUI({
        selectInput("contaminants", "Seleccionar Contaminantes:",
                    choices = c("Todos" = "ALL", "O3", "NO2", "CO", "SO2", "PM-10", "PM-2.5"))
      })
    } else {
      output$station_ui <- renderUI({
        selectInput("station", "Seleccionar Estación:",
                    choices = c("NINFAS", "UTP", "AGUA SANTA", "BINE"))
      })
      output$time_ui <- renderUI({
        selectInput("year", "Seleccionar Año:",
                    choices = c("2020", "2021", "2022", "2023", "2024"))
      })
      output$contaminants_ui <- renderUI({
        selectInput("contaminants", "Seleccionar Contaminantes:",
                    choices = c("Todos" = "ALL", "O3", "NO2", "CO", "SO2", "PM-10", "PM-2.5"))
      })
    }
  })
  
  # Reacciona a los cambios en el tipo de gráfico y en los filtros para cargar datos y generar el gráfico
  output$contaminantPlot <- renderPlot({
    data <- load_and_process_data(input$station, input$year, input$graphType)
    
    # Depuración: Imprimir los primeros registros del dataset cargado
    print("Head")
    print(head(data))
    #print("Summary")
    #print(summary(data))
    #print("STR")
    #print(str(data))
    
    # ---------- Hora ----------
    
    if (input$graphType == "hour") {
      if (is.null(data)) {
        ggplot() + 
          labs(title = paste("Error: No se encontraron suficientes datos para", input$station, input$year), x = NULL, y = NULL) +
          theme_void()
      } else {
        if ("ALL" %in% input$contaminants) {
          filtered_data <- data
        } else {
          filtered_data <- data %>%
            filter(Contaminante %in% input$contaminants)
        }
        
        p <- ggplot(filtered_data, aes(x = Hora, y = Promedio_Concentracion, color = Contaminante, group = Contaminante)) +
          geom_line(size = 1) +
          facet_wrap(~ Contaminante, scales = "free_y") +
          labs(title = paste("Promedio de concentración de contaminantes por hora en Puebla -", input$station, input$year),
               x = "Hora del día",
               y = "Promedio de concentración") +
          theme_minimal()
        
        # Añadir las predicciones ARIMA por hora si la opción está activada
        if (input$showPrediction) {
          predicciones <- lapply(unique(filtered_data$Contaminante), function(contaminante) {
            datos_contaminante <- filtered_data %>% filter(Contaminante == contaminante)
            ts_data <- ts(datos_contaminante$Promedio_Concentracion, frequency = 24)
            fit <- auto.arima(ts_data)
            pred <- forecast(fit, h = 24)
            pred_df <- data.frame(Hora = (max(datos_contaminante$Hora) + 1):(max(datos_contaminante$Hora) + 24),
                                  Prediccion = pred$mean,
                                  Contaminante = contaminante)
            return(pred_df)
          })
          
          predicciones <- bind_rows(predicciones)
          
          p <- p + geom_line(data = predicciones, aes(x = Hora, y = Prediccion, color = Contaminante, linetype = "Predicción"), size = 1, inherit.aes = FALSE)
        }
        
        p
        
      }
      
      # ---------- Mes ----------
      
    } else if (input$graphType == "month") {
      if (is.null(data)) {
        ggplot() + 
          labs(title = paste("Error: No se encontraron suficientes datos para", input$station, input$year), x = NULL, y = NULL) +
          theme_void()
      } else {
        if ("ALL" %in% input$contaminants) {
          filtered_data <- data
        } else {
          filtered_data <- data %>%
            filter(Contaminante %in% input$contaminants)
        }
        
        p <- ggplot(filtered_data, aes(x = Mes, y = Promedio_Concentracion, color = Contaminante, group = Contaminante)) +
          geom_line(size = 1) +
          facet_wrap(~ Contaminante, scales = "free_y") +
          labs(title = paste("Promedio de concentración de contaminantes por día del mes en Puebla -", input$station, input$year),
               x = "Mes del año",
               y = "Promedio de concentración") +
          theme_minimal()
        
        p
        
      }
      
      # ---------- Año ----------
      
    } else if (input$graphType == "year") {
      if (is.null(data) || nrow(data) == 0) {
        ggplot() + 
          labs(title = paste("Error: No se encontraron suficientes datos para", input$station), x = NULL, y = NULL) +
          geom_blank()
      } else {
        if ("ALL" %in% input$contaminants) {
          filtered_data <- data
        } else {
          filtered_data <- data %>%
            filter(Contaminante %in% input$contaminants)
        }
        
        p <- ggplot(filtered_data, aes(x = Year, y = Promedio_Concentracion, color = Contaminante, group = Contaminante)) +
          geom_line(size = 1) +
          facet_wrap(~ Contaminante, scales = "free_y") +
          labs(title = paste("Promedio de concentración de contaminantes por año en Puebla -", input$station),
               x = "Año",
               y = "Promedio de concentración") +
          theme_minimal()
        
        p
        
      }
    }
    
  })
}


# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
