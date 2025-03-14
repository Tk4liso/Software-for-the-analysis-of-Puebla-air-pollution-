library(shiny)

# ==== Ejercicio 1 ====
ui<-fluidPage(
  textInput("nombre", "¿Cuál es tu nombre?"),
  textOutput("saludos")
)

server<-function(input, output, session){
  output$saludos<-renderText({
    paste0("Hola ", input$nombre)
  })
}

#Construye e inicializa la app
shinyApp(ui, server)