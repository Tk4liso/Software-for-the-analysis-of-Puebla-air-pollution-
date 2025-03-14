#Scrapin calidad del aire | 1 Enero 2024 -  1 Mayo 2024|

library(RSelenium)

# Iniciar RSelenium
rD <- rsDriver(browser = "chrome", port = 4445L, verbose = FALSE)
remDr <- rD[["client"]]

# Navegar a la página web
remDr$navigate("https://calidaddelaire.puebla.gob.mx/views/reporteICA.php")

# Extraer el contenido de la página
page_source <- remDr$getPageSource()[[1]]

# Leer el HTML con rvest
pagina <- read_html(page_source)

# Extraer la tabla
tabla <- pagina %>%
  html_element(css = '#div_tabla_ICA table') %>%
  html_table()

print(tabla)

# Cerrar RSelenium
remDr$close()
rD$server$stop()

