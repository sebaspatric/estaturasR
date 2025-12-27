library(shiny)
library(googlesheets4)
library(dplyr)
library(ggplot2)

# --- Autenticación con Google (usa tu archivo JSON) ---
# Guarda el bloque JSON en un archivo llamado "credenciales.json"
# y asegúrate de que esté en la misma carpeta que app.R
gs4_auth(path = "credenciales.json")

# --- URL de tu hoja de cálculo ---
sheet_url <- "https://docs.google.com/spreadsheets/d/1oPAV-vcVykhRwDD_b8beABkAzTtyDCLsHwbuR0W1jS0/edit?usp=sharing"

ui <- fluidPage(
  titlePanel("Ingreso de estaturas en clase 📊"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("nombre", "Nombre del estudiante"),
      numericInput("estatura", "Estatura (cm)", value = 170, min = 100, max = 250),
      actionButton("enviar", "Enviar"),
      sliderInput("paso", "Tamaño del intervalo (cm)", min = 1, max = 10, value = 5)
    ),
    
    mainPanel(
    
      verbatimTextOutput("stats"),
      plotOutput("grafico"),
      tableOutput("tabla")
    )
  )
)

server <- function(input, output, session) {
  
  datos <- reactiveVal(data.frame(Nombre = character(), Estatura = numeric()))
  
  # --- Guardar datos en Google Sheets ---
  observeEvent(input$enviar, {
    nuevo <- data.frame(Nombre = input$nombre, Estatura = input$estatura)
    sheet_append(sheet_url, nuevo)
    datos(rbind(datos(), nuevo))
  })
  
  # --- Leer datos desde Google Sheets ---
  observe({
    df <- read_sheet(sheet_url)
    datos(df)
  })
  
  # --- Mostrar tabla ---
  output$tabla <- renderTable({
    datos()
  })
  
  # --- Estadísticas ---
  output$stats <- renderPrint({
    df <- datos()
    if(nrow(df) > 0){
      cat("Media:", mean(df$Estatura), "\n")
      cat("Desviación estándar:", sd(df$Estatura), "\n")
      cat("Máx:", max(df$Estatura), "\n")
      cat("Mín:", min(df$Estatura), "\n")
    }
  })
  
  # --- Gráfico ---
  # --- Gráfico ---
  output$grafico <- renderPlot({
    df <- datos()
    if(nrow(df) > 0){
      paso <- input$paso
      
      # Histograma con frecuencias
      p <- ggplot(df, aes(x = Estatura)) +
        geom_histogram(binwidth = paso, fill = "skyblue", color = "black", alpha = 0.6) +
        labs(title = "Distribución Normal vs Frecuencia de Estaturas",
             x = "Estatura (cm)", y = "Frecuencia")
      
      # Calcular curva normal en escala de densidad
      mu <- mean(df$Estatura)
      sigma <- sd(df$Estatura)
      curva <- data.frame(
        x = seq(min(df$Estatura), max(df$Estatura), length.out = 100)
      )
      curva$y <- dnorm(curva$x, mean = mu, sd = sigma)
      
      # Escalar la curva para que se vea en el mismo gráfico
      max_freq <- max(hist(df$Estatura, plot = FALSE)$counts)
      max_density <- max(curva$y)
      factor <- max_freq / max_density
      
      p + 
        geom_line(data = curva, aes(x = x, y = y * factor), color = "red", size = 1) +
        scale_y_continuous(sec.axis = sec_axis(~./factor, name = "Densidad Normal"))
    }
  })
  
}

shinyApp(ui, server)

