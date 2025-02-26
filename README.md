library(shiny)
library(plotly)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Cargar datos desde los archivos Excel
datos_lesionados <- read_excel("personas_lesionadas.xlsx") %>%
  rename(`Trimestre Año` = Trimestre_Año) %>%  
  mutate(`Trimestre Año` = gsub("_", " ", `Trimestre Año`),
         `Trimestre Año` = factor(`Trimestre Año`, 
                                  levels = c("IT 2019", "IIT 2019", "IIIT 2019", "IVT 2019", 
                                             "IT 2020", "IIT 2020", "IIIT 2020", "IVT 2020", 
                                             "IT 2021", "IIT 2021", "IIIT 2021", "IVT 2021", 
                                             "IT 2022", "IIT 2022", "IIIT 2022", "IVT 2022", 
                                             "IT 2023", "IIT 2023", "IIIT 2023", "IVT 2023", 
                                             "IT 2024", "IIT 2024", "IIIT 2024", "IVT 2024")))

datos_fallecidos <- read_excel("personas_fallecidas.xlsx") %>%
  rename(`Trimestre Año` = Trimestre_Año) %>%  
  mutate(`Trimestre Año` = gsub("_", " ", `Trimestre Año`),
         `Trimestre Año` = factor(`Trimestre Año`, 
                                  levels = c("IT 2019", "IIT 2019", "IIIT 2019", "IVT 2019", 
                                             "IT 2020", "IIT 2020", "IIIT 2020", "IVT 2020", 
                                             "IT 2021", "IIT 2021", "IIIT 2021", "IVT 2021", 
                                             "IT 2022", "IIT 2022", "IIIT 2022", "IVT 2022", 
                                             "IT 2023", "IIT 2023", "IIIT 2023", "IVT 2023", 
                                             "IT 2024", "IIT 2024", "IIIT 2024", "IVT 2024")))

Matriz_Personas_Fallecidas2024 <- read_excel("matriz_personas_fallecidas.xlsx") 

Reportes_viales2024 <- read_excel("Reportes_Incidentes_Viales.xlsx") %>%
  mutate(`Trimestre Año` = factor(`Trimestre Año`, 
                                  levels = c("IT 2019", "IIT 2019", "IIIT 2019", "IVT 2019", 
                                             "IT 2020", "IIT 2020", "IIIT 2020", "IVT 2020", 
                                             "IT 2021", "IIT 2021", "IIIT 2021", "IVT 2021", 
                                             "IT 2022", "IIT 2022", "IIIT 2022", "IVT 2022", 
                                             "IT 2023", "IIT 2023", "IIIT 2023", "IVT 2023", 
                                             "IT 2024", "IIT 2024", "IIIT 2024", "IVT 2024")))

# Nueva paleta de colores basada en la imagen proporcionada
colores_paleta <- c("indigo_dye" = "#1F456E", "ming" = "#427D9D", "sage" = "#9BBEC8", 
                    "pale_aqua" = "#DDF2FD", "almost_black" = "#16423C", "conductor" = "#fedc97")

# Nueva paleta para líneas en Totales (colores vibrantes y distinguibles)
colores_totales <- c("Motociclista" = "#FF4500",  # Naranja brillante
                     "Peatón" = "#32CD32",       # Verde lima
                     "Ciclista" = "#1E90FF",     # Azul dodger
                     "Conductor" = "#FFD700",    # Dorado
                     "Pasajero" = "#8A2BE2")     # Violeta vibrante

# UI
iu <- fluidPage(
  tags$style(HTML("
    .btn { text-align: left; }  /* Alinea el texto de los botones a la izquierda */
    .pie-container { height: 350px; } /* Ajustar altura para eliminar espacio innecesario */
  ")),
  
  tags$div(
    style = "background-color: #e9ecef; border-radius: 10px; padding: 20px; text-align: center; 
             font-size: 24px; font-weight: bold; color: #1F456E; border: 1px solid #ced4da;",
    tags$span("Visualizador Interactivo del Reporte Trimestral De Hechos De Tránsito Abril - Junio 2024")
  ),
  
  # Recuadro 1: Información del autor (con fondo blanco)
  tags$div(
    style = "background-color: #ffffff; border-radius: 10px; padding: 15px; margin: 20px; border: 1px solid #ced4da; text-align: justify; font-size: 16px; color: #1F456E;",
    tags$p("Mtro. José César Romero Galván"),
    tags$p("Estudiante de Doctorado en Ciencias Políticas y Sociales en el campo disciplinario de la Administración Pública en la Universidad Nacional Autónoma de México (UNAM), obtuve Mención Honorífica en mis estudios de maestría en Gobierno y Asuntos Públicos (Políticas Públicas) en la misma institución. Fui reconocido con el segundo lugar en el 1er Premio Nacional de Políticas Públicas, organizado por El Colegio de México (COLMEX) y la Universidad de Monterrey (UDEM) y como uno de los seis Enlaces Universitarios del Banco de México con mejor desempeño en el año 2018. Además, representé al alumnado de Maestría en el Comité Académico del Programa de Posgrado en la UNAM en el periodo 2023 - 2025. He tomado distintos cursos sobre econometría, estadística inferencial y de Ciencia de Datos para las Ciencias Sociales en instituciones como la UNAM, CIDE y FLACSO México. He trabajado en proyectos de investigación y voluntariados en instituciones como CEPAL, UNESCO y UNAM, donde he realizado análisis y visualización de datos utilizando RStudio.")
  ),
  
  # Texto original del segundo recuadro (sin fondo blanco)
  tags$div(
    style = "text-align: justify; font-size: 16px; padding: 10px; color: #1F456E;",
    tags$p("Este visualizador contiene gráficos interactivos que representan estadísticas oficiales de personas lesionadas y fallecidas en hechos de tránsito, obtenidas de la Secretaría de Movilidad del Gobierno de la Ciudad de México en colaboración con la Secretaría de Seguridad Ciudadana, la Fiscalía General de Justicia de la Ciudad de México, C5, Bloomberg Philanthropies y la Initiative for Global Road Safety. El objetivo de esta aplicación es proporcionar una herramienta de visualización de los datos relacionados con los hechos de tránsito, permitiendo analizar las tendencias a lo largo de los años, con el fin de darle una herramienta a los tomadores de decisiones para mejorar las políticas públicas de movilidad en la capital del país."),
    tags$p(style = "color: #427D9D;", "Datos más recientes disponibles: Abril - Junio de 2024.")
  ),
  
  tags$div(
    style = "background-color: #f8f9fa; border-radius: 10px; padding: 15px; margin: 20px; border: 1px solid #ced4da;",
    tags$h3("Reportes de incidentes viales ingresados al C5", style = "color: #1F456E; font-weight: normal; text-align: center;"),
    tags$p("Esta gráfica muestra la evolución de los reportes de incidentes viales recibidos por el Centro de Comando, Control, Cómputo, Comunicaciones y Contacto Ciudadano (C5) desde el primer trimestre de 2019 hasta el segundo trimestre de 2024. Permite analizar tendencias temporales en la incidencia de estos eventos.", 
           style = "color: #1F456E; font-size: 14px; text-align: center;"),
    div(style = "background-color: #1F456E; color: white; padding: 5px; border-radius: 5px;",
        radioButtons("tipo_grafico_reportes", "Seleccionar tipo de gráfico:",
                     choices = list("Barras" = "barra", "Líneas" = "linea"),
                     selected = "barra", inline = TRUE)),
    plotlyOutput("grafico_reportes_viales"),
    downloadButton("descargar_reportes_viales", "Descargar Datos de Reportes Viales", 
                   style = "background-color: #fedc97; color: black; border: none; margin-right: 10px;"),
    downloadButton("descargar_grafico_reportes_png", "Descargar Gráfica", 
                   style = "background-color: #fedc97; color: black; border: none;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #1F456E; color: white; text-align: left; width: 100%;",
      selectInput("usuario", "Selecciona el tipo de usuario:",
                  choices = c("Totales", colnames(datos_lesionados)[-1]), selected = "Totales"),
      radioButtons("tipo_dato", "Seleccionar tipo de datos:",
                   choices = list("Lesionados" = "lesionados", "Fallecidos" = "fallecidos"),
                   selected = "lesionados"),
      radioButtons("grafico_tipo", "Seleccionar tipo de gráfico:",
                   choices = list("Líneas" = "linea", "Barras" = "barra"),
                   selected = "linea"),
      downloadButton("descargar_filtrado", "Descargar Datos Filtrados", 
                     style = "background-color: #fedc97; color: black; border: none;"),
      downloadButton("descargar_completo", "Descargar Base de Datos Completa", 
                     style = "background-color: #fedc97; color: black; border: none;")
    ),
    
    mainPanel(
      fluidRow(
        column(
          width = 12,
          tags$div(
            style = "background-color: #f8f9fa; border-radius: 10px; padding: 15px; margin: 20px; border: 1px solid #ced4da;",
            uiOutput("titulo_grafico_tendencia"),
            tags$p("Esta gráfica permite visualizar la tendencia de personas lesionadas o fallecidas en hechos de tránsito por tipo de usuario (motociclistas, peatones, etc.) o en total, desde el primer trimestre de 2019 hasta el segundo trimestre de 2024. Facilita la identificación de patrones a lo largo del tiempo.", 
                   style = "color: #1F456E; font-size: 14px; text-align: center;"),
            plotlyOutput("grafico", height = "600px"),
            downloadButton("descargar_grafico_tendencia_png", "Descargar Gráfica", 
                           style = "background-color: #fedc97; color: black; border: none;")
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          tags$div(
            style = "background-color: #f8f9fa; border-radius: 10px; padding: 15px; margin: 20px; border: 1px solid #ced4da;",
            tags$h3("Distribución porcentual", style = "color: #1F456E; font-weight: normal; text-align: center;"),
            tags$p("Muestra la distribución porcentual de personas lesionadas o fallecidas por tipo de usuario (2019-2024). Usa el panel izquierdo para alternar entre personas lesionadas y fallecidas.", 
                   style = "color: #1F456E; font-size: 14px; text-align: center;"),
            actionButton("togglePiePorcentaje", "Cambiar a Pastel/Anillos", 
                         style = "background-color: #1F456E; color: white; border: none; margin-right: 10px;"),
            div(class = "pie-container", plotlyOutput("grafico_pastel_porcentaje", height = "350px")),
            downloadButton("descargar_pastel_porcentaje", "Descargar Datos", 
                           style = "background-color: #fedc97; color: black; border: none; margin-right: 10px;"),
            downloadButton("descargar_grafico_pastel_porcentaje_png", "Descargar Gráfica", 
                           style = "background-color: #fedc97; color: black; border: none;")
          )
        ),
        column(
          width = 6,
          tags$div(
            style = "background-color: #f8f9fa; border-radius: 10px; padding: 15px; margin: 20px; border: 1px solid #ced4da;",
            tags$h3("Distribución total", style = "color: #1F456E; font-weight: normal; text-align: center;"),
            tags$p("Presenta el número total de personas lesionadas o fallecidas por tipo de usuario (2019-2024). Usa el panel izquierdo para alternar entre personas lesionadas y fallecidas.", 
                   style = "color: #1F456E; font-size: 14px; text-align: center;"),
            actionButton("togglePieTotal", "Cambiar a Pastel/Anillos", 
                         style = "background-color: #1F456E; color: white; border: none; margin-right: 10px;"),
            div(class = "pie-container", plotlyOutput("grafico_pastel_total", height = "350px")),
            downloadButton("descargar_pastel_total", "Descargar Datos", 
                           style = "background-color: #fedc97; color: black; border: none; margin-right: 10px;"),
            downloadButton("descargar_grafico_pastel_total_png", "Descargar Gráfica", 
                           style = "background-color: #fedc97; color: black; border: none;")
          )
        )
      )
    )
  ),
  
  tags$div(
    style = "background-color: #f8f9fa; border-radius: 10px; padding: 15px; margin: 20px; border: 1px solid #ced4da;",
    tags$h3("Tabla de personas fallecidas IIT 2024", style = "color: #1F456E; text-align: center; font-weight: normal;"),
    tags$style(HTML("
      table thead th {
        color: #1F456E;  /* Color azul para la primera fila */
      }
    ")),
    tableOutput("tabla_matriz"),
    downloadButton("descargar_tabla", "Descargar Datos de la Tabla", 
                   style = "background-color: #fedc97; color: black; border: none;")
  ),
  
  tags$footer(
    tags$div(
      style = "text-align: center; padding: 20px; font-size: 14px; color: #1F456E; background-color: #e9ecef; border-radius: 10px; border: 1px solid #ced4da;",
      "Aplicación desarrollada con Shiny de Rstudio por José César Romero Galván.",
      tags$br(),
      "Fuente de los datos: ",
      tags$a(href = "https://semovi.cdmx.gob.mx/storage/app/media/HT/2024/ReporteHT_2doTrimestre2024_.pdf", 
             target = "_blank", "Reporte Trimestral de Hechos de Tránsito Abril - Junio de 2024"),
      tags$br(),
      "Código de fuente de esta app y del procesamiento de los datos disponible en",
      tags$br(),
      tags$a(href = "https://github.com/CesarRomero10/Movilidad_CDMX", target = "_blank", "GitHub")
    )
  ),
  
  div(class = "logo-container", 
      style = "text-align: center;", 
      img(src = "https://www.politicas.unam.mx/images/logo/unam-fcpys.png", alt = "UNAM FCPyS", width = "350px"))
)

# Servidor
servidor <- function(input, output, session) {
  
  # Estado reactivo para alternar entre gráficas de pastel y anillos
  pieGraphTypePorcentaje <- reactiveVal("donut")  # Por defecto, inicia con anillos para porcentajes
  pieGraphTypeTotal <- reactiveVal("donut")       # Por defecto, inicia con anillos para totales
  
  # Actualizar tipo de gráfica para "Distribución porcentual"
  observeEvent(input$togglePiePorcentaje, {
    if (pieGraphTypePorcentaje() == "donut") {
      pieGraphTypePorcentaje("pie")
      updateActionButton(session, "togglePiePorcentaje", label = "Cambiar a Anillos")
    } else {
      pieGraphTypePorcentaje("donut")
      updateActionButton(session, "togglePiePorcentaje", label = "Cambiar a Pastel")
    }
  })
  
  # Actualizar tipo de gráfica para "Distribución total"
  observeEvent(input$togglePieTotal, {
    if (pieGraphTypeTotal() == "donut") {
      pieGraphTypeTotal("pie")
      updateActionButton(session, "togglePieTotal", label = "Cambiar a Anillos")
    } else {
      pieGraphTypeTotal("donut")
      updateActionButton(session, "togglePieTotal", label = "Cambiar a Pastel")
    }
  })
  
  # Título dinámico para la gráfica de tendencia
  output$titulo_grafico_tendencia <- renderUI({
    if (input$usuario == "Totales") {
      tags$h3("Tendencia de personas lesionadas o fallecidas en hechos de tránsito", style = "color: #1F456E; font-weight: normal; text-align: center;")
    } else {
      tipo_dato_texto <- ifelse(input$tipo_dato == "lesionados", "lesionados", "fallecidos")
      usuario_texto <- tolower(tipo_usuario_plural(input$usuario))
      tags$h3(paste("Gráfica de tendencia de", usuario_texto, tipo_dato_texto, "en hechos de tránsito"), style = "color: #1F456E; font-weight: normal; text-align: center;")
    }
  })
  
  # Gráfica de barras o líneas para reportes viales
  output$grafico_reportes_viales <- renderPlotly({
    if (input$tipo_grafico_reportes == "barra") {
      p <- ggplot(Reportes_viales2024, aes(x = `Trimestre Año`, y = `Reportes por incidentes viales ingresados al C5`)) +
        geom_bar(stat = "identity", fill = colores_paleta["sage"]) +
        theme_minimal() +
        labs(title = NULL, x = "Trimestre", y = "Número de Reportes") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(color = colores_paleta["indigo_dye"], size = 16),
              axis.title = element_text(color = colores_paleta["indigo_dye"]),
              axis.text = element_text(color = colores_paleta["indigo_dye"])) +
        theme(plot.background = element_rect(fill = "#e9ecef"))
      
      ggplotly(p) %>%
        layout(hoverlabel = list(font = list(color = "white")))
    } else {
      p <- ggplot(Reportes_viales2024, aes(x = `Trimestre Año`, y = `Reportes por incidentes viales ingresados al C5`, group = 1)) +
        geom_line(color = colores_paleta["ming"], size = 0.3) +
        geom_point(color = "#00CED1", size = 3, shape = 16) +
        theme_minimal() +
        labs(title = NULL, x = "Trimestre", y = "Número de Reportes") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(color = colores_paleta["indigo_dye"], size = 16),
              axis.title = element_text(color = colores_paleta["indigo_dye"]),
              axis.text = element_text(color = colores_paleta["indigo_dye"])) +
        theme(plot.background = element_rect(fill = "#e9ecef"))
      
      ggplotly(p)
    }
  })
  
  # Descargar datos de Reportes_viales2024
  output$descargar_reportes_viales <- downloadHandler(
    filename = function() { "reportes_viales_2024.csv" },
    content = function(file) { write.csv(Reportes_viales2024, file, row.names = FALSE) }
  )
  
  # Descargar gráfico de reportes viales en PNG
  output$descargar_grafico_reportes_png <- downloadHandler(
    filename = function() { "grafico_reportes_viales.png" },
    content = function(file) {
      if (input$tipo_grafico_reportes == "barra") {
        p <- ggplot(Reportes_viales2024, aes(x = `Trimestre Año`, y = `Reportes por incidentes viales ingresados al C5`)) +
          geom_bar(stat = "identity", fill = colores_paleta["sage"]) +
          theme_minimal() +
          labs(title = NULL, x = "Trimestre", y = "Número de Reportes") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(color = colores_paleta["indigo_dye"], size = 16),
                axis.title = element_text(color = colores_paleta["indigo_dye"]),
                axis.text = element_text(color = colores_paleta["indigo_dye"]),
                plot.background = element_rect(fill = "#e9ecef"))
      } else {
        p <- ggplot(Reportes_viales2024, aes(x = `Trimestre Año`, y = `Reportes por incidentes viales ingresados al C5`, group = 1)) +
          geom_line(color = colores_paleta["ming"], size = 0.3) +
          geom_point(color = "#00CED1", size = 3, shape = 16) +
          theme_minimal() +
          labs(title = NULL, x = "Trimestre", y = "Número de Reportes") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(color = colores_paleta["indigo_dye"], size = 16),
                axis.title = element_text(color = colores_paleta["indigo_dye"]),
                axis.text = element_text(color = colores_paleta["indigo_dye"]),
                plot.background = element_rect(fill = "#e9ecef"))
      }
      ggsave(file, plot = p, device = "png", width = 9, height = 5, dpi = 300)
    }
  )
  
  # Función reactiva para obtener los datos filtrados
  datos_filtrados <- reactive({
    if (input$tipo_dato == "lesionados") {
      if (input$usuario == "Totales") { datos_lesionados } 
      else { datos_lesionados %>% select(`Trimestre Año`, input$usuario) }
    } else {
      if (input$usuario == "Totales") { datos_fallecidos } 
      else { datos_fallecidos %>% select(`Trimestre Año`, input$usuario) }
    }
  })
  
  # Función para descargar los datos filtrados
  output$descargar_filtrado <- downloadHandler(
    filename = function() { paste("datos_filtrados_", input$usuario, ".csv", sep = "") },
    content = function(file) { write.csv(datos_filtrados(), file, row.names = FALSE) }
  )
  
  # Función para descargar la base de datos completa
  output$descargar_completo <- downloadHandler(
    filename = function() { "base_datos_completa.csv" },
    content = function(file) { write.csv(rbind(datos_lesionados, datos_fallecidos), file, row.names = FALSE) }
  )
  
  tipo_usuario_plural <- function(usuario) {
    switch(usuario,
           "Motociclista" = "motociclistas",
           "Peatón" = "peatones",
           "Ciclista" = "ciclistas",
           "Conductor" = "conductores",
           "Pasajero" = "pasajeros",
           usuario)
  }
  
  datos_seleccionados <- reactive({
    if (input$tipo_dato == "lesionados") { datos_lesionados } else { datos_fallecidos }
  })
  
  # Gráfico de líneas o barras
  output$grafico <- renderPlotly({
    tipo_usuario <- tipo_usuario_plural(input$usuario)
    tipo_dato_texto <- ifelse(input$tipo_dato == "lesionados", "lesionados", "fallecidos")
    
    if (input$grafico_tipo == "linea") {
      if (input$usuario == "Totales") {
        datos_long <- datos_seleccionados() %>%
          pivot_longer(cols = -`Trimestre Año`, names_to = "tipo_usuario", values_to = "valor")
        
        p <- ggplot(datos_long, aes(x = `Trimestre Año`, y = valor, color = tipo_usuario, group = tipo_usuario, 
                                    text = paste("Trimestre:", `Trimestre Año`, "<br>",
                                                 "Valor:", valor, "<br>",
                                                 "Usuario:", tipo_usuario))) +
          geom_line(size = 0.3) +
          geom_point(size = 2.5, color = "#00CED1", shape = 16) +
          scale_color_manual(values = colores_totales, name = "Tipo de usuario") +
          theme_minimal() +
          labs(title = NULL, x = "Trimestre", y = paste("Número de personas", tipo_dato_texto)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(color = colores_paleta["indigo_dye"], size = 16),
                axis.title = element_text(color = colores_paleta["indigo_dye"]),
                axis.text = element_text(color = colores_paleta["indigo_dye"]),
                legend.position = "bottom",
                legend.title = element_text(size = 10),
                plot.margin = unit(c(1, 1, 3, 1), "cm"),
                plot.background = element_rect(fill = "#e9ecef"))
        
        ggplotly(p, tooltip = "text") %>%
          layout(legend = list(orientation = "h", x = 0.1, y = -0.25))
      } else {
        p <- ggplot(datos_seleccionados(), aes(x = `Trimestre Año`, y = .data[[input$usuario]], group = 1)) + 
          geom_line(color = colores_paleta["ming"], size = 0.3) +
          geom_point(color = "#00CED1", size = 2.5, shape = 16) +
          theme_minimal() +
          labs(title = NULL, x = "Trimestre", y = paste("Número de personas", tipo_dato_texto)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(color = colores_paleta["indigo_dye"], size = 16),
                axis.title = element_text(color = colores_paleta["indigo_dye"]),
                axis.text = element_text(color = colores_paleta["indigo_dye"]),
                plot.background = element_rect(fill = "#e9ecef"))
        
        ggplotly(p)
      }
    } else {
      if (input$usuario == "Totales") {
        datos_sum <- datos_seleccionados() %>%
          rowwise() %>%
          mutate(Total = sum(c_across(-`Trimestre Año`), na.rm = TRUE)) %>%
          select(`Trimestre Año`, Total)
        
        p <- ggplot(datos_sum, aes(x = `Trimestre Año`, y = Total)) + 
          geom_bar(stat = "identity", fill = colores_paleta["sage"]) +
          theme_minimal() +
          labs(title = NULL, x = "Trimestre", y = paste("Número de personas", tipo_dato_texto)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(color = colores_paleta["indigo_dye"], size = 16),
                axis.title = element_text(color = colores_paleta["indigo_dye"]),
                axis.text = element_text(color = colores_paleta["indigo_dye"]),
                plot.background = element_rect(fill = "#e9ecef"))
        
        ggplotly(p) %>%
          layout(hoverlabel = list(font = list(color = "white")))
      } else {
        p <- ggplot(datos_seleccionados(), aes(x = `Trimestre Año`, y = .data[[input$usuario]])) + 
          geom_bar(stat = "identity", fill = colores_paleta["sage"]) +
          theme_minimal() +
          labs(title = NULL, x = "Trimestre", y = paste("Número de personas", tipo_dato_texto)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(color = colores_paleta["indigo_dye"], size = 16),
                axis.title = element_text(color = colores_paleta["indigo_dye"]),
                axis.text = element_text(color = colores_paleta["indigo_dye"]),
                plot.background = element_rect(fill = "#e9ecef"))
        
        ggplotly(p) %>%
          layout(hoverlabel = list(font = list(color = "white")))
      }
    }
  })
  
  # Descargar gráfico de tendencia en PNG
  output$descargar_grafico_tendencia_png <- downloadHandler(
    filename = function() { "grafico_tendencia.png" },
    content = function(file) {
      tipo_dato_texto <- ifelse(input$tipo_dato == "lesionados", "lesionados", "fallecidos")
      if (input$grafico_tipo == "linea") {
        if (input$usuario == "Totales") {
          datos_long <- datos_seleccionados() %>%
            pivot_longer(cols = -`Trimestre Año`, names_to = "tipo_usuario", values_to = "valor")
          p <- ggplot(datos_long, aes(x = `Trimestre Año`, y = valor, color = tipo_usuario, group = tipo_usuario)) +
            geom_line(size = 0.3) +
            geom_point(size = 2.5, color = "#00CED1", shape = 16) +
            scale_color_manual(values = colores_totales, name = "Tipo de usuario") +
            theme_minimal() +
            labs(title = NULL, x = "Trimestre", y = paste("Número de personas", tipo_dato_texto)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  plot.title = element_text(color = colores_paleta["indigo_dye"], size = 16),
                  axis.title = element_text(color = colores_paleta["indigo_dye"]),
                  axis.text = element_text(color = colores_paleta["indigo_dye"]),
                  legend.position = "bottom",
                  legend.title = element_text(size = 10),
                  plot.margin = unit(c(1, 1, 3, 1), "cm"),
                  plot.background = element_rect(fill = "#e9ecef"))
        } else {
          p <- ggplot(datos_seleccionados(), aes(x = `Trimestre Año`, y = .data[[input$usuario]], group = 1)) + 
            geom_line(color = colores_paleta["ming"], size = 0.3) +
            geom_point(color = "#00CED1", size = 2.5, shape = 16) +
            theme_minimal() +
            labs(title = NULL, x = "Trimestre", y = paste("Número de personas", tipo_dato_texto)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  plot.title = element_text(color = colores_paleta["indigo_dye"], size = 16),
                  axis.title = element_text(color = colores_paleta["indigo_dye"]),
                  axis.text = element_text(color = colores_paleta["indigo_dye"]),
                  plot.background = element_rect(fill = "#e9ecef"))
        }
      } else {
        if (input$usuario == "Totales") {
          datos_sum <- datos_seleccionados() %>%
            rowwise() %>%
            mutate(Total = sum(c_across(-`Trimestre Año`), na.rm = TRUE)) %>%
            select(`Trimestre Año`, Total)
          p <- ggplot(datos_sum, aes(x = `Trimestre Año`, y = Total)) + 
            geom_bar(stat = "identity", fill = colores_paleta["sage"]) +
            theme_minimal() +
            labs(title = NULL, x = "Trimestre", y = paste("Número de personas", tipo_dato_texto)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  plot.title = element_text(color = colores_paleta["indigo_dye"], size = 16),
                  axis.title = element_text(color = colores_paleta["indigo_dye"]),
                  axis.text = element_text(color = colores_paleta["indigo_dye"]),
                  plot.background = element_rect(fill = "#e9ecef"))
        } else {
          p <- ggplot(datos_seleccionados(), aes(x = `Trimestre Año`, y = .data[[input$usuario]])) + 
            geom_bar(stat = "identity", fill = colores_paleta["sage"]) +
            theme_minimal() +
            labs(title = NULL, x = "Trimestre", y = paste("Número de personas", tipo_dato_texto)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  plot.title = element_text(color = colores_paleta["indigo_dye"], size = 16),
                  axis.title = element_text(color = colores_paleta["indigo_dye"]),
                  axis.text = element_text(color = colores_paleta["indigo_dye"]),
                  plot.background = element_rect(fill = "#e9ecef"))
        }
      }
      ggsave(file, plot = p, device = "png", width = 9, height = 5, dpi = 300)
    }
  )
  
  # Datos para las gráficas de pastel
  datos_pastel <- reactive({
    datos_seleccionados() %>%
      summarise(across(-`Trimestre Año`, sum, na.rm = TRUE)) %>%
      pivot_longer(cols = everything(), names_to = "tipo_usuario", values_to = "total") %>%
      mutate(porcentaje = total / sum(total) * 100)
  })
  
  # Gráfica de pastel (porcentajes)
  output$grafico_pastel_porcentaje <- renderPlotly({
    datos <- datos_pastel()
    colores_ajustados <- ifelse(datos$tipo_usuario == "Conductor", "#fedc97", colores_paleta[match(datos$tipo_usuario, names(colores_paleta))])
    colores_ajustados[is.na(colores_ajustados)] <- colores_paleta[!names(colores_paleta) %in% c("conductor")][1:sum(is.na(colores_ajustados))]
    
    if (pieGraphTypePorcentaje() == "donut") {
      p_pastel <- plot_ly(datos, labels = ~tipo_usuario, values = ~porcentaje, type = 'pie', hole = 0.6,
                          textinfo = 'label+percent', hoverinfo = 'label+percent',
                          marker = list(colors = colores_ajustados)) %>%
        layout(title = NULL, titlefont = list(color = colores_paleta["indigo_dye"], size = 18, family = "Arial, sans-serif"),
               showlegend = FALSE, paper_bgcolor = "#e9ecef")
    } else {
      p_pastel <- plot_ly(datos, labels = ~tipo_usuario, values = ~porcentaje, type = 'pie',
                          textinfo = 'label+percent', hoverinfo = 'label+percent',
                          marker = list(colors = colores_ajustados)) %>%
        layout(title = NULL, titlefont = list(color = colores_paleta["indigo_dye"], size = 18, family = "Arial, sans-serif"),
               showlegend = FALSE, paper_bgcolor = "#e9ecef")
    }
    p_pastel
  })
  
  # Gráfica de pastel (totales)
  output$grafico_pastel_total <- renderPlotly({
    datos <- datos_pastel()
    colores_ajustados <- ifelse(datos$tipo_usuario == "Conductor", "#fedc97", colores_paleta[match(datos$tipo_usuario, names(colores_paleta))])
    colores_ajustados[is.na(colores_ajustados)] <- colores_paleta[!names(colores_paleta) %in% c("conductor")][1:sum(is.na(colores_ajustados))]
    
    if (pieGraphTypeTotal() == "donut") {
      p_pastel <- plot_ly(datos, labels = ~tipo_usuario, values = ~total, type = 'pie', hole = 0.6,
                          textinfo = 'label+value', hoverinfo = 'label+value',
                          marker = list(colors = colores_ajustados)) %>%
        layout(title = NULL, titlefont = list(color = colores_paleta["indigo_dye"], size = 18, family = "Arial, sans-serif"),
               showlegend = FALSE, paper_bgcolor = "#e9ecef")
    } else {
      p_pastel <- plot_ly(datos, labels = ~tipo_usuario, values = ~total, type = 'pie',
                          textinfo = 'label+value', hoverinfo = 'label+value',
                          marker = list(colors = colores_ajustados)) %>%
        layout(title = NULL, titlefont = list(color = colores_paleta["indigo_dye"], size = 18, family = "Arial, sans-serif"),
               showlegend = FALSE, paper_bgcolor = "#e9ecef")
    }
    p_pastel
  })
  
  # Descargar datos de la gráfica de pastel (porcentajes)
  output$descargar_pastel_porcentaje <- downloadHandler(
    filename = function() { "datos_pastel_porcentajes.csv" },
    content = function(file) { write.csv(datos_pastel() %>% select(tipo_usuario, porcentaje), file, row.names = FALSE) }
  )
  
  # Descargar gráfico de pastel (porcentajes) en PNG
  output$descargar_grafico_pastel_porcentaje_png <- downloadHandler(
    filename = function() { 
      if (pieGraphTypePorcentaje() == "donut") "grafico_porcentajes_anillos.png" else "grafico_pastel_porcentajes.png" 
    },
    content = function(file) {
      datos <- datos_pastel()
      colores_ajustados <- ifelse(datos$tipo_usuario == "Conductor", "#fedc97", colores_paleta[match(datos$tipo_usuario, names(colores_paleta))])
      colores_ajustados[is.na(colores_ajustados)] <- colores_paleta[!names(colores_paleta) %in% c("conductor")][1:sum(is.na(colores_ajustados))]
      
      if (pieGraphTypePorcentaje() == "donut") {
        p <- ggplot(datos, aes(x = 2, y = porcentaje, fill = tipo_usuario)) +
          geom_bar(stat = "identity", width = 1, color = "#e9ecef") +
          coord_polar("y", start = 0) +
          geom_bar(data = datos, aes(x = 1.5, y = 0), stat = "identity", fill = "#e9ecef") +
          scale_fill_manual(values = colores_ajustados) +
          theme_void() +
          theme(plot.background = element_rect(fill = "#e9ecef"), legend.position = "none") +
          geom_text(aes(label = paste0(tipo_usuario, "\n", round(porcentaje, 1), "%")), 
                    position = position_stack(vjust = 0.5), color = "white", size = 3) +
          xlim(0.5, 2.5)
      } else {
        p <- ggplot(datos, aes(x = "", y = porcentaje, fill = tipo_usuario)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar("y", start = 0) +
          scale_fill_manual(values = colores_ajustados) +
          theme_void() +
          theme(plot.background = element_rect(fill = "#e9ecef"), legend.position = "none") +
          geom_text(aes(label = paste0(tipo_usuario, "\n", round(porcentaje, 1), "%")), 
                    position = position_stack(vjust = 0.5), color = "white", size = 3)
      }
      ggsave(file, plot = p, device = "png", width = 9, height = 5, dpi = 300)
    }
  )
  
  # Descargar datos de la gráfica de pastel (totales)
  output$descargar_pastel_total <- downloadHandler(
    filename = function() { "datos_pastel_totales.csv" },
    content = function(file) { write.csv(datos_pastel() %>% select(tipo_usuario, total), file, row.names = FALSE) }
  )
  
  # Descargar gráfico de pastel (totales) en PNG
  output$descargar_grafico_pastel_total_png <- downloadHandler(
    filename = function() { 
      if (pieGraphTypeTotal() == "donut") "grafico_totales_anillos.png" else "grafico_pastel_totales.png" 
    },
    content = function(file) {
      datos <- datos_pastel()
      colores_ajustados <- ifelse(datos$tipo_usuario == "Conductor", "#fedc97", colores_paleta[match(datos$tipo_usuario, names(colores_paleta))])
      colores_ajustados[is.na(colores_ajustados)] <- colores_paleta[!names(colores_paleta) %in% c("conductor")][1:sum(is.na(colores_ajustados))]
      
      if (pieGraphTypeTotal() == "donut") {
        p <- ggplot(datos, aes(x = 2, y = total, fill = tipo_usuario)) +
          geom_bar(stat = "identity", width = 1, color = "#e9ecef") +
          coord_polar("y", start = 0) +
          geom_bar(data = datos, aes(x = 1.5, y = 0), stat = "identity", fill = "#e9ecef") +
          scale_fill_manual(values = colores_ajustados) +
          theme_void() +
          theme(plot.background = element_rect(fill = "#e9ecef"), legend.position = "none") +
          geom_text(aes(label = paste0(tipo_usuario, "\n", total)), 
                    position = position_stack(vjust = 0.5), color = "white", size = 3) +
          xlim(0.5, 2.5)
      } else {
        p <- ggplot(datos, aes(x = "", y = total, fill = tipo_usuario)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar("y", start = 0) +
          scale_fill_manual(values = colores_ajustados) +
          theme_void() +
          theme(plot.background = element_rect(fill = "#e9ecef"), legend.position = "none") +
          geom_text(aes(label = paste0(tipo_usuario, "\n", total)), 
                    position = position_stack(vjust = 0.5), color = "white", size = 3)
      }
      ggsave(file, plot = p, device = "png", width = 9, height = 5, dpi = 300)
    }
  )
  
  # Tabla de Personas Fallecidas
  output$tabla_matriz <- renderTable({
    Matriz_Personas_Fallecidas2024 %>%
      mutate(across(where(is.numeric), ~ format(., nsmall = 0)))
  }, align = "c", striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Descargar datos de la tabla de Personas Fallecidas
  output$descargar_tabla <- downloadHandler(
    filename = function() { "datos_tabla.csv" },
    content = function(file) { write.csv(Matriz_Personas_Fallecidas2024, file, row.names = FALSE) }
  )
}

shinyApp(ui = iu, server = servidor)
