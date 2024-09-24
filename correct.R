library(ojsr)
library(tidyverse)
library(googlesheets4)

gs4_deauth()
sheet_url <- "https://docs.google.com/spreadsheets/d/1Vn0MYaZwBshZLEeZCj67Jo4uWvZawI0emsd-26ru_0U/edit?usp=sharing"
data <- googlesheets4::read_sheet(sheet_url)

data2 <- data %>%
  select(input_url = `input_url`, 
         YEAR = `DC.Date.created`, 
         author = `DC.Creator.PersonalName`, 
         description = `DC.Description`, 
         title = `citation_title`, 
         keywords = `keywords`,
         institution = `citation_author_institution`) %>%
  select(inst = institution) %>%
  separate_rows(inst, sep = ",") %>%
  mutate(inst = trimws(inst)) %>%
  count(inst, sort = TRUE) 


library(shiny)
library(dplyr)
library(wordcloud2)

# Diccionario de equivalencias predefinido (input opcional)
equivalencias <- c(
  "Universidad de Buenos Aires" = "Universidad de Buenos Aires",
  "Universidad de Buenos Aires (UBA)" = "Universidad de Buenos Aires",
  "UBA" = "Universidad de Buenos Aires",
  "Facultad de Psicología- Universidad de Buenos Aires" = "Universidad de Buenos Aires",
  "University of Buenos Aires" = "Universidad de Buenos Aires",
  "Universidad de Buenos Aires (Argentina)" = "Universidad de Buenos Aires",
  "Universidad de Buenos Aires / Instituto de Investigaciones Gino Germani" = "Universidad de Buenos Aires",
  "Faculty of Psychology of the University of Buenos Aires" = "Universidad de Buenos Aires",
  "Facultad de Ciencias Sociales. Universidad de Buenos Aires. Instituto de Investigaciones Gino Germani." = "Universidad de Buenos Aires",
  "Instituto de Investigaciones Gino Germani (IIGG) — Facultad de Ciencias Sociales (FSOC) — Universidad de Buenos Aires (UBA)" = "Universidad de Buenos Aires",
  "INEBA-CONICET. Facultad de Psicología de la Universidad de Buenos Aires." = "Universidad de Buenos Aires",
  "Faculty of Psychology of the University of Buenos Aires (UBA) National Council for Scientific and Technical Research (CONICET)" = "Universidad de Buenos Aires",
  "Instituto de Investigaciones - Facultad de Psicología - Universidad de Buenos Aires" = "Universidad de Buenos Aires",
  "CONICET" = "CONICET",
  "CONICET/Facultad de Psicología" = "CONICET",
  "INEBA-CONICET." = "CONICET",
  "CONICET- UNSAM (CELES)" = "CONICET",
  "CONICET y UNIVERSIDAD NACIONAL DE CÓRDOBA" = "CONICET",
  "CONICET / Universidad de Buenos Aires" = "CONICET",
  "Universidad de Buenos Aires (UBA) / CONICET" = "CONICET",
  "Universidad de Buenos Aires - CONICET" = "CONICET",
  "Universidad de Buenos Aires - Consejo Nacional de Investigaciones Científicas y Técnicas (CONICET)" = "CONICET",
  "Consejo Nacional de Investigaciones Científicas y Técnicas (CONICET) Escuela Interdisciplinaria de Altos Estudios Sociales (EIDAES)" = "CONICET",
  "University of Flores" = "University of Flores",
  "Universidad de Flores" = "University of Flores",
  "Universidad de Flores (UFLO)" = "University of Flores",
  "USAL / Universidad de Flores UFLO" = "University of Flores",
  "Universidad Maimónides" = "Universidad Maimónides",
  "Universidad Maimonides" = "Universidad Maimónides",
  "Universidad Maimónides (UM)" = "Universidad Maimónides",
  "Chile" = "NA",
  "Perú" = "NA",
  "NA" = "NA",
  "Argentina" = "NA",
  "Valencia (España)" = "NA",
  "Escuela de Psicología" = "NA",
  "España" = "NA",
  "Facultad de Psicología" = "NA",
  "Carrera de Psicología" = "NA",
  "Carrera de Trabajo Social" = "NA",
  "Castellón (España)." = "NA",
  "Cuba" = "NA",
  "Estados Unidos" = "NA",
  "Facultad de Economía y Negocios" = "NA",
  "Facultad de Educación" = "NA",
  "Facultad de Educación y Ciencias Sociales" = "NA",
  "Facultad de Humanidades" = "NA"
)


library(shiny)
library(dplyr)

ui <- fluidPage(
  titlePanel("Normalización de Instituciones"),
  fluidRow(
    column(4,
           h4("Diccionario de Equivalencias Generado"),
           verbatimTextOutput("diccionario_output"),
           h4("Selecciona variantes para unificar"),
           selectInput("variantes", "Variantes disponibles:", choices = NULL, multiple = TRUE, selectize = TRUE),
           actionButton("confirmar", "Confirmar selección"),
           h4("Estadísticas"),
           textOutput("total_original"),
           textOutput("total_seleccionado")
    ),
    column(4,
           h4("Selecciona la variante principal"),
           uiOutput("principal_selector"),
           actionButton("aplicar", "Aplicar normalización")
    ),
    column(4,
           h4("Conteo ordenado de términos"),
           tableOutput("conteo_terminos")
    )
  )
)

server <- function(input, output, session) {
  
  # Diccionario de equivalencias predefinido (input opcional)
  diccionario_equivalencias <- reactiveVal(equivalencias)  # Inicializar con el diccionario de equivalencias
  
  variantes <- reactiveVal()
  seleccionadas <- reactiveVal(NULL)
  principal <- reactiveVal(NULL)
  data_modificada <- reactiveVal(data2)
  
  observe({
    variantes_actuales <- unique(c(unique(data_modificada()$inst), names(diccionario_equivalencias())))
    variantes(variantes_actuales)
    updateSelectInput(session, "variantes", choices = variantes())
  })
  
  output$total_original <- renderText({
    paste("Valores únicos originales:", length(unique(data2$inst)))
  })
  
  observeEvent(input$confirmar, {
    seleccionadas(input$variantes)
    output$principal_selector <- renderUI({
      selectInput("principal", "Selecciona la variante principal:", choices = seleccionadas(), selectize = TRUE)
    })
  })
  
  observeEvent(input$principal, {
    principal(input$principal)
  })
  
  observeEvent(input$aplicar, {
    req(principal(), seleccionadas())
    
    diccionario_actual <- setNames(rep(principal(), length(seleccionadas())), seleccionadas())
    
    # Actualizar el diccionario global de equivalencias
    nuevo_diccionario <- c(diccionario_equivalencias(), diccionario_actual)
    diccionario_equivalencias(nuevo_diccionario)
    
    # Normalizar los valores en la tabla
    data_modificada(data_modificada() %>% 
                      mutate(inst = ifelse(inst %in% names(diccionario_actual), diccionario_actual[inst], inst)))
    
    # Actualizar la lista de variantes únicas
    variantes_actuales <- unique(c(unique(data_modificada()$inst), names(nuevo_diccionario)))
    variantes(variantes_actuales)
    
    # Mostrar el conteo ordenado de términos
    output$conteo_terminos <- renderTable({
      as.data.frame(sort(table(data_modificada()$inst), decreasing = TRUE))
    })
    
    # Mostrar el diccionario generado en formato R
    output$diccionario_output <- renderPrint({
      cat("equivalencias <- c(\n")
      cat(paste(
        paste0('"', names(nuevo_diccionario), '" = "', nuevo_diccionario, '"'),
        collapse = ",\n"
      ))
      cat("\n)")
    })
    
    # Actualizar el total de valores únicos después de la normalización
    output$total_seleccionado <- renderText({
      paste("Valores únicos después de normalización:", length(unique(data_modificada()$inst)))
    })
  })
  
  output$total_seleccionado <- renderText({
    paste("Valores únicos después de normalización:", length(unique(data2$inst)))
  })
}

shinyApp(ui = ui, server = server)
