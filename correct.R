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
  "University of Flores" = "Universidad de Flores",
  "Universidad de Flores" = "Universidad de Flores",
  "Universidad de Flores (UFLO)" = "Universidad de Flores",
  "USAL / Universidad de Flores UFLO" = "Universidad de Flores",
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
  "Facultad de Humanidades" = "NA",
  "Universidad Católica Silva Henríquez" = "Universidad Católica Silva Henríquez",
  "Universidad Católica Silva Henríquez (UCSH)" = "Universidad Católica Silva Henríquez",
  "Universidad Católica Silva Henríquez (UCSH)
Universidad CatUniversidad Andrés Bello (UNAB)" = "Universidad Católica Silva Henríquez",
  "Barcelona" = "Universidad de Barcelona",
  "Universidad de Barcelona" = "Universidad de Barcelona",
  "Carrera de Psicología" = "Carrera de Trabajo
Social",
  "Carrera de Trabajo
Social" = "Carrera de Trabajo
Social",
  "Carrera de Trabajo Social" = "Carrera de Trabajo
Social",
  "Universidad Andrés Bello" = "Universidad Andrés Bello",
  "Universidad Andres Bello" = "Universidad Andrés Bello",
  "Universidad Andres Bello." = "Universidad Andrés Bello",
  "Universidad Católica Silva Henríquez (UCSH)
Universidad CatUniversidad Andrés Bello (UNAB)" = "Universidad Andrés Bello",
  "Belgium" = "NA",
  "Carrera de Psicología" = "NA",
  "Carrera de Trabajo
Social" = "NA",
  "Carrera de Trabajo Social" = "NA",
  "Carrera de Trabajo
Social" = "NA",
  "UBA" = "Universidad de Buenos Aires",
  "Facultad de Psicología - UBA" = "Universidad de Buenos Aires",
  "Universidad de Buenos Aires (UBA)" = "Universidad de Buenos Aires",
  "Facultad de Ciencias Sociales- UBA" = "Universidad de Buenos Aires",
  "Universidad de Buenos Aires (UBA) / CONICET" = "Universidad de Buenos Aires",
  "Instituto de Investigaciones Gino Germani (IIGG) — Facultad de Ciencias Sociales (FSOC) — Universidad de Buenos Aires (UBA)" = "Universidad de Buenos Aires",
  "Faculty of Psychology of the University of Buenos Aires (UBA)
National Council for Scientific and Technical Research (CONICET)" = "Universidad de Buenos Aires",
  "Faculty of Psychology of the University of Buenos Aires (UBA) National Council for Scientific and Technical Research (CONICET)" = "Universidad de Buenos Aires",
  "Universidad Abierta Interamericana (UAI" = "Universidad Abierta Interamericana (UAI)",
  "Universidad Abierta Interamericana (UAI)" = "Universidad Abierta Interamericana (UAI)",
  "CONICET" = "CONICET",
  "CONICET- UNSAM (CELES)" = "CONICET",
  "INEBA-CONICET. Facultad de Psicología de la Universidad de Buenos Aires." = "CONICET",
  "Universidad de Buenos Aires - Consejo Nacional de Investigaciones Científicas y Técnicas (CONICET)" = "CONICET",
  "Faculty of Psychology of the University of Buenos Aires (UBA) National Council for Scientific and Technical Research (CONICET)" = "CONICET",
  "Faculty of Psychology of the University of Buenos Aires (UBA)
National Council for Scientific and Technical Research (CONICET)" = "CONICET",
  "CONICET/Facultad de Psicología" = "CONICET",
  "CONICET / Universidad de Buenos Aires" = "CONICET",
  "CONICET y UNIVERSIDAD NACIONAL DE CÓRDOBA" = "CONICET",
  "CONICET y  UNIVERSIDAD NACIONAL DE CÓRDOBA" = "CONICET",
  "INEBA-CONICET." = "CONICET",
  "Universidad de Buenos Aires - CONICET" = "CONICET",
  "Universidad de Buenos Aires (UBA) / CONICET" = "CONICET",
  "INEBA-CONICET.
Facultad de Psicología de la Universidad de Buenos Aires." = "CONICET",
  "Consejo Nacional de Investigaciones Científicas y Técnicas (CONICET)
Escuela Interdisciplinaria de Altos Estudios Sociales (EIDAES)" = "CONICET",
  "Consejo Nacional de Investigaciones Científicas y Técnicas (CONICET) Escuela Interdisciplinaria de Altos Estudios Sociales (EIDAES)" = "CONICET",
  "Consejo Nacional de Investigaciones Científicas y Técnicas" = "CONICET",
  "Consejo Nacional de Investigaciones Científicas y Técnicas UNIPE" = "CONICET",
  "Consejo Nacional de Investigaciones Científicas y Técnicas (CONICET) Escuela Interdisciplinaria de Altos Estudios Sociales (EIDAES)" = "CONICET",
  "Consejo Nacional de Investigaciones Científicas y Técnicas (CONICET)
Escuela Interdisciplinaria de Altos Estudios Sociales (EIDAES)" = "CONICET",
  "Universidad de Buenos Aires - Consejo Nacional de Investigaciones Científicas y Técnicas (CONICET)" = "CONICET",
  "Universidad Abierta Interamericana" = "Universidad Abierta Interamericana",
  "Universidad Abierta Interamericana (UAI" = "Universidad Abierta Interamericana",
  "Universidad Abierta Interamericana (UAI)" = "Universidad Abierta Interamericana",
  "Universidad de Buenos Aires / Instituto de
Investigaciones Gino Germani" = "Universidad de Buenos Aires",
  "Universidad de Buenos Aires / Instituto de Investigaciones Gino Germani" = "Universidad de Buenos Aires",
  "Facultad de Ciencias Sociales. Universidad de Buenos Aires.
Instituto de Investigaciones Gino Germani." = "Universidad de Buenos Aires",
  "Facultad de Ciencias Sociales. Universidad de Buenos Aires. Instituto de Investigaciones Gino Germani." = "Universidad de Buenos Aires",
  "Instituto de Investigaciones Gino Germani (IIGG) — Facultad de Ciencias Sociales (FSOC) — Universidad de Buenos Aires (UBA)" = "Universidad de Buenos Aires",
  "Faculty of Psychology of the University of Buenos Aires (UBA) National Council for Scientific and Technical Research (CONICET)" = "Universidad de Buenos Aires",
  "Faculty of Psychology of the University of Buenos Aires (UBA)
National Council for Scientific and Technical Research (CONICET)" = "Universidad de Buenos Aires",
  "CONICET- UNSAM (CELES)" = "CONICET",
  "Universidad de Buenos Aires - Consejo Nacional de Investigaciones Científicas y Técnicas (CONICET)" = "CONICET",
  "INEBA-CONICET." = "CONICET",
  "INEBA-CONICET.
Facultad de Psicología de la Universidad de Buenos Aires." = "CONICET",
  "INEBA-CONICET. Facultad de Psicología de la Universidad de Buenos Aires." = "CONICET",
  "INEBA-CONICET.
Facultad de Psicología de la Universidad de Buenos Aires." = "CONICET",
  "Universidad Católica Silva Henríquez" = "Universidad Católica Silva Henríquez",
  "Universidad Católica Silva Henríquez (UCSH)" = "Universidad Católica Silva Henríquez",
  "Universidad Católica Silva Henríquez (UCSH)
Universidad CatUniversidad Andrés Bello (UNAB)" = "Universidad Católica Silva Henríquez",
  "Universidad Católica Silva Henríquez (UCSH)
Universidad CatUniversidad Andrés Bello (UNAB)" = "Universidad Católica Silva Henríquez"
)






library(shiny)
library(dplyr)

ui <- fluidPage(
  titlePanel("Normalización de Instituciones"),
  fluidRow(
    column(4,
           h4("Selecciona variantes para unificar"),
           selectInput("variantes", "Variantes disponibles:", choices = NULL, multiple = TRUE, selectize = TRUE),
           actionButton("confirmar", "Confirmar selección")
    ),
    column(4,
           h4("Selecciona la variante principal"),
           uiOutput("principal_selector"),
           actionButton("aplicar", "Aplicar normalización")
    ),
    column(4,
           h4("Estadísticas"),
           textOutput("total_original"),
           textOutput("total_seleccionado"),
           h4("Conteo ordenado de términos"),
           tableOutput("conteo_terminos")
    )
  ),
  fluidRow(
    column(12,
           h4("Diccionario de Equivalencias Generado"),
           verbatimTextOutput("diccionario_output")
    )
  )
)

server <- function(input, output, session) {
  
  diccionario_equivalencias <- reactiveVal(equivalencias)
  variantes <- reactiveVal()
  seleccionadas <- reactiveVal(NULL)
  principal <- reactiveVal(NULL)
  data_modificada <- reactiveVal(data2)
  
  observe({
    if (length(diccionario_equivalencias()) > 0) {
      data_modificada(data2 %>% 
                        mutate(inst = ifelse(inst %in% names(diccionario_equivalencias()), 
                                             diccionario_equivalencias()[inst], inst)))
    }
    
    variantes_actuales <- unique(c(unique(data_modificada()$inst), names(diccionario_equivalencias())))
    variantes(variantes_actuales)
    updateSelectInput(session, "variantes", choices = variantes())
    
    output$conteo_terminos <- renderTable({
      as.data.frame(sort(table(data_modificada()$inst), decreasing = TRUE))
    })
    
    output$total_original <- renderText({
      paste("Valores únicos originales:", length(unique(data2$inst)))
    })
    
    output$total_seleccionado <- renderText({
      paste("Valores únicos después de normalización:", length(unique(data_modificada()$inst)))
    })
  })
  
  observeEvent(input$confirmar, {
    seleccionadas(input$variantes)
    # Mostrar los valores "normalizados" como opciones de variante principal
    variantes_normalizadas <- unique(diccionario_equivalencias())  # Términos normalizados
    opciones_seleccion <- c(seleccionadas(), variantes_normalizadas)  # Añadir variantes normalizadas como opciones
    
    output$principal_selector <- renderUI({
      selectInput("principal", "Selecciona la variante principal:", choices = opciones_seleccion, selectize = TRUE)
    })
  })
  
  observeEvent(input$principal, {
    principal(input$principal)
  })
  
  observeEvent(input$aplicar, {
    req(principal(), seleccionadas())
    diccionario_actual <- setNames(rep(principal(), length(seleccionadas())), seleccionadas())
    nuevo_diccionario <- c(diccionario_equivalencias(), diccionario_actual)
    diccionario_equivalencias(nuevo_diccionario)
    data_modificada(data_modificada() %>% 
                      mutate(inst = ifelse(inst %in% names(diccionario_actual), diccionario_actual[inst], inst)))
    variantes_actuales <- unique(c(unique(data_modificada()$inst), names(nuevo_diccionario)))
    variantes(variantes_actuales)
    output$conteo_terminos <- renderTable({
      as.data.frame(sort(table(data_modificada()$inst), decreasing = TRUE))
    })
    
    output$diccionario_output <- renderPrint({
      cat("equivalencias <- c(\n")
      cat(paste(
        paste0('"', names(diccionario_equivalencias()), '" = "', diccionario_equivalencias(), '"'),
        collapse = ",\n"
      ))
      cat("\n)")
    })
    
    output$total_seleccionado <- renderText({
      paste("Valores únicos después de normalización:", length(unique(data_modificada()$inst)))
    })
  })
  
  output$diccionario_output <- renderPrint({
    if (length(diccionario_equivalencias()) > 0) {
      cat("equivalencias <- c(\n")
      cat(paste(
        paste0('"', names(diccionario_equivalencias()), '" = "', diccionario_equivalencias(), '"'),
        collapse = ",\n"
      ))
      cat("\n)")
    }
  })
}

shinyApp(ui = ui, server = server)
