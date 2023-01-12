library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(fontawesome)
# these three to be able to save plotly as png
library(webshot2)
library(pagedown)
library(curl)

investigadores <-  readr::read_rds("data/investigadores_clean.rds")
ocde <- readr::read_rds("data/ocde.rds")

count_institucion <- fct_count(investigadores$institucion_principal_fct) |> 
    mutate(rank = rank(desc(n), ties.method = "min"))

choices_reglamento <- levels(investigadores$reglamento)
choices_condicion <- levels(investigadores$condicion)
choices_nivel <- levels(investigadores$nivel)
choices_genero <- levels(investigadores$genero)

ui <- page_fluid(
    title = "RENACYT",
    class = "p-3",
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
        
    layout_column_wrap(
        width = NULL, 
        style = htmltools::css(grid_template_columns = "1fr 2fr"),
        heights_equal = "row",
        fill = FALSE,
        
        card(
            class = "border-light",
            accordion(
                accordion_item(
                    accordion_header(
                        h2("Explorador RENACYT")
                    ),
                    accordion_body("Texto de bienvenida")
                ),
                accordion_item(
                    accordion_header(
                        span(fa("fas fa-filter"), "Filtros")
                    ),
                    accordion_body(
                        pickerInput(
                            inputId = "institucion",
                            width = "100%",
                            label = "Institución principal", 
                            choices = count_institucion$f |> levels(),
                            selected = count_institucion$f |> levels(),
                            choicesOpt = list(
                                subtext = with(count_institucion, glue::glue("{n} ({rank})"))
                            ),
                            options = list(
                                `actions-box` = TRUE,
                                `deselect-all-text` = "Ninguno",
                                `select-all-text` = "Todos",
                                size = 3,
                                `live-search` = TRUE,
                                `live-search-normalize` = TRUE,
                                `live-search-placeholder` = "Nombre de institución",
                                `none-selected-text` = "Sin selección",
                                `selected-text-format` = "count > 2",
                                width = "100%"
                            ), 
                            multiple = TRUE
                        ),
                        pickerInput(
                            inputId = "reglamento", 
                            width = "100%",
                            label = "Reglamento",
                            choices = choices_reglamento,
                            selected = choices_reglamento,
                            multiple = TRUE
                        ),
                        pickerInput(
                            inputId = "condicion",
                            width = "100%",
                            label = "Condición", 
                            choices = choices_condicion,
                            selected = choices_condicion,
                            options = list(
                                `actions-box` = TRUE,
                                `live-search` = TRUE), 
                            multiple = TRUE
                        ),
                        pickerInput(
                            inputId = "nivel",
                            width = "100%",
                            label = "Nivel", 
                            choices = choices_nivel,
                            selected = choices_nivel,
                            options = list(
                                `actions-box` = TRUE,
                                size = 3,
                                `live-search` = TRUE), 
                            multiple = TRUE
                        ),
                        pickerInput(
                            inputId = "genero", 
                            width = "100%",
                            label = "Reglamento",
                            choices = choices_genero,
                            selected = choices_genero,
                            multiple = TRUE
                        ),
                    )
                ),
                accordion_item(
                    accordion_header(
                        span(fa("fas fa-pen-to-square"), "Personalizar gráficos")
                    ),
                    accordion_body(
                        color_picker(
                            "color_bars", "Color de barras", "#9ECAE1"
                        ),
                        color_picker(
                            "color_lines", "Color de líneas", '#08306B'
                        ),
                        color_picker(
                            "color_bg", "Color de fondo", '#ffffff'
                        ),
                        sliderInput("size_title", "Tamaño de título", 8, 30, 16, width = "100%")
                    )
                ),
                accordion_item(
                    accordion_header(
                        span(fa("fas fa-file-arrow-down"), "Descargar reporte")
                    ),
                    accordion_body(
                        textOutput("advertencia"),
                        downloadButton("descargar", label = "Descargar")
                    )
                ),
                accordion_item(
                    accordion_header(
                        span(fa("fas fa-circle-info"), "Info")
                    ),
                    accordion_body("inner text 2")
                )
            )
        ),
        layout_column_wrap(
            width = 1/2,
            fill = FALSE,
            plotlyOutput("plot_reglamento"),
            plotlyOutput("plot_condicion"),
            plotlyOutput("plot_nivel"),
            plotlyOutput("plot_genero"),
        )
    )
    
)

server <- function(input, output, session) {
    
    plotlyfy2 <- function(data, .var, title) {
        data |> 
            plotlyfy(
            .var = {{ .var }}, 
            title = title,
            bars_color = input$color_bars,
            lines_color = input$color_lines,
            plot_bgcolor = input$color_bg, 
            paper_bgcolor = input$color_bg,
            title_size = input$size_title
        )
    }
    
    inv_data <- reactive({
        investigadores |> 
            filter(
                institucion_principal_fct %in% input$institucion,
                reglamento %in% input$reglamento,
                condicion %in% input$condicion,
                nivel %in% input$nivel,
                genero %in% input$genero
            )
    })
    
    plot_reglamento <- reactive({
        inv_data() |> 
            plotlyfy2(
                .var = reglamento, 
                title = "Reglamento con el que se registró"
            )
    })
    
    plot_condicion <- reactive({
        inv_data() |> 
            plotlyfy2(
                .var = condicion, 
                title = "Condición del registro"
            )
    })
    
    plot_nivel <- reactive({
        inv_data() |> 
            plotlyfy2(
                .var = nivel, 
                title = "Nivel obtenido"
            )
    })
    
    plot_genero <- reactive({
        inv_data() |> 
            plotlyfy2(
                .var = genero, 
                title = "Género registrado"
            )
    })
    
    output$plot_reglamento <- renderPlotly(plot_reglamento())
    
    output$plot_condicion <- renderPlotly(plot_condicion())
    
    output$plot_nivel <- renderPlotly(plot_nivel())
    
    output$plot_genero <- renderPlotly(plot_genero())
    
    output$advertencia <- renderText({
        if (length(input$institucion) > 10) {
            "Advertencia: Seleccionar más de 20 instituciones puede generar un listado muy extenso en la sección 'Filtros'"
        }
    })

    output$descargar <- downloadHandler(
        filename = function() {
            paste0("explorador-renacyt-", Sys.time(), ".docx")
        },
        content = function(file) {
            shinyWidgets::sendSweetAlert(
                title = "Generando reporte",
                text = "Esto puede tardar un poco. Paciencia.",
                type = "info"
            )
            rmarkdown::render(
                input = "inst/reporte.Rmd",
                output_file = file,
                params = list(
                    instituciones = input$institucion,
                    reglamento = input$reglamento,
                    condicion = input$condicion,
                    nivel = input$nivel,
                    genero = input$genero,
                    fecha = Sys.Date() |> as.character(),
                    grafico_reglamento = plot_reglamento(),
                    grafico_condicion = plot_condicion(),
                    grafico_nivel = plot_nivel(),
                    grafico_genero = plot_genero()
                ))
        }
    )
    
}

shinyApp(ui, server)